package org.phenoscape.owl.sim

import monix.eval.Task
import monix.reactive.Observable
import org.apache.jena.datatypes.TypeMapper
import org.apache.jena.rdf.model.impl.ResourceImpl
import org.apache.jena.rdf.model.{AnonId, ResourceFactory, Statement => JenaStatement}
import org.apache.jena.riot.RDFFormat
import org.apache.jena.riot.system.StreamRDFWriter
import org.openrdf.model.impl.{NumericLiteralImpl, StatementImpl, URIImpl}
import org.openrdf.model.vocabulary.RDF
import org.openrdf.model.{BNode, Literal, Statement, URI}
import org.phenoscape.kb.ingest.util.OntUtil
import org.phenoscape.owl.Vocab
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{OWLClass, OWLNamedIndividual, OWLOntology}
import org.semanticweb.owlapi.reasoner.{OWLReasoner, Node => ReasonerNode}

import java.io.{File, FileOutputStream, PrintWriter}
import scala.collection.mutable
import scala.collection.parallel._
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._
import scala.util.hashing.MurmurHash3

class OWLsim(ontology: OWLOntology, inCorpus: OWLNamedIndividual => Boolean) {

  type SuperClassOfIndex = Map[Node, Set[Node]]
  type SubClassOfIndex = Map[Node, Set[Node]]

  private val OWLThing = OWLManager.getOWLDataFactory.getOWLThing
  private val OWLNothing = OWLManager.getOWLDataFactory.getOWLNothing

  val (superClassOfIndex, subClassOfIndex, directAssociationsByNode, directAssociationsByIndividual) = {
    val reasoner = new ElkReasonerFactory().createReasoner(ontology)
    val (superClassOf, subClassOf) = nonRedundantHierarchy(reasoner)
    val (directAssocByNode, directAssocByIndividual) = indexDirectAssociations(reasoner)
    reasoner.dispose()
    (superClassOf, subClassOf, directAssocByNode, directAssocByIndividual)
  }

  val allNodes: Set[Node] = superClassOfIndex.keySet

  val classToNode: Map[OWLClass, Node] = (for {
    node <- allNodes
    aClass <- node.classes
  } yield aClass -> node).toMap

  val childToReflexiveAncestorIndex: Map[Node, Set[Node]] = indexAncestorsReflexive(classToNode(OWLNothing))

  val allIndividuals: Set[OWLNamedIndividual] = directAssociationsByIndividual.keySet

  val individualsInCorpus: Set[OWLNamedIndividual] = directAssociationsByIndividual.keySet.filter(inCorpus)

  val directAndIndirectAssociationsByIndividual: Map[OWLNamedIndividual, Set[Node]] =
    directAssociationsByIndividual.map {
      case (individual, nodes) => individual -> (nodes ++ nodes.flatMap(childToReflexiveAncestorIndex))
    }

  private val rdfType = new URIImpl(Vocab.rdfType.toString)
  private val rdfsSubClassOf = new URIImpl(Vocab.rdfsSubClassOf.toString)

  def directAndIndirectAssociationsByIndividualToTriples: Set[Statement] =
    for {
      (individual, nodes) <- directAndIndirectAssociationsByIndividual.toSet
      node <- nodes
      term <- node.classes
    } yield new StatementImpl(new URIImpl(individual.getIRI.toString), rdfType, new URIImpl(term.getIRI.toString))

  def childToReflexiveAncestorIndexToTriples: Set[Statement] =
    for {
      (node, nodes) <- childToReflexiveAncestorIndex.toSet
      term <- node.classes
      ancestorNode <- nodes
      ancestor <- ancestorNode.classes
    } yield new StatementImpl(new URIImpl(term.getIRI.toString), rdfsSubClassOf, new URIImpl(ancestor.getIRI.toString))

  val corpusSize: Int = individualsInCorpus.size

  private def uncorrectedIC(numInstances: Int): Double = -Math.log(numInstances.toDouble / corpusSize) / Math.log(2)

  val MaximumIC: Double = uncorrectedIC(1)

  def normalizedIC(numInstances: Int): Double = uncorrectedIC(numInstances) / MaximumIC

  val directAndIndirectAssociationsByNode: Map[Node, Set[OWLNamedIndividual]] = accumulateAssociations(
    classToNode(OWLThing)
  )

  val nodeIC: Map[Node, Double] = convertFrequenciesToInformationContent(classToNode(OWLNothing))

  def computeAllSimilarityToCorpus(inputs: Set[OWLNamedIndividual]): Set[Statement] =
    (for {
      inputProfile <- inputs.toParArray
      corpusProfile <- individualsInCorpus.toParArray
      (_, triples) = groupWiseSimilarity(inputProfile, corpusProfile).toTriples
      triple <- triples
    } yield triple).toSet.seq

  def computeAllSimilarityToCorpusDirectOutput(
    inputs: Set[OWLNamedIndividual],
    triplesOutfile: File,
    tsvOutfile: File
  ): Unit = {
    import monix.execution.Scheduler.Implicits.global
    val tsvWriter = new PrintWriter(tsvOutfile, "utf-8")
    tsvWriter.println("?match\t?score\t?query\t?corpusprofile")
    val outputStream = new FileOutputStream(triplesOutfile)
    val rdfWriter = StreamRDFWriter.getWriterStream(outputStream, RDFFormat.TURTLE_FLAT.getLang)
    rdfWriter.start()
    val comparisons = for {
      inputProfile <- Observable.fromIterable(inputs)
      corpusProfile <- Observable.fromIterable(individualsInCorpus)
    } yield (inputProfile, corpusProfile)

    val processed = comparisons.mapParallelUnordered(Runtime.getRuntime.availableProcessors) {
      case (inputProfile, corpusProfile) =>
        Task {
          val similarity = groupWiseSimilarity(inputProfile, corpusProfile)
          val (comparison, triples) = similarity.toTriples
          val tsvLine =
            s"$comparison\t${similarity.score}\t${similarity.queryIndividual.getIRI}\t${similarity.corpusIndividual.getIRI}"
          (tsvLine, triples)
        }
    }
    processed
      .foreachL {
        case (tsvLine, triples) =>
          //FIXME wasted conversions here
          triples.foreach(triple => rdfWriter.triple(sesameTripleToJena(triple).asTriple))
          tsvWriter.println(tsvLine)
      }
      .runSyncUnsafe(Duration.Inf)
    rdfWriter.finish()
    outputStream.close()
    tsvWriter.close()
  }

  def computeAllSimilarityToCorpusJ(
    inputs: Set[OWLNamedIndividual]
  ): Map[(OWLNamedIndividual, OWLNamedIndividual), Double] =
    (for {
      inputProfile <- inputs.toParArray
      corpusProfile <- individualsInCorpus.toParArray
      score = groupWiseSimilarityJaccard(inputProfile, corpusProfile)
    } yield (inputProfile, corpusProfile) -> score).toMap.seq

  def computeAllSimilarityToCorpusJDirectOutput(inputs: Set[OWLNamedIndividual], output: File): Unit = {
    val pw = new PrintWriter(output)
    for {
      inputProfile <- inputs.toParArray
      corpusProfile <- individualsInCorpus.toParArray
      score = groupWiseSimilarityJaccard(inputProfile, corpusProfile)
    } this.synchronized {
      pw.println(s"${inputProfile.getIRI.toString}\t${corpusProfile.getIRI.toString}\t$score")
    }
    pw.close()
  }

  def computePairwiseSimilarityInCorpusJDirectOutput(output: File): Unit = {
    val pw = new PrintWriter(output)
    for {
      inputProfile <- individualsInCorpus.toParArray
      corpusProfile <- individualsInCorpus.toParArray
      if inputProfile.getIRI.toString < corpusProfile.getIRI.toString
      score = groupWiseSimilarityJaccard(inputProfile, corpusProfile)
    } this.synchronized {
      pw.println(s"${inputProfile.getIRI.toString}\t${corpusProfile.getIRI.toString}\t$score")
    }
    pw.close()
  }

  def nonRedundantHierarchy(reasoner: OWLReasoner): (SuperClassOfIndex, SubClassOfIndex) = {
    val parentToChildren = mutable.Map[Node, Set[Node]]()
    val childToParents = mutable.Map[Node, Set[Node]]()

    def traverse(reasonerNode: ReasonerNode[OWLClass]): Unit = {
      val parent = Node(reasonerNode)
      if (!parentToChildren.contains(parent)) {
        val representative = reasonerNode.getRepresentativeElement
        val children = reasoner.getSubClasses(representative, true).getNodes.asScala.toSet
        children.foreach { childNode =>
          traverse(childNode)
          val child = Node(childNode)
          val parents = childToParents.getOrElse(child, Set.empty)
          childToParents += (child -> (parents + parent))
        }
        parentToChildren += (parent -> children.map(Node(_)))
      }
    }

    val top = reasoner.getTopClassNode
    traverse(top)
    childToParents += (Node(top) -> Set.empty)
    (parentToChildren.toMap, childToParents.toMap)
  }

  def indexDirectAssociations(
    reasoner: OWLReasoner
  ): (Map[Node, Set[OWLNamedIndividual]], Map[OWLNamedIndividual, Set[Node]]) = {
    val individuals = reasoner.getRootOntology.getIndividualsInSignature(Imports.INCLUDED).asScala.toSet
    val init = (Map.empty[Node, Set[OWLNamedIndividual]], Map.empty[OWLNamedIndividual, Set[Node]])
    val individualsToNodes = individuals.map { individual =>
      val nodes = reasoner.getTypes(individual, true).getNodes.asScala.map(Node(_)).toSet
      individual -> nodes
    }.toMap
    (invertMapOfSets(individualsToNodes), individualsToNodes)
  }

  private def accumulateAssociations(node: Node): Map[Node, Set[OWLNamedIndividual]] = {
    val index = mutable.Map[Node, Set[OWLNamedIndividual]]()

    def traverse(node: Node): Unit =
      if (!index.contains(node)) {
        val children = superClassOfIndex(node)
        children.foreach(traverse)
        val nodeAssociations = directAssociationsByNode.getOrElse(node, Set.empty) ++ children.flatMap(index)
        index += (node -> nodeAssociations)
      }

    traverse(node)
    index.toMap
  }

  private def indexAncestorsReflexive(bottom: Node): Map[Node, Set[Node]] = {
    val index = mutable.Map[Node, Set[Node]]()

    def traverse(node: Node): Unit =
      if (!index.contains(node)) {
        val parents = subClassOfIndex(node)
        parents.foreach(traverse)
        val ancestors = parents ++ parents.flatMap(index)
        index += (node -> (ancestors + node))
      }

    traverse(bottom)
    index.toMap
  }

  private def convertFrequenciesToInformationContent(bottom: Node): Map[Node, Double] = {
    val ics = mutable.Map[Node, Double]()

    def traverse(node: Node): Unit =
      if (!ics.contains(node)) {
        val parents = subClassOfIndex(node)
        parents.foreach(traverse)
        val instancesInCorpus = directAndIndirectAssociationsByNode(node).intersect(individualsInCorpus)
        val freq = instancesInCorpus.size
        val ic =
          if (freq == 0)
            if (parents.isEmpty) 1
            else parents.map(ics).max
          else
            normalizedIC(freq)
        ics += (node -> ic)
      }

    traverse(bottom)
    ics.toMap
  }

  def commonSubsumersOf(i: Node, j: Node): Set[Node] =
    childToReflexiveAncestorIndex(i).intersect(childToReflexiveAncestorIndex(j))

  def commonSubsumersOf(i: OWLNamedIndividual, j: OWLNamedIndividual): Set[Node] =
    directAndIndirectAssociationsByIndividual(i).intersect(directAndIndirectAssociationsByIndividual(j))

  def maxICSubsumer(i: Node, j: Node): Node = if (i == j) i else commonSubsumersOf(i, j).maxBy(nodeIC)

  def groupWiseSimilarity(
    queryIndividual: OWLNamedIndividual,
    corpusIndividual: OWLNamedIndividual
  ): GroupWiseSimilarity = {
    val directAssociationsByCorpusIndividual = directAssociationsByIndividual(corpusIndividual)
    val pairScores = for {
      queryAnnotation <- directAssociationsByIndividual(queryIndividual)
    } yield directAssociationsByCorpusIndividual
      .map { corpusAnnotation =>
        val maxSubsumer = maxICSubsumer(queryAnnotation, corpusAnnotation)
        PairScore(queryAnnotation, corpusAnnotation, maxSubsumer, nodeIC(maxSubsumer))
      }
      .maxBy(_.maxSubsumerIC)
    val medianScore = median(pairScores.map(_.maxSubsumerIC).toSeq)
    GroupWiseSimilarity(queryIndividual, corpusIndividual, medianScore, pairScores)
  }

  def groupWiseSimilarityJaccard(queryIndividual: OWLNamedIndividual, corpusIndividual: OWLNamedIndividual): Double = {
    val queryTypes = directAndIndirectAssociationsByIndividual(queryIndividual)
    val corpusTypes = directAndIndirectAssociationsByIndividual(corpusIndividual)
    queryTypes.intersect(corpusTypes).size.toDouble / queryTypes.union(corpusTypes).size
  }

  def similarityProfileJaccard(queryIndividual: OWLNamedIndividual, corpusIndividual: OWLNamedIndividual): Set[Node] = {
    val queryTypes = directAndIndirectAssociationsByIndividual(queryIndividual)
    val corpusTypes = directAndIndirectAssociationsByIndividual(corpusIndividual)
    val common = queryTypes.intersect(corpusTypes)
    val superClassesOfIntersection = common.flatMap(subClassOfIndex)
    common -- superClassesOfIntersection
  }

  private def median(values: Seq[Double]): Double = {
    val (lower, upper) = values.sorted.splitAt(values.size / 2)
    if (values.size % 2 == 0) (lower.last + upper.head) / 2.0 else upper.head
  }

  private def invertMapOfSets[K, V](in: Map[K, Set[V]]): Map[V, Set[K]] =
    in.flatMap { case (k, vs) => vs.map(_ -> k) }
      .groupBy { case (v, k) => v }
      .map {
        case (v, vks) =>
          v -> vks.map {
            case (v1, k1) => k1
          }.toSet
      }

  def classICScoresAsTriples: Set[Statement] = {
    val has_ic = new URIImpl(Vocab.has_ic.getIRI.toString)
    (for {
      (node, ic) <- nodeIC
      term <- node.classes
    } yield {
      val termURL = new URIImpl(term.getIRI.toString)
      new StatementImpl(termURL, has_ic, new NumericLiteralImpl(ic))
    }).toSet
  }

  private def sesameTripleToJena(triple: Statement): JenaStatement = {
    val subject = triple.getSubject match {
      case bnode: BNode => new ResourceImpl(new AnonId(bnode.getID))
      case uri: URI     => ResourceFactory.createResource(uri.stringValue)
    }
    val predicate = ResourceFactory.createProperty(triple.getPredicate.stringValue)
    val obj = triple.getObject match {
      case bnode: BNode => new ResourceImpl(new AnonId(bnode.getID))
      case uri: URI     => ResourceFactory.createResource(uri.stringValue)
      case literal: Literal if literal.getLanguage != null =>
        ResourceFactory.createLangLiteral(literal.getLabel, literal.getLanguage)
      case literal: Literal if literal.getDatatype != null =>
        ResourceFactory.createTypedLiteral(
          literal.getLabel,
          TypeMapper.getInstance.getSafeTypeByName(literal.getDatatype.stringValue)
        )
      case literal: Literal => ResourceFactory.createStringLiteral(literal.getLabel)
    }
    ResourceFactory.createStatement(subject, predicate, obj)
  }

}

final case class Node(classes: Set[OWLClass]) {

  override val hashCode: Int = MurmurHash3.productHash(this)

}

object Node {

  def apply(reasonerNode: ReasonerNode[OWLClass]): Node = Node(reasonerNode.getEntities.asScala.toSet)

}

final case class PairScore(queryAnnotation: Node, corpusAnnotation: Node, maxSubsumer: Node, maxSubsumerIC: Double)

final case class GroupWiseSimilarity(
  queryIndividual: OWLNamedIndividual,
  corpusIndividual: OWLNamedIndividual,
  score: Double,
  pairs: Set[PairScore]
) {

  import GroupWiseSimilarity._

  def toTriples: (URI, Set[Statement]) = {
    val self = new URIImpl(OntUtil.nextIRI.toString)
    val micasTriples = for {
      pair <- pairs
      subsumer <- pair.maxSubsumer.classes
    } yield new StatementImpl(new URIImpl(subsumer.getIRI.toString), RDF.TYPE, FoundAsMICA)
    val bestPairComparisons = pairs.toSeq.sortBy(_.maxSubsumerIC).takeRight(20).filter(_.maxSubsumerIC > 0)
    val distinctSubsumers: Set[Node] = bestPairComparisons.map(_.maxSubsumer).toSet
    val subsumerTriples = for {
      node <- distinctSubsumers
      term <- node.classes
    } yield new StatementImpl(self, has_subsumer, new URIImpl(term.getIRI.toString))
    val triples = Set(
      new StatementImpl(self, combined_score, new NumericLiteralImpl(score)),
      new StatementImpl(self, for_query_profile, new URIImpl(queryIndividual.getIRI.toString)),
      new StatementImpl(self, for_corpus_profile, new URIImpl(corpusIndividual.getIRI.toString))
    ) ++ subsumerTriples ++ micasTriples
    self -> triples.toSet
  }

}

object GroupWiseSimilarity {

  val combined_score = new URIImpl(Vocab.combined_score.getIRI.toString)
  val has_subsumer = new URIImpl(Vocab.has_subsumer.getIRI.toString)
  val for_query_profile = new URIImpl(Vocab.for_query_profile.getIRI.toString)
  val for_corpus_profile = new URIImpl(Vocab.for_corpus_profile.getIRI.toString)
  val FoundAsMICA = new URIImpl(Vocab.FoundAsMICA.getIRI.toString)

}

final case class SimpleSimilarity(i: OWLNamedIndividual, j: OWLNamedIndividual, score: Double) {

  override def toString = s"${i.getIRI.toString}\t${j.getIRI.toString}\t$score"

}
