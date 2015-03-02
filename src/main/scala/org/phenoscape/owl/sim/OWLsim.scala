package org.phenoscape.owl.sim

import scala.collection.JavaConverters._
import scala.collection.mutable

import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.reasoner.{Node => ReasonerNode}

class OWLsim(ontology: OWLOntology) {

  type ICTable = Map[Node, Double]

  /**
   * parent to children
   */
  type SuperClassOfIndex = Map[Node, Set[Node]]
  type SubClassOfIndex = Map[Node, Set[Node]]

  private val scaleFactor = 1000

  private val OWLThing = OWLManager.getOWLDataFactory.getOWLThing
  private val OWLNothing = OWLManager.getOWLDataFactory.getOWLNothing

  private val (superClassOfIndex, subClassOfIndex) = nonRedundantHierarchy(ontology)

  private val allNodes: Set[Node] = superClassOfIndex.keySet

  val classToNode: Map[OWLClass, Node] = (for {
    node <- allNodes
    aClass <- node.classes
  } yield aClass -> node).toMap

  private val childToAncestorIndex = indexAncestors(classToNode(OWLNothing))

  val (directAssociationsByNode, directAssociationsByIndividual) = indexDirectAssociations(ontology)

  val directAndIndirectAssociationsByIndividual = directAssociationsByIndividual.map {
    case (individual, nodes) => individual -> (nodes ++ nodes.flatMap(childToAncestorIndex))
  }

  val corpusSize: Int = directAssociationsByNode.values.flatten.toSet.size

  val directAndIndirectAssociationsByNode: Map[Node, Set[OWLNamedIndividual]] = accumulateAssociations(classToNode(OWLThing))

  val nodeIC: Map[Node, Double] = convertFrequenciesToInformationContent(classToNode(OWLNothing))

  def nonRedundantHierarchy(ontology: OWLOntology): (SuperClassOfIndex, SubClassOfIndex) = {
    val reasoner = new ElkReasonerFactory().createReasoner(ontology)
    val parentToChildren = mutable.Map[Node, Set[Node]]()
    val childToParents = mutable.Map[Node, Set[Node]]()
    def traverse(node: ReasonerNode[OWLClass]): Unit = {
      val parent = Node(node)
      if (!parentToChildren.contains(parent)) {
        val representative = node.getRepresentativeElement
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
    reasoner.dispose()
    (parentToChildren.toMap, childToParents.toMap)
  }

  //FIXME use reasoner instead of axioms to remove redundant annotations?
  def indexDirectAssociations(ontology: OWLOntology): (Map[Node, Set[OWLNamedIndividual]], Map[OWLNamedIndividual, Set[Node]]) = {
    val classAssertions: Set[OWLClassAssertionAxiom] = ontology.getAxioms(AxiomType.CLASS_ASSERTION).asScala.toSet
    val init = (Map.empty[Node, Set[OWLNamedIndividual]], Map.empty[OWLNamedIndividual, Set[Node]])
    classAssertions.foldLeft(init) {
      case ((byNode, byIndividual), assertion) =>
        (assertion.getIndividual, assertion.getClassExpression) match {
          case (namedInd: OWLNamedIndividual, namedClass: OWLClass) => {
            val node = classToNode(namedClass)
            val currentByNode = byNode.getOrElse(node, Set.empty)
            val currentByIndividual = byIndividual.getOrElse(namedInd, Set.empty)
            (byNode + (node -> (currentByNode + namedInd)),
              byIndividual + (namedInd -> (currentByIndividual + node)))
          }
          case _ => (byNode, byIndividual)
        }
    }
  }

  private def accumulateAssociations(node: Node): Map[Node, Set[OWLNamedIndividual]] = {
    val index = mutable.Map[Node, Set[OWLNamedIndividual]]()
    def traverse(node: Node): Unit = {
      if (!index.contains(node)) {
        val children = superClassOfIndex(node)
        children.foreach(traverse)
        val nodeAssociations = directAssociationsByNode.getOrElse(node, Set.empty) ++ children.flatMap(index)
        index += (node -> nodeAssociations)
      }
    }
    traverse(node)
    index.toMap
  }

  private def indexAncestors(bottom: Node): Map[Node, Set[Node]] = {
    val index = mutable.Map[Node, Set[Node]]()
    def traverse(node: Node): Unit = {
      if (!index.contains(node)) {
        val parents = subClassOfIndex(node)
        parents.foreach(traverse)
        val ancestors = parents ++ parents.flatMap(index)
        index += (node -> ancestors)
      }
    }
    traverse(bottom)
    index.toMap
  }

  def convertFrequenciesToInformationContent(bottom: Node): Map[Node, Double] = {
    val ics = mutable.Map[Node, Double]()
    def traverse(node: Node): Unit = {
      if (!ics.contains(node)) {
        val parents = subClassOfIndex(node)
        parents.foreach(traverse)
        val freq = directAndIndirectAssociationsByNode(node).size
        val ic = if (freq == 0) {
          parents.map(ics).max
        } else {
          val basicIC = -Math.log((freq.toDouble / corpusSize)) / Math.log(2)
          val ancestorCount = childToAncestorIndex(node).size
          val bump = ancestorCount / scaleFactor.toDouble
          basicIC + bump
        }
        ics += (node -> ic)
      }
    }
    traverse(bottom)
    ics.toMap
  }

  def commonSubsumersOf(i: OWLNamedIndividual, j: OWLNamedIndividual): Set[Node] =
    directAndIndirectAssociationsByIndividual(i).intersect(directAndIndirectAssociationsByIndividual(j))

  def groupWiseSimilarity(i: OWLNamedIndividual, j: OWLNamedIndividual) = {
    //median of max IC from all pairs
    
  }

}

case class Node(classes: Set[OWLClass])

object Node {

  def apply(reasonerNode: ReasonerNode[OWLClass]): Node = Node(reasonerNode.getEntities.asScala.toSet)

}
