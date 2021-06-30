package org.phenoscape.owl

import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.lang3.StringUtils
import org.phenoscape.kb.ingest.util.{OBOUtil, PostCompositionParser}
import org.phenoscape.owl.Vocab._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.{AddOntologyAnnotation, IRI, OWLAxiom, OWLOntology}
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary

import java.io.{File, FileOutputStream}
import scala.io.Source
import scala.jdk.CollectionConverters._

/**
  * This corresponds to the REA homology model in the Phenoscape homology paper.
  */
object HomologyTableWithTaxa extends App {

  val factory = OWLManager.getOWLDataFactory
  val manager = OWLManager.createOWLOntologyManager
  val source = AnnotationProperty(DublinCoreVocabulary.SOURCE.getIRI)
  val description = AnnotationProperty(DublinCoreVocabulary.DESCRIPTION.getIRI)
  val InHistoricalHomologyRelationshipWith = ObjectProperty("http://purl.obolibrary.org/obo/RO_HOM0000007")
  val SeriallyHomologousTo = ObjectProperty("http://purl.obolibrary.org/obo/RO_HOM0000027")
  val Association = Class("http://purl.org/oban/association")
  val HasSubject = ObjectProperty("http://purl.org/oban/association_has_subject")
  val HasPredicate = ObjectProperty("http://purl.org/oban/association_has_predicate")
  val HasObject = ObjectProperty("http://purl.org/oban/association_has_object")
  val IsNegated = DataProperty("http://purl.org/phenoscape/oban/is_negated") //FIXME

  val input = Source.fromFile(args(0), "utf-8")

  def convertFile(file: Source): OWLOntology = {
    val axioms = (file.getLines.drop(1).flatMap(processEntry)).toSet.asJava
    val ontology = manager.createOntology(axioms, IRI.create("http://purl.org/phenoscape/demo/phenoscape_homology.owl"))
    manager.applyChange(
      new AddOntologyAnnotation(
        ontology,
        factory.getOWLAnnotation(description, factory.getOWLLiteral("Homology Assertions using the REA model"))
      )
    )
    ontology
  }

  def processEntry(line: String): Set[OWLAxiom] = {
    val items = line.split("\t", -1)
    val uniqueID = DigestUtils.sha1Hex(line)
    val uniquePrefix = s"http://purl.phenoscape.org/homology/annotation/$uniqueID"
    val relation = items(4).trim
    val (property, negated) = relation match {
      case "hom to"         => (InHistoricalHomologyRelationshipWith, false)
      case "ser hom to"     => (SeriallyHomologousTo, false)
      case "not hom to"     => (InHistoricalHomologyRelationshipWith, true)
      case "not ser hom to" => (SeriallyHomologousTo, true)
    }
    val structure1Text = items(1).trim
    val structure1 =
      if (structure1Text.contains("^")) PostCompositionParser.parseExpression(structure1Text).get
      else Class(IRI.create(structure1Text))
    val taxon1 = Class(IRI.create(items(3).trim))
    val structure2Text = items(6).trim
    val structure2 =
      if (structure2Text.contains("^")) PostCompositionParser.parseExpression(structure2Text).get
      else Class(IRI.create(structure2Text))
    val taxon2 = Class(IRI.create(items(8).trim))
    var axioms = Set.empty[OWLAxiom]
    Option(StringUtils.stripToNull(items(12))) match {
      case Some(evidenceID) =>
        val evidenceCode = Class(OBOUtil.iriForTermID(evidenceID))
        val evidence = Individual(s"$uniquePrefix#evidence")
        val pub = factory.getOWLLiteral(items(13).trim)
        if (!negated) {
          axioms += ((structure1 and (in_taxon some taxon1)) SubClassOf (property some (structure2 and (in_taxon some taxon2)))) Annotation (axiom_has_evidence, evidence)
          axioms += ((structure2 and (in_taxon some taxon2)) SubClassOf (property some (structure1 and (in_taxon some taxon1)))) Annotation (axiom_has_evidence, evidence)
        }
        val association = Individual(uniquePrefix)
        axioms += association Type Association
        axioms += association Fact (has_evidence, evidence)
        axioms += association Fact (IsNegated, negated)
        val structure1Ind = Individual(s"$uniquePrefix#structure1")
        val structure2Ind = Individual(s"$uniquePrefix#structure2")
        axioms += structure1Ind Type (structure1 and (in_taxon some taxon1))
        axioms += structure2Ind Type (structure2 and (in_taxon some taxon2))
        axioms += association Fact (HasSubject, structure1Ind)
        axioms += association Fact (HasObject, structure2Ind)
        axioms += association Fact (HasPredicate, Individual(property.getIRI))
        axioms += evidence Type evidenceCode
        axioms += evidence Annotation (source, pub)
      case None =>
        if (!negated) {
          axioms += ((structure1 and (in_taxon some taxon1)) SubClassOf (property some (structure2 and (in_taxon some taxon2))))
          axioms += ((structure2 and (in_taxon some taxon2)) SubClassOf (property some (structure1 and (in_taxon some taxon1))))
        }
    }
    axioms
  }

  val output = convertFile(input)
  manager.saveOntology(output, new FunctionalSyntaxDocumentFormat(), new FileOutputStream(new File(args(1))))

}
