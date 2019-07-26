package org.phenoscape.owl

import java.io.{File, FileOutputStream}

import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.lang3.StringUtils
import org.phenoscape.kb.ingest.util.{OBOUtil, PostCompositionParser}
import org.phenoscape.owl.Vocab._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.{AddOntologyAnnotation, IRI, OWLAxiom, OWLOntology}
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary

import scala.collection.JavaConverters._
import scala.io.Source

/**
 * This corresponds to the AVA homology model in the Phenoscape homology paper.
 */
object HomologyTableToOWLVAHM extends App {

  val factory = OWLManager.getOWLDataFactory
  val manager = OWLManager.createOWLOntologyManager
  val source = AnnotationProperty(DublinCoreVocabulary.SOURCE.getIRI)
  val description = AnnotationProperty(DublinCoreVocabulary.DESCRIPTION.getIRI)
  val InHistoricalHomologyRelationshipWith = ObjectProperty("http://purl.obolibrary.org/obo/RO_HOM0000007")
  val SeriallyHomologousTo = ObjectProperty("http://purl.obolibrary.org/obo/RO_HOM0000027")
  val HistoricalHomologyMemberof = ObjectProperty("http://example.org/historical_homology_member_of")
  val HasHistoricalHomologyMember = ObjectProperty("http://example.org/has_historical_homology_member")
  val SerialHomologyMemberOf = ObjectProperty("http://example.org/serial_homology_member_of")
  val HasSerialHomologyMember = ObjectProperty("http://example.org/has_serial_homology_member")

  val input = Source.fromFile(args(0), "utf-8")

  def convertFile(file: Source): OWLOntology = {
    val axioms = (file.getLines.drop(1).flatMap(processEntry)).toSet.asJava
    val ontology = manager.createOntology(axioms, IRI.create("http://purl.org/phenoscape/demo/phenoscape_homology.owl"))
    manager.applyChange(new AddOntologyAnnotation(ontology, factory.getOWLAnnotation(description, factory.getOWLLiteral("Homology Assertions using the AVA model"))))
    manager.addAxiom(ontology, HistoricalHomologyMemberof InverseOf HasHistoricalHomologyMember)
    manager.addAxiom(ontology, SerialHomologyMemberOf InverseOf HasSerialHomologyMember)
    manager.addAxiom(ontology, InHistoricalHomologyRelationshipWith SubPropertyChain (HistoricalHomologyMemberof o HasHistoricalHomologyMember))
    manager.addAxiom(ontology, SeriallyHomologousTo SubPropertyChain (SerialHomologyMemberOf o HasSerialHomologyMember))
    ontology
  }

  def processEntry(line: String): Set[OWLAxiom] = {
    val items = line.split("\t", -1)
    val uniqueID = DigestUtils.sha1Hex(line)
    val uniquePrefix = s"http://purl.phenoscape.org/homology/annotation/$uniqueID"
    val relation = items(4).trim
    val (upProperty, downProperty, negated) = relation match {
      case "hom to"         => (HistoricalHomologyMemberof, HasHistoricalHomologyMember, false)
      case "ser hom to"     => (SerialHomologyMemberOf, HasSerialHomologyMember, false)
      case "not hom to"     => (HistoricalHomologyMemberof, HasHistoricalHomologyMember, true)
      case "not ser hom to" => (SerialHomologyMemberOf, HasSerialHomologyMember, true)
    }
    val structure1Text = items(1).trim
    val structure1 = if (structure1Text.contains("^")) PostCompositionParser.parseExpression(structure1Text).get
    else Class(IRI.create(structure1Text))
    val taxon1 = Class(IRI.create(items(3).trim))
    val structure2Text = items(6).trim
    val structure2 = if (structure2Text.contains("^")) PostCompositionParser.parseExpression(structure2Text).get
    else Class(IRI.create(structure2Text))
    val taxon2 = Class(IRI.create(items(8).trim))
    val ancestor = Individual(s"$uniquePrefix#ancestor")
    var axioms = Set.empty[OWLAxiom]

    val maybeEvidenceID = Option(StringUtils.stripToNull(items(12)))
    if (!negated) {
      axioms += (structure1 and (in_taxon some taxon1)) SubClassOf (upProperty value ancestor)
      axioms += ancestor Type (downProperty some (structure1 and (in_taxon some taxon1)))
      axioms += (structure2 and (in_taxon some taxon2)) SubClassOf (upProperty value ancestor)
      axioms += ancestor Type (downProperty some (structure2 and (in_taxon some taxon2)))
      maybeEvidenceID.foreach { evidenceID =>
        val evidenceCode = Class(OBOUtil.iriForTermID(evidenceID))
        val evidence = Individual(s"$uniquePrefix#evidence")
        val pub = factory.getOWLLiteral(items(13).trim)
        axioms += evidence Type evidenceCode
        axioms += evidence Annotation(source, pub)
        axioms += ancestor Fact(has_evidence, evidence)
      }
    }
    axioms
  }

  val output = convertFile(input)
  manager.saveOntology(output, new FunctionalSyntaxDocumentFormat(), new FileOutputStream(new File(args(1))))

}