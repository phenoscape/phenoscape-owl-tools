package org.phenoscape.owl

import org.phenoscape.kb.ingest.util.OBOUtil
import org.phenoscape.owl.Vocab._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary

import java.io.File
import java.util.UUID
import scala.io.Source
import scala.jdk.CollectionConverters._

object HomologyTableToOWLAsAnnotations {

  val factory = OWLManager.getOWLDataFactory
  val manager = OWLManager.createOWLOntologyManager
  val source = factory.getOWLAnnotationProperty(DublinCoreVocabulary.SOURCE.getIRI)
  val description = factory.getOWLAnnotationProperty(DublinCoreVocabulary.DESCRIPTION.getIRI)
  val aboutStructure = ObjectProperty("http://example.org/about_structure")
  val homologyAnnotation = Class("http://example.org/HomologyAnnotation")
  val negativeHomologyAnnotation = Class("http://example.org/NegativeHomologyAnnotation")

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(args(0), "utf-8")
    val output = convertFile(input)
    manager.saveOntology(output, IRI.create(new File(args(1))))
  }

  def convertFile(file: Source): OWLOntology = {
    val axioms = (file.getLines.drop(1) flatMap processEntry).toSet.asJava
    val ontology =
      manager.createOntology(axioms, IRI.create("http://purl.obolibrary.org/obo/uberon/homology_annotations.owl"))
    manager.applyChange(
      new AddOntologyAnnotation(
        ontology,
        factory.getOWLAnnotation(description, factory.getOWLLiteral("Homology Assertions"))
      )
    )
    manager.applyChange(
      new AddImport(
        ontology,
        factory.getOWLImportsDeclaration(IRI.create("http://purl.obolibrary.org/obo/uberon/ext.owl"))
      )
    )
    manager.applyChange(
      new AddImport(ontology, factory.getOWLImportsDeclaration(IRI.create("http://purl.obolibrary.org/obo/eco.owl")))
    )
    ontology
  }

  def processEntry(line: String): Set[OWLAxiom] = {
    val items = line.split("\t", -1)
    val annotation = Individual("http://example.org/" + UUID.randomUUID().toString)
    val structure1 = Individual(IRI.create(items(1).trim))
    val structure2 = Individual(IRI.create(items(6).trim))
    val evidenceCode = Class(OBOUtil.iriForTermID(items(10).trim))
    val evidence = Individual("http://example.org/" + UUID.randomUUID().toString)
    val pub = factory.getOWLLiteral(items(11).trim)
    Set(
      if (items(4).trim == "hom to")
        annotation Type homologyAnnotation
      else
        annotation Type negativeHomologyAnnotation,
      annotation Fact (aboutStructure, structure1),
      annotation Fact (aboutStructure, structure2),
      annotation Fact (has_evidence, evidence),
      evidence Type evidenceCode,
      evidence Annotation (source, pub)
    )
  }

}
