package org.phenoscape.owl

import scala.collection.JavaConverters._

import org.semanticweb.owlapi.apibinding.OWLManager
import java.io.File
import org.semanticweb.owlapi.model.OWLOntology
import scala.io.Source
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.phenoscape.scowl._
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary
import org.phenoscape.kb.ingest.util.OBOUtil
import org.semanticweb.owlapi.model.AddOntologyAnnotation
import org.semanticweb.owlapi.model.AddImport
import java.util.UUID
import Vocab._

object HomologyTableToOWL extends OWLTask {

  val manager = OWLManager.createOWLOntologyManager
  val source = factory.getOWLAnnotationProperty(DublinCoreVocabulary.SOURCE.getIRI)
  val description = factory.getOWLAnnotationProperty(DublinCoreVocabulary.DESCRIPTION.getIRI)

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(args(0), "utf-8")
    val output = convertFile(input)
    manager.saveOntology(output, IRI.create(new File(args(1))))
  }

  def convertFile(file: Source): OWLOntology = {
    val axioms = (file.getLines.drop(1) flatMap processEntry).toSet.asJava
    val ontology = manager.createOntology(axioms, IRI.create("http://purl.obolibrary.org/obo/uberon/homology.owl"))
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
    if (items(4).trim == "hom to") {
      val structure1 = Class(IRI.create(items(1).trim))
      val structure2 = Class(IRI.create(items(6).trim))
      val evidenceCode = Class(OBOUtil.iriForTermID(items(10).trim))
      val evidence = Individual("http://example.org/" + UUID.randomUUID().toString)
      val pub = factory.getOWLLiteral(items(11).trim)
      Set(
        (structure1 SubClassOf (HOMOLOGOUS_TO some structure2)) Annotation (axiom_has_evidence, evidence),
        (structure2 SubClassOf (HOMOLOGOUS_TO some structure1)) Annotation (axiom_has_evidence, evidence),
        evidence Type evidenceCode,
        evidence Annotation (source, pub)
      )
    } else
      //FIXME
      // including negative homology assertions will create inconsistency
      // since the same structures are asserted both ways
      Set()
  }

}
