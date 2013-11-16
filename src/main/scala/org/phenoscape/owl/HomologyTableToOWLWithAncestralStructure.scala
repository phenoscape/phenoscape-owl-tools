package org.phenoscape.owl

import java.io.File
import java.util.UUID

import scala.collection.JavaConversions._
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.io.Source

import org.phenoscape.scowl.OWL._
import org.phenoscape.owl.util.OBOUtil
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.AddOntologyAnnotation
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary

object HomologyTableToOWLWithAncestralStructure extends OWLTask {

  val manager = this.getOWLOntologyManager
  val derivedByDescentFrom = ObjectProperty(Vocab.DERIVED_BY_DESCENT_FROM)
  val hasDerivedByDescendant = ObjectProperty(Vocab.HAS_DERIVED_BY_DESCENDANT)
  val hasEvidence = ObjectProperty(Vocab.EVIDENCE)
  val source = factory.getOWLAnnotationProperty(DublinCoreVocabulary.SOURCE.getIRI)
  val description = factory.getOWLAnnotationProperty(DublinCoreVocabulary.DESCRIPTION.getIRI)
  val aboutStructure = ObjectProperty("http://example.org/about_structure")

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(args(0), "utf-8")
    val output = convertFile(input)
    manager.saveOntology(output, IRI.create(new File(args(1))))
  }

  def convertFile(file: Source): OWLOntology = {
    val axioms = (file.getLines.drop(1) flatMap processEntry).toSet
    val ontology = manager.createOntology(axioms, IRI.create("http://purl.obolibrary.org/obo/uberon/homology_with_ancestors.owl"))
    manager.applyChange(new AddOntologyAnnotation(ontology, factory.getOWLAnnotation(description, factory.getOWLLiteral("Homology Assertions"))))
    manager.applyChange(new AddImport(ontology, factory.getOWLImportsDeclaration(IRI.create("http://purl.obolibrary.org/obo/uberon/ext.owl"))))
    manager.applyChange(new AddImport(ontology, factory.getOWLImportsDeclaration(IRI.create("http://purl.obolibrary.org/obo/eco.owl"))))
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
      val ancestralStructure = Individual("http://example.org/" + UUID.randomUUID().toString)
      Set(
        ancestralStructure Fact (hasEvidence, evidence),
        structure1 SubClassOf (derivedByDescentFrom value ancestralStructure),
        structure2 SubClassOf (derivedByDescentFrom value ancestralStructure),
        ancestralStructure Type (hasDerivedByDescendant some structure1),
        ancestralStructure Type (hasDerivedByDescendant some structure2),
        evidence Type evidenceCode,
        evidence Annotation (source, pub))
    } else {
      //FIXME
      // not including negative homology assertions
      Set()
    }
  }

}