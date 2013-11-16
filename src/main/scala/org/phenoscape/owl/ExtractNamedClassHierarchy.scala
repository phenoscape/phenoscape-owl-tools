package org.phenoscape.owl

import java.io.File
import java.util.UUID
import scala.collection.JavaConversions._
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLAxiom

object ExtractNamedClassHierarchy extends OWLTask {

  val manager = this.getOWLOntologyManager

  def main(args: Array[String]): Unit = {
    val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)))
    val hierarchy = extractHierarchy(ontology)
    manager.saveOntology(hierarchy, IRI.create(new File(args(2))))
  }

  def extractHierarchy(ontology: OWLOntology): OWLOntology = {
    val namedClassAxioms = for {
      axiom <- ontology.getAxioms(AxiomType.SUBCLASS_OF, true)
      if !axiom.getSubClass().isAnonymous()
      if !axiom.getSuperClass().isAnonymous()
    } yield axiom
    manager.createOntology(namedClassAxioms.toSet[OWLAxiom], IRI.create("http://example.org/" + UUID.randomUUID().toString()))
  }

}