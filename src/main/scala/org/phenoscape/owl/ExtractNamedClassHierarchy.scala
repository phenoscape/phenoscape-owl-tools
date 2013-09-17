package org.phenoscape.owl

import java.io.File
import java.util.UUID

import scala.collection.JavaConversions._

import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLOntology

object ExtractNamedClassHierarchy extends OWLTask {

  def main(args: Array[String]): Unit = {
    val manager = this.getOWLOntologyManager();
    val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)));
    val hierarchy = extractHierarchy(ontology);
    manager.saveOntology(hierarchy, IRI.create(new File(args(2))));
  }

  def extractHierarchy(ontology: OWLOntology): OWLOntology = {
    val manager = ontology.getOWLOntologyManager();
    val namedClassAxioms = ontology.getAxioms(AxiomType.SUBCLASS_OF, true).filter(axiom => ((!axiom.getSubClass().isAnonymous()) && (!axiom.getSuperClass().isAnonymous())));
    val hierarchyOntology = manager.createOntology(IRI.create("http://example.org/" + UUID.randomUUID().toString()));
    manager.addAxioms(hierarchyOntology, namedClassAxioms);
    return hierarchyOntology;
  }

}