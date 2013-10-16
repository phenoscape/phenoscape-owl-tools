package org.phenoscape.owl

import java.io.File

import scala.collection.JavaConversions._
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom

object ReverseDevelopsFromRuleGenerator extends OWLTask {

  //FIXME rename to reverse absence rules

  val hasPart = ObjectProperty(Vocab.HAS_PART);
  val partOf = ObjectProperty(Vocab.PART_OF);
  val developsFrom = ObjectProperty(Vocab.DEVELOPS_FROM);

  def main(args: Array[String]): Unit = {
    val manager = this.getOWLOntologyManager();
    val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)));
    val developsFromOntology = generateDevelopsFromRules(ontology);
    manager.saveOntology(developsFromOntology, IRI.create(new File(args(1))));
  }

  def generateDevelopsFromRules(ontology: OWLOntology): OWLOntology = {
    val manager = ontology.getOWLOntologyManager();
    val factory = manager.getOWLDataFactory();
    val newIRI = ontology.getOntologyID().getOntologyIRI().toString() + "/reverse_develops_from_rules.owl";
    val developsFromOntology = manager.createOntology(IRI.create(newIRI));
    ontology.getClassesInSignature(false).flatMap(createRules(_)).foreach(manager.addAxiom(developsFromOntology, _));
    return developsFromOntology;
  }

  def createRules(ontClass: OWLClass): Set[OWLSubClassOfAxiom] = {
    Set(
      (hasPart some (developsFrom some ontClass)) SubClassOf (hasPart some ontClass),
      (hasPart some (partOf some ontClass)) SubClassOf (hasPart some ontClass));
  }

}