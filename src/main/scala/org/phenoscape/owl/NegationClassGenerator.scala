package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConversions._
import scala.collection.Set
import scala.collection.mutable
import org.nescent.strix.OWL._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLAxiom

object NegationClassGenerator extends OWLTask {

  val negates = OWLManager.getOWLDataFactory().getOWLAnnotationProperty(Vocab.NEGATES);

  def main(args: Array[String]): Unit = {
    val manager = this.getOWLOntologyManager();
    val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)));
    val negationsOntology = generateNegationClasses(ontology);
    manager.saveOntology(negationsOntology, IRI.create(new File(args(1))));
  }

  def generateNegationClasses(ontology: OWLOntology): OWLOntology = {
    val manager = ontology.getOWLOntologyManager();
    val factory = manager.getOWLDataFactory();
    val newIRI = getNegationOntologyIRI(ontology);
    val negationsOntology = manager.createOntology(newIRI);
    ontology.getClassesInSignature(false).map(createNegationClassAxioms(_, ontology)).foreach(manager.addAxioms(negationsOntology, _));
    return negationsOntology;
  }

  def createNegationClassAxioms(ontClass: OWLClass, ontology: OWLOntology): Set[OWLAxiom] = {
    val axioms = mutable.Set[OWLAxiom]();
    val factory = OWLManager.getOWLDataFactory();
    val negation = factory.getOWLClass(getNegationIRI(ontClass.getIRI()));
    val desc = not(ontClass);
    axioms.add(factory.getOWLEquivalentClassesAxiom(negation, desc));
    val anonymousEquivalents = ontClass.getEquivalentClasses(ontology).filter(_.isAnonymous());
    anonymousEquivalents.map(equivClass => factory.getOWLEquivalentClassesAxiom(negation, not(equivClass)));
    axioms.add(factory.getOWLAnnotationAssertionAxiom(negates, negation.getIRI(), ontClass.getIRI()));
    return axioms;
  }

  def getNegationIRI(classIRI: IRI): IRI = {
    return IRI.create("http://phenoscape.org/not/" + classIRI.toString());
  }

  def getNegationOntologyIRI(ontology: OWLOntology): IRI = {
    return IRI.create("http://phenoscape.org/not/" + ontology.getOntologyID().getOntologyIRI().toString());
  }

}