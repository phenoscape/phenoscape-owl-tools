package org.phenoscape.owl

import scala.annotation.migration

import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology

object NegationClassGenerator extends OWLTask {

  val negates = factory.getOWLAnnotationProperty(Vocab.NEGATES)

  def generateNegationClasses(classes: Set[OWLClass]): Set[OWLAxiom] = {
    val factory = OWLManager.getOWLDataFactory
    for {
      ontClass <- classes
      axiom <- createNegationClassAxioms(ontClass)
    } yield axiom
  }

  def createNegationClassAxioms(ontClass: OWLClass): Set[OWLAxiom] = {
    val negation = Class(getNegationIRI(ontClass.getIRI))
    Set(
      negation EquivalentTo not(ontClass),
      negation Annotation (negates, ontClass.getIRI))
  }

  def getNegationIRI(classIRI: IRI): IRI = {
    return IRI.create("http://phenoscape.org/not/" + classIRI.toString)
  }

  def getNegationOntologyIRI(ontology: OWLOntology): IRI = {
    return IRI.create("http://phenoscape.org/not/" + ontology.getOntologyID.getOntologyIRI.toString)
  }

}