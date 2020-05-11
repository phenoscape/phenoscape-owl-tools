package org.phenoscape.owl

import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLClass, OWLOntology}

object NegationClassGenerator {

  val factory = OWLManager.getOWLDataFactory
  val negates = factory.getOWLAnnotationProperty(Vocab.NEGATES)

  def generateNegationClasses(classes: Set[OWLClass]): Set[OWLAxiom] =
    for {
      ontClass <- classes
      axiom    <- createNegationClassAxioms(ontClass)
    } yield axiom

  def createNegationClassAxioms(ontClass: OWLClass): Set[OWLAxiom] = {
    val negation = Class(getNegationIRI(ontClass.getIRI))
    Set(negation EquivalentTo not(ontClass), negation Annotation (negates, ontClass.getIRI))
  }

  def getNegationIRI(classIRI: IRI): IRI = IRI.create("http://phenoscape.org/not/" + classIRI.toString)

  def getNegationOntologyIRI(ontology: OWLOntology): IRI =
    IRI.create("http://phenoscape.org/not/" + ontology.getOntologyID.getOntologyIRI.toString)

}
