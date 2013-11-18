package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConversions._
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLAxiom

object NegationClassGenerator extends OWLTask {

  val negates = OWLManager.getOWLDataFactory().getOWLAnnotationProperty(Vocab.NEGATES)

  def main(args: Array[String]): Unit = {
    val manager = this.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)))
    val negationsOntology = generateNegationClasses(ontology)
    manager.saveOntology(negationsOntology, IRI.create(new File(args(1))))
  }

  def generateNegationClasses(ontology: OWLOntology): OWLOntology = {
    val manager = ontology.getOWLOntologyManager()
    val factory = manager.getOWLDataFactory()
    val newIRI = getNegationOntologyIRI(ontology)
    val newAxioms = for {
      ontClass <- ontology.getClassesInSignature(false)
      axiom <- createNegationClassAxioms(ontClass, ontology)
    } yield axiom
    manager.createOntology(newAxioms, newIRI)
  }

  def createNegationClassAxioms(ontClass: OWLClass, ontology: OWLOntology): Set[OWLAxiom] = {
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