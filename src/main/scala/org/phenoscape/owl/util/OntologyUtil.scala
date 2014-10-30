package org.phenoscape.owl.util

import scala.collection.JavaConversions._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLClassAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import java.util.UUID
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.IRI

object OntologyUtil {

  val factory = OWLManager.getOWLDataFactory

  def ontologyWithoutDisjointAxioms(ontology: OWLOntology): OWLOntology = {
    val manager = OWLManager.createOWLOntologyManager
    val axioms = ontology.getAxioms
      .filterNot { _.getAxiomType == AxiomType.DISJOINT_CLASSES }
      .filterNot {
        case axiom: OWLEquivalentClassesAxiom => axiom.getNamedClasses.contains(factory.getOWLNothing) || axiom.getClassExpressions.contains(factory.getOWLNothing)
        case _ => false
      }
    manager.createOntology(axioms)
  }

  def nextIndividual(): OWLNamedIndividual = factory.getOWLNamedIndividual(this.nextIRI)

  def nextClass(): OWLClass = factory.getOWLClass(this.nextIRI)

  def nextIRI(): IRI = {
    val uuid = UUID.randomUUID.toString
    val id = "http://purl.org/phenoscape/uuid/" + uuid
    IRI.create(id)
  }

  def optionWithSet[T, S](in: Option[(T, Set[S])]): (Option[T], Set[S]) = in match {
    case Some((thing, set)) => (Option(thing), set)
    case None => (None, Set.empty)
  }

}