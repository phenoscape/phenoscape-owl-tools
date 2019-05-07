package org.phenoscape.owl.util

import scala.collection.JavaConverters._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom
import org.semanticweb.owlapi.model.OWLAxiom

object OntologyUtil {

  val factory = OWLManager.getOWLDataFactory

  def ontologyWithoutDisjointAxioms(ontology: OWLOntology): OWLOntology = {
    val manager = OWLManager.createOWLOntologyManager
    val axioms = filterDisjointAxioms(ontology.getAxioms().asScala.toSet)
    manager.createOntology(axioms.asJava)
  }

  def filterDisjointAxioms(axioms: Set[OWLAxiom]): Set[OWLAxiom] = axioms
    .filterNot { _.getAxiomType == AxiomType.DISJOINT_CLASSES }
    .filterNot {
      case axiom: OWLEquivalentClassesAxiom => axiom.getNamedClasses.contains(factory.getOWLNothing) || axiom.getClassExpressions.contains(factory.getOWLNothing)
      case _                                => false
    }

  def optionWithSet[T, S](in: Option[(T, Set[S])]): (Option[T], Set[S]) = in match {
    case Some((thing, set)) => (Option(thing), set)
    case None               => (None, Set.empty)
  }

  def reduceOntologyToHierarchy(ontology: OWLOntology): OWLOntology = {
    val manager = OWLManager.createOWLOntologyManager
    val factory = OWLManager.getOWLDataFactory
    val axioms = ontology.getAxioms().asScala.collect {
      case subClassOf: OWLSubClassOfAxiom if !subClassOf.getSubClass.isAnonymous && !subClassOf.getSuperClass.isAnonymous => subClassOf
      case equiv: OWLEquivalentClassesAxiom if equiv.getNamedClasses.size > 1 => factory.getOWLEquivalentClassesAxiom(equiv.getNamedClasses)
    }
    manager.createOntology(axioms.toSet[OWLAxiom].asJava)
  }

}