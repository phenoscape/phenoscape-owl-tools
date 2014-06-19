package org.phenoscape.owl.util

import scala.collection.JavaConversions._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLClassAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom

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

}