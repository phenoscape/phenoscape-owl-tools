package org.phenoscape.owl

import org.phenoscape.owl.Vocab._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports

import scala.collection.JavaConverters._

object DevelopsFromRuleGenerator {

  def generateDevelopsFromRules(ontology: OWLOntology): OWLOntology = {
    val manager = ontology.getOWLOntologyManager
    val newIRI = ontology.getOntologyID.getOntologyIRI.toString + "/develops_from_rules.owl"
    val rules = ontology.getClassesInSignature(Imports.EXCLUDED).asScala map createRule
    manager.createOntology(rules.toSet[OWLAxiom].asJava, IRI.create(newIRI))
  }

  def createRule(ontClass: OWLClass): OWLSubClassOfAxiom =
    (not(has_part some ontClass)) SubClassOf (not(has_part some (DEVELOPS_FROM some ontClass)))

}
