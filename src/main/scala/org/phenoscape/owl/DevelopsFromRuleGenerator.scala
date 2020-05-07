package org.phenoscape.owl

import scala.collection.JavaConverters._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom
import org.semanticweb.owlapi.model.OWLAxiom
import Vocab._

object DevelopsFromRuleGenerator extends OWLTask {

  def generateDevelopsFromRules(ontology: OWLOntology): OWLOntology = {
    val manager = ontology.getOWLOntologyManager
    val newIRI = ontology.getOntologyID.getOntologyIRI.toString + "/develops_from_rules.owl"
    val rules = ontology.getClassesInSignature(false).asScala map createRule
    manager.createOntology(rules.toSet[OWLAxiom].asJava, IRI.create(newIRI))
  }

  def createRule(ontClass: OWLClass): OWLSubClassOfAxiom =
    (not(has_part some ontClass)) SubClassOf (not(has_part some (DEVELOPS_FROM some ontClass)))

}
