package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConversions._
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom
import org.semanticweb.owlapi.model.OWLAxiom

object DevelopsFromRuleGenerator extends OWLTask {

  val hasPart = ObjectProperty(Vocab.HAS_PART)
  val developsFrom = ObjectProperty(Vocab.DEVELOPS_FROM)

  def generateDevelopsFromRules(ontology: OWLOntology): OWLOntology = {
    val manager = ontology.getOWLOntologyManager
    val newIRI = ontology.getOntologyID.getOntologyIRI.toString + "/develops_from_rules.owl"
    val rules = ontology.getClassesInSignature(false) map createRule
    manager.createOntology(rules.toSet[OWLAxiom], IRI.create(newIRI))
  }

  def createRule(ontClass: OWLClass): OWLSubClassOfAxiom =
    (not(hasPart some ontClass)) SubClassOf (not(hasPart some (developsFrom some ontClass)))

}