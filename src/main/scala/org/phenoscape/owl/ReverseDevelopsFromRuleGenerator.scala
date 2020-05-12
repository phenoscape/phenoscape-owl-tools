package org.phenoscape.owl

import java.io.File

import org.phenoscape.owl.Vocab._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports

import scala.collection.JavaConverters._

object ReverseDevelopsFromRuleGenerator {

  //FIXME rename to reverse absence rules

  def main(args: Array[String]): Unit = {
    val manager              = OWLManager.createOWLOntologyManager
    val ontology             = manager.loadOntologyFromOntologyDocument(new File(args(0)))
    val developsFromOntology = generateDevelopsFromRules(ontology)
    manager.saveOntology(developsFromOntology, IRI.create(new File(args(1))))
  }

  def generateDevelopsFromRules(ontology: OWLOntology): OWLOntology = {
    val manager   = ontology.getOWLOntologyManager
    val newIRI    = ontology.getOntologyID.getOntologyIRI.toString + "/reverse_develops_from_rules.owl"
    val newAxioms = ontology.getClassesInSignature(Imports.EXCLUDED).asScala flatMap createRules
    manager.createOntology(newAxioms.toSet[OWLAxiom].asJava, IRI.create(newIRI))
  }

  def createRules(ontClass: OWLClass): Set[OWLSubClassOfAxiom] =
    Set(
      (has_part some (DEVELOPS_FROM some ontClass)) SubClassOf (has_part some ontClass),
      (has_part some (part_of some ontClass)) SubClassOf (has_part some ontClass)
    )

}
