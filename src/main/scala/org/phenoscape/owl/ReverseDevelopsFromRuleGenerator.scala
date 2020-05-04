package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConverters._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom
import org.semanticweb.owlapi.model.OWLAxiom
import org.phenoscape.owl.Vocab._

object ReverseDevelopsFromRuleGenerator extends OWLTask {

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
    val newAxioms = ontology.getClassesInSignature(false).asScala flatMap createRules
    manager.createOntology(newAxioms.toSet[OWLAxiom].asJava, IRI.create(newIRI))
  }

  def createRules(ontClass: OWLClass): Set[OWLSubClassOfAxiom] =
    Set(
      (has_part some (DEVELOPS_FROM some ontClass)) SubClassOf (has_part some ontClass),
      (has_part some (part_of some ontClass)) SubClassOf (has_part some ontClass)
    )

}
