package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConverters._
import org.semanticweb.elk.owlapi.ElkReasoner
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.reasoner.InferenceType
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.util.InferredAxiomGenerator
import org.semanticweb.owlapi.util.InferredClassAssertionAxiomGenerator
import org.semanticweb.owlapi.util.InferredEquivalentClassAxiomGenerator
import org.semanticweb.owlapi.util.InferredOntologyGenerator
import org.semanticweb.owlapi.util.InferredPropertyAssertionGenerator
import org.semanticweb.owlapi.util.InferredSubClassAxiomGenerator
import org.semanticweb.owlapi.util.OWLEntityRemover
import org.semanticweb.owlapi.model.OWLAxiom

object MaterializeInferences extends OWLTask {

  val REASONER = "org.phenoscape.owl.reasoner"
  val PROPERTIES_ONLY = "org.phenoscape.owl.reasoner.propertiesonly"

  def main(args: Array[String]): Unit = {
    val manager = OWLManager.createOWLOntologyManager
    val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)))
    materializeInferences(ontology)
    if (args.size > 1) {
      manager.saveOntology(ontology, IRI.create(new File(args(1))))
    } else {
      manager.saveOntology(ontology)
    }
    System.exit(0) //for some reason this is required for execution to terminate when using Elk
  }

  def materializeInferences(ontology: OWLOntology): Unit = {
    val reasoner = if (propertiesOnly()) {
      val manager = ontology.getOWLOntologyManager()
      val classes = ontology.getClassesInSignature()
      val tempOntology = manager.createOntology(
        ontology.getImportsClosure.asScala
          .flatMap(_.getAxioms().asScala)
          .toSet
          .asJava
      )
      val entityRemover = new OWLEntityRemover(Set(tempOntology).asJava)
      tempOntology.getClassesInSignature().asScala.foreach(entityRemover.visit)
      manager.applyChanges(entityRemover.getChanges())
      createReasoner(tempOntology, getReasonerChoice())
    } else {
      createReasoner(ontology, getReasonerChoice())
    }
    materializeInferences(ontology, reasoner)
    reasoner.dispose()
  }

  def materializeInferences(
      ontology: OWLOntology,
      reasoner: OWLReasoner
  ): Unit = {
    reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY) // this must be called first for ELK
    val axiomGenerators = List[InferredAxiomGenerator[_ <: OWLAxiom]](
      new InferredClassAssertionAxiomGenerator(),
      new InferredEquivalentClassAxiomGenerator(),
      new InferredSubClassAxiomGenerator()
    )
    val axiomGeneratorsUpdated = if (!reasoner.isInstanceOf[ElkReasoner]) {
      (new InferredPropertyAssertionGenerator()) :: axiomGenerators
    } else axiomGenerators
    val generator =
      new InferredOntologyGenerator(reasoner, axiomGeneratorsUpdated.asJava)
    generator.fillOntology(
      ontology.getOWLOntologyManager.getOWLDataFactory,
      ontology
    )
  }

  def getReasonerChoice(): String = {
    if (System.getProperties().containsKey(REASONER)) {
      return System.getProperty(REASONER)
    } else {
      return "elk"
    }
  }

  def createReasoner(ontology: OWLOntology, kind: String): OWLReasoner = {
    kind match {
      //case "hermit" => new ReasonerFactory().createReasoner(ontology)
      case "elk" => new ElkReasonerFactory().createReasoner(ontology)
      // case "trowl" => new RELReasonerFactory().createReasoner(ontology)
    }
  }

  def propertiesOnly(): Boolean = {
    if (System.getProperties().containsKey(PROPERTIES_ONLY)) {
      System.getProperty(PROPERTIES_ONLY).toBoolean
    } else {
      false
    }
  }

}
