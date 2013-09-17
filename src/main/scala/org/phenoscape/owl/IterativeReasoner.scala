package org.phenoscape.owl

import java.io.File
import scala.Array.canBuildFrom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.apibinding.OWLManager

object IterativeReasoner extends OWLTask {

  def main(args: Array[String]): Unit = {
    val mainManager = this.getOWLOntologyManager();
    //val targetManager = this.getOWLOntologyManager();
    val mainOntology = mainManager.loadOntologyFromOntologyDocument(new File(args(0)));
    val targetOntologies = args.slice(1, args.length).map(file => OWLManager.createOWLOntologyManager().loadOntologyFromOntologyDocument(new File(file)));
    computeInferences(mainOntology, targetOntologies);
    mainManager.saveOntology(mainOntology);
  }

  def computeInferences(mainOntology: OWLOntology, targetOntologies: Seq[OWLOntology]): Unit = {
    val manager = mainOntology.getOWLOntologyManager();
    val aggregateTargetOntology = manager.createOntology();
    targetOntologies.foreach(target => manager.addAxioms(aggregateTargetOntology, target.getAxioms()));
    computeInferences(mainOntology, aggregateTargetOntology);
  }

  def computeInferences(mainOntology: OWLOntology, targetOntology: OWLOntology): Unit = {
    val manager = mainOntology.getOWLOntologyManager();
    var oldAxiomCount = 0;
    var newAxiomCount = 1;
    while (newAxiomCount > oldAxiomCount) {
      oldAxiomCount = mainOntology.getAxiomCount();
      MaterializeInferences.materializeInferences(mainOntology, MaterializeInferences.createReasoner(mainOntology, "elk"));
      val hierarchy = ExtractNamedClassHierarchy.extractHierarchy(mainOntology);
      targetOntology.getOWLOntologyManager().addAxioms(targetOntology, hierarchy.getAxioms());
      targetOntology.getOWLOntologyManager().saveOntology(targetOntology, IRI.create(new File("/Users/jim/Desktop/target.owl")));
      MaterializeInferences.materializeInferences(targetOntology, MaterializeInferences.createReasoner(targetOntology, "trowl"));
      mainOntology.getOWLOntologyManager().addAxioms(mainOntology, targetOntology.getAxioms());
      newAxiomCount = mainOntology.getAxiomCount();
    }
  }

}