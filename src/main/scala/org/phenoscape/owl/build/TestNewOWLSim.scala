package org.phenoscape.owl.build

import java.io.File
import java.util.Date
import scala.collection.JavaConverters._
import org.apache.commons.io.FileUtils
import org.phenoscape.owl.sim.OWLsim
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLNamedIndividual

object TestNewOWLSim extends App {

  val manager = OWLManager.createOWLOntologyManager()
  val ontfile = new File("staging/kb/tbox-hierarchy-and-profiles-2015-2-24.owl")
  //val ontfile = new File("uberpheno.owl")
  val ontology = manager.loadOntologyFromOntologyDocument(ontfile)
  println("Creating OWLSim")
  val owlSim = new OWLsim(ontology, ind => ind.getIRI.toString.contains("VTO_"))
  //val owlSim = new OWLsim(ontology, ind => true)
  println("Done creating OWLSim")
  val geneProfiles = owlSim.allIndividuals.filterNot(ind => ind.getIRI.toString.contains("VTO_"))
  println("Computing similarity matrix")
  val similarityMatrix = owlSim.computeAllSimilarityToCorpus(geneProfiles)
  //val similarityMatrix = owlSim.computeAllSimilarityToCorpus(owlSim.allIndividuals)
  println("Writing results to file")
  FileUtils.writeLines(new File("similarities.txt"), similarityMatrix.asJavaCollection)
  println("Done: " + new Date())

}