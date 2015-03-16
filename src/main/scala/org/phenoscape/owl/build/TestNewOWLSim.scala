package org.phenoscape.owl.build

import java.io.File
import java.util.Date
import scala.collection.JavaConverters._
import org.apache.commons.io.FileUtils
import org.phenoscape.owl.sim.OWLsim
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.openrdf.rio.Rio
import java.io.FileOutputStream
import java.io.BufferedOutputStream
import org.openrdf.rio.turtle.TurtleWriter
import org.openrdf.rio.RDFFormat

object TestNewOWLSim extends App {

  val taskCount = args(0).toInt
  val taskNum = args(1).toInt

  val manager = OWLManager.createOWLOntologyManager()
  val ontfile = new File("../staging/kb/tbox-hierarchy-and-profiles-2015-2-24.owl")
  //val ontfile = new File("uberpheno.owl")
  val ontology = manager.loadOntologyFromOntologyDocument(ontfile)
  println("Creating OWLSim")
  val owlSim = new OWLsim(ontology, ind => ind.getIRI.toString.contains("VTO_"))
  //val owlSim = new OWLsim(ontology, ind => true)
  println("Done creating OWLSim")
  val geneProfiles = owlSim.allIndividuals.filterNot(ind => ind.getIRI.toString.contains("VTO_"))
  val orderedProfiles = geneProfiles.toSeq.sortBy(_.getIRI.toString())
  val numProfiles = orderedProfiles.size
  val groupSize = (numProfiles.toFloat / taskCount).ceil.toInt
  val startIndex = (taskNum - 1) * groupSize
  val group = orderedProfiles.drop(startIndex).take(groupSize)
  println("Computing similarity matrix")
  val similarityMatrix = owlSim.computeAllSimilarityToCorpus(group.toSet)
  //val similarityMatrix = owlSim.computeAllSimilarityToCorpus(owlSim.allIndividuals)
  println("Writing results to file")
  //FileUtils.writeLines(new File("similarities.txt"), similarityMatrix.asJavaCollection)
  val triplesOutput = new BufferedOutputStream(new FileOutputStream(new File(s"similarities-$taskNum.ttl")))
  Rio.write(similarityMatrix.asJava, triplesOutput, RDFFormat.TURTLE)
  triplesOutput.close()
  println("Done: " + new Date())

}