package org.phenoscape.owl.build

import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.util.Date

import scala.collection.JavaConverters._

import org.openrdf.rio.RDFFormat
import org.openrdf.rio.Rio
import org.phenoscape.owl.sim.OWLsim
import org.phenoscape.owl.util.OntologyUtil
import org.semanticweb.owlapi.apibinding.OWLManager

object RunPairwiseOWLSim extends App {

  val taskCount = args(0).toInt
  val taskNum = args(1).toInt
  val ontfile = new File(args(2))
  val profilesFile = new File(args(3))

  val manager = OWLManager.createOWLOntologyManager()
  val ontology = manager.loadOntologyFromOntologyDocument(ontfile)
  val profiles = manager.loadOntologyFromOntologyDocument(profilesFile)
  val combined = manager.createOntology((ontology.getAxioms.asScala ++ profiles.getAxioms.asScala).asJava, OntologyUtil.nextIRI)
  println("Creating OWLSim")
  val owlSim = new OWLsim(combined, ind => ind.getIRI.toString.contains("VTO_"))
  println("Done creating OWLSim")
  val geneProfiles = owlSim.allIndividuals.filterNot(ind => ind.getIRI.toString.contains("VTO_"))
  val orderedProfiles = geneProfiles.toSeq.sortBy(_.getIRI.toString())
  val numProfiles = orderedProfiles.size
  val groupSize = (numProfiles.toFloat / taskCount).ceil.toInt
  val startIndex = (taskNum - 1) * groupSize
  val group = orderedProfiles.drop(startIndex).take(groupSize)
  println("Computing similarity matrix")
  val similarityMatrix = owlSim.computeAllSimilarityToCorpus(group.toSet)
  println("Writing results to file")
  val triplesOutput = new BufferedOutputStream(new FileOutputStream(new File(s"similarities-$taskNum.ttl")))
  val writer = Rio.createWriter(RDFFormat.TURTLE, triplesOutput)
  writer.startRDF()
  similarityMatrix.foreach(writer.handleStatement)
  writer.endRDF()
  triplesOutput.close()
  println("Done: " + new Date())

}