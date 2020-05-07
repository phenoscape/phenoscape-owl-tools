package org.phenoscape.owl.build

import java.io.File
import java.util.Date

import org.phenoscape.kb.ingest.util.OntUtil
import org.phenoscape.owl.sim.OWLsim
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLNamedIndividual

import scala.collection.JavaConverters._

object RunPairwiseOWLSim extends App {

  val taskCount = args(0).toInt
  val taskNum = args(1).toInt
  val ontfile = new File(args(2))
  val profilesFile = new File(args(3))
  val corpus = args(4)
  val outfile = args(5)

  val manager = OWLManager.createOWLOntologyManager()
  val ontology = manager.loadOntologyFromOntologyDocument(ontfile)
  val profiles = manager.loadOntologyFromOntologyDocument(profilesFile)

  val combined =
    manager.createOntology((ontology.getAxioms().asScala ++ profiles.getAxioms().asScala).asJava, OntUtil.nextIRI)

  println("Creating OWLSim")
  def isTaxon(ind: OWLNamedIndividual): Boolean = ind.getIRI.toString.contains("VTO_")

  val inCorpusFunc = if (corpus == "taxa") { ind: OWLNamedIndividual =>
    ind.getIRI.toString.contains("VTO_")
  } else if (corpus == "genes") { ind: OWLNamedIndividual =>
    !ind.getIRI.toString.contains("VTO_")
  } else throw new RuntimeException("Invalid corpus name.")

  val owlSim = new OWLsim(combined, inCorpusFunc)
  println("Done creating OWLSim")
  val queryProfiles = owlSim.allIndividuals.filterNot(inCorpusFunc)
  val orderedProfiles = queryProfiles.toSeq.sortBy(_.getIRI.toString())
  val numProfiles = orderedProfiles.size
  val groupSize = (numProfiles.toFloat / taskCount).ceil.toInt
  val startIndex = (taskNum - 1) * groupSize
  val group = orderedProfiles.drop(startIndex).take(groupSize)
  println("Computing similarity matrix")
  owlSim.computeAllSimilarityToCorpusDirectOutput(group.toSet, new File(outfile))
  println("Done: " + new Date())
}
