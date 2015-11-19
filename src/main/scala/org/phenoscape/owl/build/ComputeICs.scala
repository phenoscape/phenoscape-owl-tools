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
import org.semanticweb.owlapi.model.OWLNamedIndividual

object ComputeICs extends App {

  val ontfile = new File(args(0))
  val profilesFile = new File(args(1))

  def inCorpus(ind: OWLNamedIndividual): Boolean = ind.getIRI.toString.contains("VTO_")
  def inQueries(ind: OWLNamedIndividual): Boolean = !ind.getIRI.toString.contains("VTO_")

  val manager = OWLManager.createOWLOntologyManager()
  val ontology = manager.loadOntologyFromOntologyDocument(ontfile)
  val profiles = manager.loadOntologyFromOntologyDocument(profilesFile)
  val combined = manager.createOntology((ontology.getAxioms.asScala ++ profiles.getAxioms.asScala).asJava, OntologyUtil.nextIRI)

  def outputICs(corpusFunction: OWLNamedIndividual => Boolean, filename: String): Unit = {
    val owlSim = new OWLsim(combined, corpusFunction)
    val triples = owlSim.classICScoresAsTriples
    val triplesOutput = new BufferedOutputStream(new FileOutputStream(new File(filename)))
    val writer = Rio.createWriter(RDFFormat.TURTLE, triplesOutput)
    writer.startRDF()
    triples.foreach(writer.handleStatement)
    writer.endRDF()
    triplesOutput.close()
  }

  outputICs(inCorpus, "corpus_ics.ttl")
  outputICs(inQueries, "query_ics.ttl")

}