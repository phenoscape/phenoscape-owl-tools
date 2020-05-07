package org.phenoscape.owl.build

import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.util.Date
import scala.collection.JavaConverters._
import org.openrdf.rio.RDFFormat
import org.openrdf.rio.Rio
import org.phenoscape.owl.sim.OWLsim
import org.phenoscape.kb.ingest.util.OntUtil
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLNamedIndividual

object ComputeICs extends App {

  val ontfile = new File(args(0))
  val profilesFile = new File(args(1))
  val corpus = args(2)
  val outfile = new File(args(3))

  val inCorpusFunc = if (corpus == "taxa") { ind: OWLNamedIndividual =>
    ind.getIRI.toString.contains("VTO_")
  } else if (corpus == "genes") { ind: OWLNamedIndividual =>
    !ind.getIRI.toString.contains("VTO_")
  } else throw new RuntimeException("Invalid corpus name.")

  def inQueriesFunc(ind: OWLNamedIndividual): Boolean = !(inCorpusFunc(ind))

  val manager = OWLManager.createOWLOntologyManager()
  val ontology = manager.loadOntologyFromOntologyDocument(ontfile)
  val profiles = manager.loadOntologyFromOntologyDocument(profilesFile)

  val combined =
    manager.createOntology((ontology.getAxioms().asScala ++ profiles.getAxioms().asScala).asJava, OntUtil.nextIRI)

  val owlSim = new OWLsim(combined, inCorpusFunc)
  val triples = owlSim.classICScoresAsTriples
  val triplesOutput = new BufferedOutputStream(new FileOutputStream(outfile))
  val writer = Rio.createWriter(RDFFormat.TURTLE, triplesOutput)
  writer.startRDF()
  triples.foreach(writer.handleStatement)
  writer.endRDF()
  triplesOutput.close()

}
