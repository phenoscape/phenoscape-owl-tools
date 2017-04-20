package org.phenoscape.owl.build

import java.io.File
import scala.collection.JavaConverters._
import org.phenoscape.owl.sim.OWLsim
import org.phenoscape.kb.ingest.util.OntUtil
import org.semanticweb.owlapi.apibinding.OWLManager
import java.io.FileWriter
import java.io.BufferedWriter

object PrintTaxonAndGeneProfileSizes extends App {

  val ontfile = new File(args(0))
  val profilesFile = new File(args(1))
  val outFile = new File(args(2))

  val manager = OWLManager.createOWLOntologyManager()
  val ontology = manager.loadOntologyFromOntologyDocument(ontfile)
  val profiles = manager.loadOntologyFromOntologyDocument(profilesFile)
  val combined = manager.createOntology((ontology.getAxioms().asScala ++ profiles.getAxioms().asScala).asJava, OntUtil.nextIRI)
  val owlSim = new OWLsim(combined, ind => ind.getIRI.toString.contains("VTO_"))
  val bw = new BufferedWriter(new FileWriter(outFile))
  owlSim.directAssociationsByIndividual.foreach {
    case (profile, annotations) =>
      bw.write(s"${profile.getIRI.toString}\t${annotations.size}\n")
  }
  bw.close()

}