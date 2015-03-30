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

object PrintTaxonAndGeneProfileSizes extends App {

  val ontfile = new File(args(0))
  val profilesFile = new File(args(1))

  val manager = OWLManager.createOWLOntologyManager()
  val ontology = manager.loadOntologyFromOntologyDocument(ontfile)
  val profiles = manager.loadOntologyFromOntologyDocument(profilesFile)
  val combined = manager.createOntology((ontology.getAxioms.asScala ++ profiles.getAxioms.asScala).asJava, OntologyUtil.nextIRI)
  val owlSim = new OWLsim(combined, ind => ind.getIRI.toString.contains("VTO_"))
  owlSim.directAssociationsByIndividual.foreach {
    case (profile, annotations) =>
      println(s"${profile.getIRI.toString}: ${annotations.size}")
  }

}