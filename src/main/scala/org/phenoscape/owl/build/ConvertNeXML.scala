package org.phenoscape.owl.build

import java.io.File

import org.phenoscape.owl.PhenexToOWL
import org.phenoscape.owl.PropertyNormalizer
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.IRI
import java.io.FileOutputStream

object ConvertNeXML extends App {

  val ontologyPath = args(0)
  val outputFile = args(1)
  val nexmlFiles = args.drop(2)
  val manager = OWLManager.createOWLOntologyManager()
  val ontology = manager.loadOntology(IRI.create(new File(ontologyPath)))
  nexmlFiles.foreach { file =>
    val nexmlOnt = PropertyNormalizer.normalize(PhenexToOWL.convert(new File(file), ontology))
    manager.saveOntology(nexmlOnt, new FunctionalSyntaxDocumentFormat(), new FileOutputStream(new File(outputFile)))
  }

}