package org.phenoscape.owl.build

import org.phenoscape.owl.{PhenexToOWL, PropertyNormalizer}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.IRI

import java.io.{File, FileOutputStream}

object ConvertNeXML extends App {

  val ontologyPath = args(0)
  val nexmlFile = args(1)
  val outputFile = args(2)
  val manager = OWLManager.createOWLOntologyManager()
  val ontology = manager.loadOntology(IRI.create(new File(ontologyPath)))

  val nexmlOnt = PropertyNormalizer.normalize(PhenexToOWL.convert(new File(nexmlFile), ontology))
  manager.saveOntology(nexmlOnt, new FunctionalSyntaxDocumentFormat(), new FileOutputStream(new File(outputFile)))

}
