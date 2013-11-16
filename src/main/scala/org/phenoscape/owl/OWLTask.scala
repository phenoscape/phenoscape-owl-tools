package org.phenoscape.owl

import java.io.File
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntologyManager
import org.semanticweb.owlapi.util.AutoIRIMapper
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.IRI
import java.util.UUID
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.apache.log4j.Logger

class OWLTask {

  val ONTOLOGY_FILES = "org.phenoscape.owl.files"
  var uuid: String = UUID.randomUUID.toString
  var nodeIncrementer: Int = 0
  val factory = OWLManager.getOWLDataFactory

  def getOWLOntologyManager(): OWLOntologyManager = {
    val manager = OWLManager.createOWLOntologyManager()
    if (System.getProperties().containsKey(ONTOLOGY_FILES)) {
      val downloadsFolder = new File(System.getProperty(ONTOLOGY_FILES))
      manager.clearIRIMappers()
      manager.addIRIMapper(new BuilderIRIMapper(downloadsFolder))
    }
    manager
  }

  def nextIndividual(): OWLNamedIndividual = {
    factory.getOWLNamedIndividual(nextIRI())
  }

  def nextClass(): OWLClass = {
    factory.getOWLClass(this.nextIRI())
  }

  def nextIRI(): IRI = {
    this.nodeIncrementer += 1
    val id = "http://purl.org/phenoscape/uuid/" + this.uuid + "-" + this.nodeIncrementer
    IRI.create(id)
  }

  def logger: Logger = {
    Logger.getLogger(this.getClass)
  }

}