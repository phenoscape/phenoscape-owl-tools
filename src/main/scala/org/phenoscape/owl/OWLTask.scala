package org.phenoscape.owl

import java.util.UUID

import org.apache.log4j.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLNamedIndividual

class OWLTask {

  lazy val logger = Logger.getLogger(this.getClass)
  val factory = OWLManager.getOWLDataFactory

}
