package org.phenoscape.owl.util

import org.semanticweb.owlapi.model.IRI
import org.phenoscape.owl.mod.mgi.MGIAnatomyBridgeToEMAPA
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.apibinding.OWLManager
import org.phenoscape.scowl.OWL._

object OBOUtil {

  val factory = OWLManager.getOWLDataFactory

  def iriForTermID(id: String): IRI = {
    if (id.startsWith("http://"))
      IRI.create(id)
    else
      IRI.create("http://purl.obolibrary.org/obo/" + id.replaceAll(":", "_"))
  }

  def zfinIRI(identifier: String): IRI = IRI.create("http://zfin.org/" + identifier)

  def mgiAnatomyIRI(identifier: String): IRI = IRI.create(MGIAnatomyBridgeToEMAPA.ontologyName + "#" + identifier.replaceAllLiterally(":", "_"))

  def mgiReferenceIRI(identifier: String): IRI = IRI.create("http://www.informatics.jax.org/reference/summary?id=" + identifier)

  def createDefinedByAnnotation(term: OWLEntity): Option[OWLAnnotationAssertionAxiom] = {
    val iri = term.getIRI.toString
    if (iri.startsWith("http://purl.obolibrary.org/obo/")) {
      val prefix = iri.stripPrefix("http://purl.obolibrary.org/obo/").split("_", -1).dropRight(1).mkString("_")
      val ontIRI = "http://purl.obolibrary.org/obo/" + prefix.toLowerCase + ".owl"
      Option(term Annotation (factory.getRDFSIsDefinedBy, IRI.create(ontIRI)))
    } else None
  }

}