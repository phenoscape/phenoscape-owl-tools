package org.phenoscape.owl.util

import org.semanticweb.owlapi.model.IRI
import org.phenoscape.owl.mod.mgi.MGIAnatomyBridgeToEMAPA

object OBOUtil {

  def iriForTermID(id: String): IRI = {
    if (id.startsWith("http://"))
      IRI.create(id)
    else
      IRI.create("http://purl.obolibrary.org/obo/" + id.replaceAll(":", "_"))
  }

  def zfinIRI(identifier: String): IRI = IRI.create("http://zfin.org/" + identifier)
  
  def mgiAnatomyIRI(identifier: String): IRI = IRI.create(MGIAnatomyBridgeToEMAPA.ontologyName + "#" + identifier.replaceAllLiterally(":", "_"))
  
  def mgiReferenceIRI(identifier: String): IRI = IRI.create("http://www.informatics.jax.org/reference/summary?id=" + identifier)

}