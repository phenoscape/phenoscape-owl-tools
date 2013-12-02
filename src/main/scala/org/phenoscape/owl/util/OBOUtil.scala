package org.phenoscape.owl.util

import org.semanticweb.owlapi.model.IRI

object OBOUtil {

  def iriForTermID(id: String): IRI = {
    if (id.startsWith("http://"))
      IRI.create(id)
    else
      IRI.create("http://purl.obolibrary.org/obo/" + id.replaceAll(":", "_"))
  }

  def zfinIRI(identifier: String): IRI = IRI.create("http://zfin.org/" + identifier)

}