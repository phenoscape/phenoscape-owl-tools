package org.phenoscape.owl

import org.semanticweb.owlapi.model.{IRI, OWLOntologyIRIMapper}

import java.io.File
import java.net.URLEncoder

class BuilderIRIMapper(folder: File) extends OWLOntologyIRIMapper {

  def getDocumentIRI(iri: IRI): IRI =
    if (iri.getScheme == "file") iri
    else IRI.create(new File(folder, URLEncoder.encode(iri.toURI.toString, "UTF-8")))

}
