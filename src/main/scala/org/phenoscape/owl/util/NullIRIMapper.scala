package org.phenoscape.owl.util

import org.semanticweb.owlapi.model.{IRI, OWLOntologyIRIMapper}

object NullIRIMapper extends OWLOntologyIRIMapper {

  override def getDocumentIRI(iri: IRI): IRI = null

}
