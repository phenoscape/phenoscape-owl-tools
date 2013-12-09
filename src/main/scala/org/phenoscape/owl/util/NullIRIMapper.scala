package org.phenoscape.owl.util

import org.semanticweb.owlapi.model.OWLOntologyIRIMapper
import org.semanticweb.owlapi.model.IRI

object NullIRIMapper extends OWLOntologyIRIMapper {

  override def getDocumentIRI(iri: IRI): IRI = null

}