package org.phenoscape.owl

import org.semanticweb.owlapi.model.IRI

object Vocab {

	val HAS_PART = IRI.create("http://purl.obolibrary.org/obo/BFO_0000051");
	val PART_OF = IRI.create("http://purl.obolibrary.org/obo/BFO_0000050");
	val BEARER_OF = IRI.create("http://purl.obolibrary.org/obo/BFO_0000053");
	val DEVELOPS_FROM = IRI.create("http://purl.obolibrary.org/obo/RO_0002202");
	val NEGATES = IRI.create("http://vocab.phenoscape.org/negation_of_class");
	val SUBCLADE_OF = IRI.create("http://vocab.phenoscape.org/subclade_of");
	val TAXON = IRI.create("http://rs.tdwg.org/dwc/terms/Taxon");
	val CHARACTER_STATE_DATA_MATRIX = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000056");
	val HAS_CHARACTER = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000142");
	val STANDARD_CHARACTER = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000075");
	val STANDARD_STATE = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000045");
	val DENOTES = IRI.create("http://purl.obolibrary.org/obo/IAO_0000219");
	val DENOTES_EXEMPLAR = IRI.create("http://vocab.phenoscape.org/denotes_exemplar");
	val TOWARDS = IRI.create("http://purl.obolibrary.org/obo/pato#towards");
}