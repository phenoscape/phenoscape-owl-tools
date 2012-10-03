package org.phenoscape.owl

import org.semanticweb.owlapi.model.IRI

object Vocab {

	val HAS_PART = IRI.create("http://purl.obolibrary.org/obo/BFO_0000051");
	val DEVELOPS_FROM = IRI.create("http://purl.obolibrary.org/obo/RO_0002202");
	val NEGATES = IRI.create("http://vocab.phenoscape.org/negation_of_class");
	val SUBCLADE_OF = IRI.create("http://vocab.phenoscape.org/subclade_of");
	val TAXON = IRI.create("http://rs.tdwg.org/dwc/terms/Taxon");

}