package org.phenoscape.owl

import org.semanticweb.owlapi.model.IRI

object Vocab {

	val HAS_PART = IRI.create("http://purl.obolibrary.org/obo/BFO_0000051");
	val PART_OF = IRI.create("http://purl.obolibrary.org/obo/BFO_0000050");
	val BEARER_OF = IRI.create("http://purl.obolibrary.org/obo/BFO_0000053");
	val DEVELOPS_FROM = IRI.create("http://purl.obolibrary.org/obo/RO_0002202");
	val NEGATES = IRI.create("http://vocab.phenoscape.org/negation_of_class");
	val MAY_HAVE_STATE_VALUE = IRI.create("http://vocab.phenoscape.org/may_have_state_value");
	val SUBCLADE_OF = IRI.create("http://vocab.phenoscape.org/subclade_of");
	val HAS_MEMBER = IRI.create("http://vocab.phenoscape.org/has_member");
	val TAXON = IRI.create("http://rs.tdwg.org/dwc/terms/Taxon");
	val CHARACTER_STATE_DATA_MATRIX = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000056");
	val HAS_CHARACTER = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000142");
	val HAS_TU = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000208");
	val STANDARD_CHARACTER = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000075");
	val STANDARD_STATE = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000045");
	val STANDARD_CELL = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000008");
	val TU = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000138");
	val HAS_EXTERNAL_REFERENCE = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000164");
	val BELONGS_TO_CHARACTER = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000205");
	val BELONGS_TO_TU = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000191");
	val HAS_STATE = IRI.create("http://purl.obolibrary.org/obo/CDAO_0000184");
	val DENOTES = IRI.create("http://purl.obolibrary.org/obo/IAO_0000219");
	val DENOTES_EXEMPLAR = IRI.create("http://vocab.phenoscape.org/denotes_exemplar");
	val TOWARDS = IRI.create("http://purl.obolibrary.org/obo/pato#towards");
	val INDIVIDUAL_ID = IRI.create("http://rs.tdwg.org/dwc/terms/individualID");
	val SPECIMEN = IRI.create("http://purl.org/dsw/Specimen");
	val SPECIMEN_TO_COLLECTION = IRI.create("http://rs.tdwg.org/dwc/terms/collectionID");
	val SPECIMEN_TO_CATALOG_ID = IRI.create("http://rs.tdwg.org/dwc/terms/catalogNumber");
	val ABSENT = IRI.create("http://purl.obolibrary.org/obo/PATO_0000462");
	val INVOLVES = IRI.create("http://vocab.phenoscape.org/involves");
	val GENE_EXPRESSION = IRI.create("http://purl.obolibrary.org/obo/GO_0010467");
	val PHENOTYPE_ANNOTATION = IRI.create("http://vocab.phenoscape.org/PhenotypeAnnotation");
	val OCCURS_IN = IRI.create("http://purl.obolibrary.org/obo/BFO_0000066");
	val GENE = IRI.create("http://purl.obolibrary.org/obo/SO_0000704");
	val ANNOTATED_GENE = IRI.create("http://vocab.phenoscape.org/annotated_gene");
	
}