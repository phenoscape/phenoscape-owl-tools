package org.phenoscape.owl

import org.semanticweb.owlapi.model.IRI

object Vocab {

	val HAS_PART = IRI.create("http://purl.obolibrary.org/obo/BFO_0000051");
	val PART_OF = IRI.create("http://purl.obolibrary.org/obo/BFO_0000050");
	val BEARER_OF = IRI.create("http://purl.obolibrary.org/obo/BFO_0000053");
	val INHERES_IN = IRI.create("http://purl.obolibrary.org/obo/BFO_0000052");
	val exhibits = IRI.create("http://purl.org/phenoscape/vocab/exhibits");
	val DEVELOPS_FROM = IRI.create("http://purl.obolibrary.org/obo/RO_0002202");
	val NEGATES = IRI.create("http://purl.org/phenoscape/vocab/negation_of_class");
	val MAY_HAVE_STATE_VALUE = IRI.create("http://purl.org/phenoscape/vocab/may_have_state_value");
	val SUBCLADE_OF = IRI.create("http://purl.org/phenoscape/vocab/subclade_of");
	val HAS_MEMBER = IRI.create("http://purl.org/phenoscape/vocab/has_member");
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
	val DENOTES_EXHIBITING = IRI.create("http://purl.org/phenoscape/vocab/denotes_exhibiting");
	val TOWARDS = IRI.create("http://purl.obolibrary.org/obo/pato#towards");
	val INDIVIDUAL_ID = IRI.create("http://rs.tdwg.org/dwc/terms/individualID");
	val SPECIMEN = IRI.create("http://purl.org/dsw/Specimen");
	val SPECIMEN_TO_COLLECTION = IRI.create("http://rs.tdwg.org/dwc/terms/collectionID");
	val SPECIMEN_TO_CATALOG_ID = IRI.create("http://rs.tdwg.org/dwc/terms/catalogNumber");
	val ABSENT = IRI.create("http://purl.obolibrary.org/obo/PATO_0000462");
	val PRESENT = IRI.create("http://purl.obolibrary.org/obo/PATO_0000467");
	val LACKS_ALL_PARTS_OF_TYPE = IRI.create("http://purl.obolibrary.org/obo/PATO_0002000");
	val INVOLVES = IRI.create("http://purl.org/phenoscape/vocab/involves");
	val GENE_EXPRESSION = IRI.create("http://purl.obolibrary.org/obo/GO_0010467");
	val ANNOTATED_PHENOTYPE = IRI.create("http://purl.org/phenoscape/vocab/PhenotypeAnnotation"); //FIXME change IRI
	val OCCURS_IN = IRI.create("http://purl.obolibrary.org/obo/BFO_0000066");
	val GENE = IRI.create("http://purl.obolibrary.org/obo/SO_0000704");
	val ASSOCIATED_WITH_GENE = IRI.create("http://purl.org/phenoscape/vocab/annotated_gene"); //FIXME change IRI
	val ANNOTATED_ORGANISM = IRI.create("http://purl.org/phenoscape/vocab/annotated_organism");
	val HAS_EXACT_SYNONYM = IRI.create("http://www.geneontology.org/formats/oboInOwl#hasExactSynonym");
	val HAS_RELATED_SYNONYM = IRI.create("http://www.geneontology.org/formats/oboInOwl#hasRelatedSynonym");
	val ASSOCIATED_WITH_TAXON = IRI.create("http://purl.org/phenoscape/vocab/annotated_taxon"); //FIXME change IRI
	val ZEBRAFISH = IRI.create("http://purl.obolibrary.org/obo/NCBITaxon_7955");
	val XENOPUS_LAEVIS = IRI.create("http://purl.obolibrary.org/obo/NCBITaxon_8355");
	val XENOPUS_TROPICALIS = IRI.create("http://purl.obolibrary.org/obo/NCBITaxon_8364");
	val MOUSE = IRI.create("http://purl.obolibrary.org/obo/NCBITaxon_10090");
	val HUMAN = IRI.create("http://purl.obolibrary.org/obo/NCBITaxon_9606");
	val CHORDATA = IRI.create("http://purl.obolibrary.org/obo/VTO_0000001");
	val DEPICTS = IRI.create("http://xmlns.com/foaf/0.1/depicts");
	val IMAGE = IRI.create("http://xmlns.com/foaf/0.1/Image");
	val LIMB_FIN = IRI.create("http://purl.obolibrary.org/obo/UBERON_0004708");
	val HOMOLOGOUS_TO = IRI.create("http://purl.obolibrary.org/obo/RO_0002158");
	val DERIVED_BY_DESCENT_FROM = IRI.create("http://purl.obolibrary.org/obo/RO_0002156");
	val HAS_DERIVED_BY_DESCENDANT = IRI.create("http://purl.obolibrary.org/obo/RO_0002157");
	val EVIDENCE = IRI.create("http://www.geneontology.org/formats/oboInOwl#evidence");
	val ANATOMICAL_ENTITY = IRI.create("http://purl.obolibrary.org/obo/UBERON_0001062");
	val QUALITY = IRI.create("http://purl.obolibrary.org/obo/PATO_0000001");
	val PHP = IRI.create("http://example.org/php"); // part_of some homologous_to some part_of... experimental
	val MULTI_CELLULAR_ORGANISM = IRI.create("http://purl.obolibrary.org/obo/UBERON_0000468");
}