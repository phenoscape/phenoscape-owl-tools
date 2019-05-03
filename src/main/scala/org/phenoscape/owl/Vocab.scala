package org.phenoscape.owl

import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager

import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

object Vocab {

  private val factory = OWLManager.getOWLDataFactory

  val rdfType = OWLRDFVocabulary.RDF_TYPE.getIRI
  val rdfsSubClassOf = OWLRDFVocabulary.RDFS_SUBCLASS_OF.getIRI
  val rdfsLabel = OWLManager.getOWLDataFactory.getRDFSLabel
  val dcDescription = DublinCoreVocabulary.DESCRIPTION.getIRI
  val has_part = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000051")
  val part_of = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000050")
  val bearer_of = ObjectProperty("http://purl.obolibrary.org/obo/RO_0000053")
  val inheres_in = ObjectProperty("http://purl.obolibrary.org/obo/RO_0000052")
  val inheres_in_part_of = ObjectProperty("http://purl.obolibrary.org/obo/RO_0002314")
  val EXHIBITS = ObjectProperty("http://purl.org/phenoscape/vocab.owl#exhibits")
  val DEVELOPS_FROM = ObjectProperty("http://purl.obolibrary.org/obo/RO_0002202")
  val NEGATES = IRI.create("http://purl.org/phenoscape/vocab.owl#negation_of_class")
  val entity_term = AnnotationProperty("http://purl.org/phenoscape/vocab.owl#entity_term")
  val quality_term = AnnotationProperty("http://purl.org/phenoscape/vocab.owl#quality_term")
  val related_entity_term = AnnotationProperty("http://purl.org/phenoscape/vocab.owl#related_entity_term")
  val may_have_state_value = ObjectProperty("http://purl.org/phenoscape/vocab.owl#may_have_state_value")
  val subclade_of = ObjectProperty("http://purl.org/phenoscape/vocab.owl#subclade_of")
  val has_member = ObjectProperty("http://purl.org/phenoscape/vocab.owl#has_member")
  val Taxon = Class("http://rs.tdwg.org/dwc/terms/Taxon")
  val CharacterStateDataMatrix = Class("http://purl.obolibrary.org/obo/CDAO_0000056")
  val has_character = ObjectProperty("http://purl.obolibrary.org/obo/CDAO_0000142")
  val has_TU = ObjectProperty("http://purl.obolibrary.org/obo/CDAO_0000208")
  val StandardCharacter = Class("http://purl.obolibrary.org/obo/CDAO_0000075")
  val StandardState = Class("http://purl.obolibrary.org/obo/CDAO_0000045")
  val StandardCell = Class("http://purl.obolibrary.org/obo/CDAO_0000008")
  val TU = Class("http://purl.obolibrary.org/obo/CDAO_0000138")
  val has_external_reference = ObjectProperty("http://purl.obolibrary.org/obo/CDAO_0000164")
  val belongs_to_character = ObjectProperty("http://purl.obolibrary.org/obo/CDAO_0000205")
  val belongs_to_TU = ObjectProperty("http://purl.obolibrary.org/obo/CDAO_0000191")
  val has_state = ObjectProperty("http://purl.obolibrary.org/obo/CDAO_0000184")
  val towards = ObjectProperty("http://purl.obolibrary.org/obo/RO_0002503")
  val individual_id = ObjectProperty("http://rs.tdwg.org/dwc/terms/individualID")
  val Specimen = Class("http://purl.org/dsw/Specimen")
  val collectionID = ObjectProperty("http://rs.tdwg.org/dwc/terms/collectionID")
  val catalogNumber = factory.getOWLDataProperty(IRI.create("http://rs.tdwg.org/dwc/terms/catalogNumber"))
  val Absent = Class("http://purl.obolibrary.org/obo/PATO_0000462")
  val Present = Class("http://purl.obolibrary.org/obo/PATO_0000467")
  val LacksAllPartsOfType = Class("http://purl.obolibrary.org/obo/PATO_0002000")
  val involves = ObjectProperty("http://purl.org/phenoscape/vocab.owl#involves")
  val GeneExpression = Class("http://purl.obolibrary.org/obo/GO_0010467")
  val AnnotatedPhenotype = Class("http://purl.org/phenoscape/vocab.owl#AnnotatedPhenotype")
  val occurs_in = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000066")
  val Gene = Class("http://purl.obolibrary.org/obo/SO_0000704")
  val associated_with_gene = ObjectProperty("http://purl.org/phenoscape/vocab.owl#associated_with_gene")
  val ANNOTATED_ORGANISM = ObjectProperty("http://purl.org/phenoscape/vocab.owl#annotated_organism")
  val HAS_EXACT_SYNONYM = IRI.create("http://www.geneontology.org/formats/oboInOwl#hasExactSynonym")
  val HAS_RELATED_SYNONYM = IRI.create("http://www.geneontology.org/formats/oboInOwl#hasRelatedSynonym")
  val associated_with_taxon = ObjectProperty("http://purl.org/phenoscape/vocab.owl#associated_with_taxon")
  val Zebrafish = Individual("http://purl.obolibrary.org/obo/NCBITaxon_7955")
  val XENOPUS_LAEVIS = IRI.create("http://purl.obolibrary.org/obo/NCBITaxon_8355")
  val XENOPUS_TROPICALIS = IRI.create("http://purl.obolibrary.org/obo/NCBITaxon_8364")
  val MOUSE = IRI.create("http://purl.obolibrary.org/obo/NCBITaxon_10090")
  val HUMAN = IRI.create("http://purl.obolibrary.org/obo/NCBITaxon_9606")
  val CHORDATA = IRI.create("http://purl.obolibrary.org/obo/VTO_0000001")
  val DEPICTS = ObjectProperty("http://xmlns.com/foaf/0.1/depicts")
  val IMAGE = IRI.create("http://xmlns.com/foaf/0.1/Image")
  val LIMB_FIN = IRI.create("http://purl.obolibrary.org/obo/UBERON_0004708")
  val AppendageGirdleComplex = Class("http://purl.obolibrary.org/obo/UBERON_0010707")
  val HOMOLOGOUS_TO = ObjectProperty("http://purl.obolibrary.org/obo/RO_0002158")
  val DERIVED_BY_DESCENT_FROM = ObjectProperty("http://purl.obolibrary.org/obo/RO_0002156")
  val HAS_DERIVED_BY_DESCENDANT = ObjectProperty("http://purl.obolibrary.org/obo/RO_0002157")
  val has_evidence = ObjectProperty("http://purl.obolibrary.org/obo/RO_0002558")
  val axiom_has_evidence = AnnotationProperty("http://purl.obolibrary.org/obo/RO_0002612")
  val ANATOMICAL_ENTITY = IRI.create("http://purl.obolibrary.org/obo/UBERON_0001062")
  val QUALITY = IRI.create("http://purl.obolibrary.org/obo/PATO_0000001")
  val PHP = ObjectProperty("http://example.org/php") // part_of some homologous_to some part_of... experimental
  val MultiCellularOrganism = Class("http://purl.obolibrary.org/obo/UBERON_0000468")
  val ABSENCE_OF = IRI.create("http://purl.org/phenoscape/vocab.owl#absence_of")
  val IMPLIES_PRESENCE_OF = ObjectProperty("http://purl.org/phenoscape/vocab.owl#implies_presence_of")
  val EQCharacterToken = Class("http://purl.org/phenoscape/vocab.owl#EQCharacterToken")
  val HasNumberOf = Class("http://purl.obolibrary.org/obo/PATO_0001555")
  val dcSource = ObjectProperty(IRI.create("http://purl.org/dc/terms/source"))
  val connected_to = ObjectProperty(IRI.create("http://purl.obolibrary.org/obo/core#connected_to"))
  val list_index = factory.getOWLAnnotationProperty(IRI.create("http://purl.org/phenoscape/vocab.owl#list_index"))
  val state_symbol = factory.getOWLAnnotationProperty(IRI.create("http://purl.org/phenoscape/vocab.owl#state_symbol"))
  val exhibits_state = ObjectProperty("http://purl.org/phenoscape/vocab.owl#exhibits_state")
  val describes_phenotype = AnnotationProperty("http://purl.org/phenoscape/vocab.owl#describes_phenotype")
  val has_presence_of = ObjectProperty("http://purl.org/phenoscape/vocab.owl#has_presence_of")
  val has_absence_of = ObjectProperty("http://purl.org/phenoscape/vocab.owl#has_absence_of")
  val has_phenotypic_profile = IRI.create("http://purl.org/phenoscape/vocab.owl#has_phenotypic_profile")
  val phenotype_of = ObjectProperty("http://purl.org/phenoscape/vocab.owl#phenotype_of")
  val has_part_inhering_in = ObjectProperty("http://purl.org/phenoscape/vocab.owl#has_part_inhering_in")
  val in_taxon = ObjectProperty("http://purl.obolibrary.org/obo/RO_0002162")

  val combined_score = ObjectProperty("http://purl.org/phenoscape/vocab.owl#combined_score")
  val has_ic = ObjectProperty("http://purl.org/phenoscape/vocab.owl#has_ic")
  val has_expect_score = ObjectProperty("http://purl.org/phenoscape/vocab.owl#has_expect_score")
  val has_subsumer = ObjectProperty("http://purl.org/phenoscape/vocab.owl#has_subsumer")
  val for_query_profile = ObjectProperty("http://purl.org/phenoscape/vocab.owl#for_query_profile")
  val for_corpus_profile = ObjectProperty("http://purl.org/phenoscape/vocab.owl#for_corpus_profile")
  val for_query_annotation = ObjectProperty("http://purl.org/phenoscape/vocab.owl#for_query_annotation")
  val for_corpus_annotation = ObjectProperty("http://purl.org/phenoscape/vocab.owl#for_corpus_annotation")
  val FoundAsMICA = Class("http://purl.org/phenoscape/vocab.owl#FoundAsMICA")

}