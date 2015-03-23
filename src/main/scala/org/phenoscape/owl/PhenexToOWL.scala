package org.phenoscape.owl

import java.io.File
import java.util.UUID
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import org.jdom2.Element
import org.jdom2.Namespace
import org.jdom2.filter.ElementFilter
import org.jdom2.input.SAXBuilder
import org.phenoscape.owl.util.OntologyUtil
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary
import Vocab._
import org.semanticweb.owlapi.model.OWLClassExpression
import org.phenoscape.owl.util.OBOUtil
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import org.phenoscape.owl.util.ExpressionUtil
import org.semanticweb.owlapi.model.OWLObject
import org.phenoscape.owl.util.OntologyUtil.optionWithSet

object PhenexToOWL extends OWLTask {

  val dcTermsNS = Namespace.getNamespace("http://purl.org/dc/terms/")
  val dwcNS = Namespace.getNamespace("http://rs.tdwg.org/dwc/terms/")
  val nexmlNS = Namespace.getNamespace("http://www.nexml.org/2009")
  val phenoNS = Namespace.getNamespace("http://www.bioontologies.org/obd/schema/pheno")
  val rdfsNS = Namespace.getNamespace("http://www.w3.org/2000/01/rdf-schema#")
  val rdfsLabel = factory.getRDFSLabel
  val rdfsComment = factory.getRDFSComment
  val dcDescription = factory.getOWLAnnotationProperty(DublinCoreVocabulary.DESCRIPTION.getIRI)
  val phenoscapeComplement = IRI.create("http://purl.obolibrary.org/obo/PHENOSCAPE_complement_of")
  val manager = OWLManager.createOWLOntologyManager
  type LabelRenderer = OWLObject => String

  def convert(file: File, vocabulary: OWLOntology = manager.createOntology()): OWLOntology = {
    val labelRenderer = ExpressionUtil.createEntityRenderer(factory.getRDFSLabel, vocabulary)
    val doc = new SAXBuilder().build(file)
    val nexml = doc.getRootElement
    val matrix = OntologyUtil.nextIndividual
    val axioms = Set(
      matrix Annotation (rdfsLabel, file.getName),
      matrix Type CharacterStateDataMatrix)
    val descriptionAxiom = for { value <- getLiteralMetaValues(nexml, "description", dcTermsNS) }
      yield matrix Annotation (rdfsComment, value)
    val (otusAxioms, taxonOTUToOWLMap) = translateOTUs(nexml, matrix)
    val (charactersAxioms, characterToOWLMap) = translateCharacters(nexml, matrix, labelRenderer)
    val matrixAxioms = translateMatrixRows(nexml, matrix, taxonOTUToOWLMap, characterToOWLMap)
    val allAxioms = axioms ++ descriptionAxiom ++ otusAxioms ++ charactersAxioms ++ matrixAxioms
    manager.createOntology(allAxioms, IRI.create("http://example.org/" + UUID.randomUUID().toString()))
  }

  def translateMatrixRows(nexml: Element, matrix: OWLNamedIndividual, taxonOTUToOWLMap: Map[String, OWLNamedIndividual], characterToOWLMap: Map[String, OWLNamedIndividual]): Set[OWLAxiom] = {
    val axioms = nexml.getChild("characters", nexmlNS).getChild("matrix", nexmlNS).getChildren("row", nexmlNS).map(translateMatrixRow(_, matrix, taxonOTUToOWLMap, characterToOWLMap))
    axioms.flatten.toSet
  }

  def translateMatrixRow(row: Element, matrix: OWLNamedIndividual, taxonOTUToOWLMap: Map[String, OWLNamedIndividual], characterToOWLMap: Map[String, OWLNamedIndividual]): Set[OWLAxiom] = {
    val otuID = row.getAttributeValue("otu")
    val owlOTU = taxonOTUToOWLMap(otuID)
    val cells = row.getChildren("cell", nexmlNS)
    cells.flatMap(translateMatrixCell(_, otuID, owlOTU, characterToOWLMap)).toSet
  }

  def translateMatrixCell(cell: Element, otuID: String, owlOTU: OWLNamedIndividual, characterToOWLMap: Map[String, OWLNamedIndividual]): Set[OWLAxiom] = {
    val owlCell = OntologyUtil.nextIndividual()
    val characterID = cell.getAttributeValue("char")
    val owlCharacter = characterToOWLMap(characterID)
    val stateID = cell.getAttributeValue("state")
    val cellAxioms = Set(
      owlCell Type StandardCell,
      owlCell Fact (belongs_to_character, owlCharacter),
      owlCell Fact (belongs_to_TU, owlOTU))
    // We are flattening uncertain/polymorphic states into multiple individual states related to the matrix cell
    val states = if (characterToOWLMap.containsKey(stateID)) Set(stateID)
    else getElementByID(stateID, cell).getChildren("member", nexmlNS).map(_.getAttributeValue("state")).toSet
    val stateAxioms = for {
      singleState <- states
      owlState <- characterToOWLMap.get(singleState)
    } yield {
      val taxonStateAxiomOpt = for (owlTaxon <- taxonForOTU(getElementByID(otuID, cell))) yield owlTaxon Fact (exhibits_state, owlState)
      taxonStateAxiomOpt.toSet + (owlCell Fact (has_state, owlState))
    }
    cellAxioms ++ stateAxioms.flatten
  }

  def translateOTUs(nexml: Element, matrix: OWLNamedIndividual): (Set[OWLAxiom], Map[String, OWLNamedIndividual]) = {
    val translations = nexml.getChild("otus", nexmlNS).getChildren("otu", nexmlNS).map(translateOTU(_, matrix))
    val (axioms, owlOTUAssociations) = translations.unzip
    (axioms.flatten.toSet, owlOTUAssociations.toMap)
  }

  def translateOTU(otu: Element, matrix: OWLNamedIndividual): (Set[OWLAxiom], (String, OWLNamedIndividual)) = {
    val owlOTU = OntologyUtil.nextIndividual
    val otuID = otu.getAttributeValue("id")
    val otuLabel = optString(otu.getAttributeValue("label")).getOrElse("")
    val axioms = Set(
      matrix Fact (has_TU, owlOTU),
      owlOTU Type TU,
      owlOTU Annotation (rdfsLabel, otuLabel))
    val otuToValidTaxonOption = for { taxon <- taxonForOTU(otu) }
      yield owlOTU Fact (has_external_reference, taxon)
    val otuComments = for { comment <- getLiteralMetaValues(otu, "comment", rdfsNS) }
      yield owlOTU Annotation (rdfsComment, comment)
    val specimensAxioms = translateSpecimens(otu, owlOTU)
    (axioms ++ otuToValidTaxonOption ++ otuComments ++ specimensAxioms, otuID -> owlOTU)
  }

  def taxonForOTU(otu: Element): Option[OWLNamedIndividual] = {
    getResourceMetaValues(otu, "taxonID", dwcNS).headOption.map(Individual(_))
  }

  def translateSpecimens(otu: Element, owlOTU: OWLNamedIndividual): Set[OWLAxiom] = {
    val axioms = for {
      specimen <- getResourceMetasForProperty(otu, "individualID", dwcNS)
      axiom <- translateSpecimen(specimen, owlOTU)
    } yield axiom
    axioms.toSet
  }

  def translateSpecimen(specimenElement: Element, owlOTU: OWLNamedIndividual): Set[OWLAxiom] = {
    val owlSpecimen = Individual()
    val axioms = Set(
      owlOTU Fact (individual_id, owlSpecimen),
      owlSpecimen Type Specimen)
    val collectionAxiom = for { iri <- getResourceMetaValues(specimenElement, "collectionID", dwcNS) }
      yield owlSpecimen Fact (collectionID, Individual(iri))
    val catalogNumAxiom = for { value <- getLiteralMetaValues(specimenElement, "catalogNumber", dwcNS) }
      yield factory.getOWLDataPropertyAssertionAxiom(catalogNumber, owlSpecimen, value)
    axioms ++ collectionAxiom ++ catalogNumAxiom
  }

  def translateCharacters(nexml: Element, matrix: OWLNamedIndividual, labelRenderer: LabelRenderer): (Set[OWLAxiom], Map[String, OWLNamedIndividual]) = {
    val characterElements = nexml.getChild("characters", nexmlNS).getChild("format", nexmlNS).getChildren("char", nexmlNS)
    val charactersWithIndex = characterElements.zipWithIndex
    val translations = charactersWithIndex.map { case (characterElement, index) => translateCharacter(characterElement, index, matrix, labelRenderer) }
    val (axioms, nexmlToOWLMaps) = translations.unzip
    (axioms.flatten.toSet, nexmlToOWLMaps.flatten.toMap)
  }

  def translateCharacter(character: Element, index: Int, matrix: OWLNamedIndividual, labelRenderer: LabelRenderer): (Set[OWLAxiom], Map[String, OWLNamedIndividual]) = {
    val owlCharacter = OntologyUtil.nextIndividual
    val characterID = character.getAttributeValue("id")
    val characterLabel = optString(character.getAttributeValue("label")).getOrElse("")
    val axioms = Set(
      matrix Fact (has_character, owlCharacter),
      owlCharacter Type StandardCharacter,
      owlCharacter Annotation (list_index, factory.getOWLLiteral(index + 1)),
      owlCharacter Annotation (rdfsLabel, characterLabel))
    val characterComments = for { comment <- getLiteralMetaValues(character, "comment", rdfsNS) }
      yield owlCharacter Annotation (rdfsComment, comment)
    val statesBlockID = character.getAttributeValue("states")
    val statesBlock = getElementByID(statesBlockID, character)
    val (statesAxioms, stateToOWLMap) = translateStates(statesBlock, owlCharacter, characterLabel, labelRenderer)
    (axioms ++ characterComments ++ statesAxioms, stateToOWLMap + (characterID -> owlCharacter))
  }

  def translateStates(statesBlock: Element, owlCharacter: OWLNamedIndividual, characterLabel: String, labelRenderer: LabelRenderer): (Set[OWLAxiom], Map[String, OWLNamedIndividual]) = {
    val stateElements = statesBlock.getChildren("state", nexmlNS)
    val translations = stateElements.map(translateState(_, owlCharacter, characterLabel, labelRenderer))
    val (axioms, owlStateAssociations) = translations.unzip
    (axioms.flatten.toSet, owlStateAssociations.toMap)
  }

  def translateState(state: Element, owlCharacter: OWLNamedIndividual, characterLabel: String, labelRenderer: LabelRenderer): (Set[OWLAxiom], (String, OWLNamedIndividual)) = {
    val owlState = OntologyUtil.nextIndividual
    val stateID = state.getAttributeValue("id")
    val stateLabel = optString(state.getAttributeValue("label")).getOrElse("")
    val stateSymbol = optString(state.getAttributeValue("symbol")).getOrElse("<?>")
    val stateAxioms = Set(
      owlState Type StandardState,
      owlCharacter Fact (may_have_state_value, owlState),
      owlState Annotation (rdfsLabel, stateLabel),
      owlState Annotation (state_symbol, stateSymbol),
      owlState Annotation (dcDescription, s"$characterLabel: $stateLabel"))
    val stateComments = for { comment <- getLiteralMetaValues(state, "comment", rdfsNS) }
      yield owlState Annotation (rdfsComment, comment)
    val phenotypeAxioms = translatePhenotypes(state, owlState, labelRenderer)
    (stateAxioms ++ stateComments ++ phenotypeAxioms, stateID -> owlState)
  }

  def translatePhenotypes(state: Element, owlState: OWLNamedIndividual, labelRenderer: LabelRenderer): Set[OWLAxiom] = {
    val phenotypeElements = state.getDescendants(new ElementFilter("phenotype_character", phenoNS)).iterator
    phenotypeElements.flatMap(translatePhenotype(_, owlState, labelRenderer)).toSet
  }

  def translatePhenotype(phenotypeElement: Element, owlState: OWLNamedIndividual, labelRenderer: LabelRenderer): Set[OWLAxiom] = {
    val owlPhenotype = OntologyUtil.nextClass()
    val phenotypeAxioms = translatePhenotypeSemantics(phenotypeElement, owlPhenotype, labelRenderer)
    phenotypeAxioms + (owlState Annotation (describes_phenotype, owlPhenotype.getIRI))
  }

  def translatePhenotypeSemantics(phenotypeElement: Element, owlPhenotype: OWLClass, labelRenderer: LabelRenderer): Set[OWLAxiom] = {
    val (entityOption, entityAxioms) = optionWithSet(for {
      bearerElement <- Option(phenotypeElement.getChild("bearer", phenoNS))
      bearerType <- Option(bearerElement.getChild("typeref", phenoNS))
    } yield {
      val (entity, axioms) = namedClassFromTyperef(bearerType, labelRenderer)
      (entity,
        axioms ++
        AbsenceClassGenerator.generateAllAbsenceAxiomsForEntity(entity) +
        (owlPhenotype Annotation (entity_term, entity.getIRI)))
    })
    val qualityElementOption = Option(phenotypeElement.getChild("quality", phenoNS))
    val (qualityOption, qualityAxioms) = optionWithSet(for {
      qualityElement <- qualityElementOption
      qualityType <- Option(qualityElement.getChild("typeref", phenoNS))
    } yield {
      val (quality, axioms) = namedClassFromTyperef(qualityType, labelRenderer)
      (quality,
        axioms + (owlPhenotype Annotation (quality_term, quality.getIRI)))
    })
    val (relatedEntityOption, relatedEntityAxioms) = optionWithSet(for {
      qualityElement <- qualityElementOption
      relatedEntityElement <- Option(qualityElement.getChild("related_entity", phenoNS))
      relatedEntityType <- Option(relatedEntityElement.getChild("typeref", phenoNS))
    } yield {
      val (relatedEntity, axioms) = namedClassFromTyperef(relatedEntityType, labelRenderer)
      (relatedEntity,
        axioms ++
        AbsenceClassGenerator.generateAllAbsenceAxiomsForEntity(relatedEntity) +
        (owlPhenotype Annotation (related_entity_term, relatedEntity.getIRI)))
    })
    val eqPhenotypeOption = (entityOption, qualityOption, relatedEntityOption) match {
      case (None, None, _)            => None
      case (Some(entity), None, None) => Option(Present and (inheres_in some entity))
      case (Some(entity), None, Some(relatedEntity)) => {
        logger.warn("Related entity with no quality. Shouldn't be possible.")
        Option(Present and (inheres_in some entity))
      }
      case (Some(entity), Some(Absent), None)                             => Option(LacksAllPartsOfType and (inheres_in some MultiCellularOrganism) and (towards value Individual(entity.getIRI)))
      case (Some(entity), Some(LacksAllPartsOfType), Some(relatedEntity)) => Option(LacksAllPartsOfType and (inheres_in some entity) and (towards value Individual(relatedEntity.getIRI)))
      case (None, Some(quality), None)                                    => Option(quality)
      case (None, Some(quality), Some(relatedEntity))                     => Option(quality and (towards some relatedEntity))
      case (Some(entity), Some(quality), None)                            => Option(quality and (inheres_in some entity))
      case (Some(entity), Some(quality), Some(relatedEntity))             => Option(quality and (inheres_in some entity) and (towards some relatedEntity))
      //TODO comparisons, etc.
    }
    val phenotypeAxioms = eqPhenotypeOption.toSet.flatMap { eqPhenotype: OWLClassExpression =>
      Set(owlPhenotype SubClassOf eqPhenotype, owlPhenotype Annotation (rdfsLabel, labelRenderer(eqPhenotype)))
    }
    entityAxioms ++ qualityAxioms ++ relatedEntityAxioms ++ phenotypeAxioms
  }

  def classFromTyperef(typeref: Element): OWLClassExpression = {
    val genusID = typeref.getAttributeValue("about")
    val qualifiers = typeref.getChildren("qualifier", phenoNS)
    val genus = Class(OBOUtil.iriForTermID(genusID))
    if (qualifiers.isEmpty) genus
    else factory.getOWLObjectIntersectionOf(qualifiers.map(restrictionFromQualifier).toSet + genus)
  }

  def namedClassFromTyperef(typeref: Element, labelRenderer: LabelRenderer): (OWLClass, Set[OWLAxiom]) = {
    classFromTyperef(typeref) match {
      case named: OWLClass => (named, Set.empty)
      case expression => {
        val (named, axioms) = ExpressionUtil.nameForExpressionWithAxioms(expression)
        val label = s"[${labelRenderer(expression)}]"
        (named, axioms + (named Annotation (factory.getRDFSLabel, label)))
      }
    }
  }

  def restrictionFromQualifier(qualifier: Element): OWLClassExpression = {
    val propertyIRI = OBOUtil.iriForTermID(qualifier.getAttributeValue("relation"))
    val filler = classFromTyperef(qualifier.getChild("holds_in_relation_to", phenoNS).getChild("typeref", phenoNS))
    if (propertyIRI == phenoscapeComplement) {
      return factory.getOWLObjectComplementOf(filler)
    } else {
      val property = factory.getOWLObjectProperty(propertyIRI)
      return factory.getOWLObjectSomeValuesFrom(property, filler)
    }
  }

  def getLiteralMetaValues(element: Element, property: String, namespace: Namespace): Set[String] = {
    val values = for {
      meta <- element.getChildren("meta", nexmlNS)
      if meta.getAttributeValue("property") == (prefixForNamespace(meta, namespace) + property)
      value <- (optString(meta.getValue) ++ optString(meta.getAttributeValue("content")))
    } yield value
    values.toSet
  }

  def getResourceMetasForProperty(element: Element, property: String, propertyNamespace: Namespace): Iterable[Element] = {
    for {
      meta <- element.getChildren("meta", nexmlNS)
      if meta.getAttributeValue("rel") == (prefixForNamespace(meta, propertyNamespace) + property)
    } yield meta
  }

  def getResourceMetaValues(element: Element, property: String, namespace: Namespace): Set[IRI] = {
    val values = for {
      meta <- getResourceMetasForProperty(element, property, namespace)
      href <- optString(meta.getAttributeValue("href"))
    } yield IRI.create(href)
    values.toSet
  }

  def prefixForNamespace(element: Element, namespace: Namespace): String = {
    val prefixes = for {
      localNamespace <- element.getNamespacesInScope
      if localNamespace.getURI == namespace.getURI
    } yield localNamespace.getPrefix + ":"
    return prefixes.headOption.getOrElse("")
  }

  def optString(text: String): Option[String] = Option(StringUtils.stripToNull(text))

  def getElementByID(id: String, elementInDoc: Element): Element = {
    val elements = elementInDoc.getDocument.getRootElement.getDescendants(new ElementFilter()).iterator
    elements.find(_.getAttributeValue("id") == id).get
  }

}