package org.phenoscape.owl

import java.io.File

import org.apache.commons.lang3.StringUtils
import org.jdom2.filter.ElementFilter
import org.jdom2.input.SAXBuilder
import org.jdom2.{Element, Namespace}
import org.phenoscape.kb.ingest.util.{ExpressionUtil, OBOUtil, OntUtil}
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.ExpressionsUtil
import org.phenoscape.owl.util.OntologyUtil.optionWithSet
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary

import scala.collection.JavaConverters._
import scala.collection.mutable

object PhenexToOWL {

  val factory = OWLManager.getOWLDataFactory
  val dcTermsNS = Namespace.getNamespace("http://purl.org/dc/terms/")
  val dwcNS = Namespace.getNamespace("http://rs.tdwg.org/dwc/terms/")
  val nexmlNS = Namespace.getNamespace("http://www.nexml.org/2009")
  val phenoNS = Namespace.getNamespace("http://www.bioontologies.org/obd/schema/pheno")
  val rdfsNS = Namespace.getNamespace("http://www.w3.org/2000/01/rdf-schema#")
  val rdfsLabel = factory.getRDFSLabel
  val rdfsComment = factory.getRDFSComment
  val dcDescription = factory.getOWLAnnotationProperty(DublinCoreVocabulary.DESCRIPTION.getIRI)
  val dcSource = factory.getOWLAnnotationProperty(DublinCoreVocabulary.SOURCE.getIRI)
  val dcCreator = factory.getOWLAnnotationProperty(DublinCoreVocabulary.CREATOR.getIRI)

  val dcBibliographicCitation =
    factory.getOWLAnnotationProperty(IRI.create("http://rs.tdwg.org/dwc/terms/bibliographicCitation"))

  val phenoscapeComplement = IRI.create("http://purl.obolibrary.org/obo/PHENOSCAPE_complement_of")
  val manager = OWLManager.createOWLOntologyManager
  type LabelRenderer = OWLObject => String

  def convert(file: File, vocabulary: OWLOntology = manager.createOntology()): OWLOntology = {
    val labelRenderer = ExpressionsUtil.createEntityRenderer(factory.getRDFSLabel, vocabulary)
    val doc = new SAXBuilder().build(file)
    val nexml = doc.getRootElement
    val (matrix, axioms) = translateMatrix(nexml, file.getName)
    val descriptionAxiom = for {
      value <- getLiteralMetaValues(nexml, "description", dcTermsNS)
    } yield matrix Annotation (rdfsComment, value)
    val curatorAxioms = for {
      value <- getLiteralMetaValues(nexml, "creator", dcTermsNS)
    } yield matrix Annotation (dcCreator, value)
    val (otusAxioms, taxonOTUToOWLMap) = translateOTUs(nexml, matrix)
    val (charactersAxioms, characterToOWLMap) = translateCharacters(nexml, matrix, labelRenderer)
    val matrixAxioms = translateMatrixRows(nexml, matrix, taxonOTUToOWLMap, characterToOWLMap)
    val allAxioms = axioms ++ descriptionAxiom ++ otusAxioms ++ charactersAxioms ++ matrixAxioms
    manager.createOntology(allAxioms.asJava, OntUtil.nextIRI())
  }

  def translateMatrix(nexml: Element, fileName: String): (OWLNamedIndividual, Set[OWLAxiom]) = {
    val sourceAxioms = for {
      sourceMeta <- getResourceMetasForProperty(nexml, "source", dcTermsNS)
      matrix <- getLiteralMetaValues(sourceMeta, "identifier", dcTermsNS).map(Individual)
      citation <- getLiteralMetaValues(sourceMeta, "bibliographicCitation", dcTermsNS)
      label <- getLiteralMetaValues(sourceMeta, "title", dcTermsNS)
    } yield (
      matrix,
      Set[OWLAxiom](
        matrix Type CharacterStateDataMatrix,
        matrix Annotation (dcBibliographicCitation, citation),
        matrix Annotation (rdfsLabel, label)
      )
    )
    sourceAxioms.headOption.getOrElse {
      val matrix = OntUtil.nextIndividual()
      (
        matrix,
        Set[OWLAxiom](
          matrix Type CharacterStateDataMatrix,
          matrix Annotation (dcBibliographicCitation, "<missing citation>"),
          matrix Annotation (rdfsLabel, fileName)
        )
      )
    }
  }

  def translateMatrixRows(
    nexml: Element,
    matrix: OWLNamedIndividual,
    taxonOTUToOWLMap: Map[String, OWLNamedIndividual],
    characterToOWLMap: Map[String, OWLNamedIndividual]
  ): Set[OWLAxiom] = {
    val axioms = nexml
      .getChild("characters", nexmlNS)
      .getChild("matrix", nexmlNS)
      .getChildren("row", nexmlNS)
      .asScala
      .map(translateMatrixRow(_, matrix, taxonOTUToOWLMap, characterToOWLMap))
    axioms.flatten.toSet
  }

  def translateMatrixRow(
    row: Element,
    matrix: OWLNamedIndividual,
    taxonOTUToOWLMap: Map[String, OWLNamedIndividual],
    characterToOWLMap: Map[String, OWLNamedIndividual]
  ): Set[OWLAxiom] = {
    val otuID = row.getAttributeValue("otu")
    val owlOTU = taxonOTUToOWLMap(otuID)
    val cells = row.getChildren("cell", nexmlNS).asScala
    cells.flatMap(translateMatrixCell(_, otuID, owlOTU, characterToOWLMap)).toSet
  }

  def translateMatrixCell(
    cell: Element,
    otuID: String,
    owlOTU: OWLNamedIndividual,
    characterToOWLMap: Map[String, OWLNamedIndividual]
  ): Set[OWLAxiom] = {
    val owlCell = OntUtil.nextIndividual()
    val characterID = cell.getAttributeValue("char")
    val owlCharacter = characterToOWLMap(characterID)
    val stateID = cell.getAttributeValue("state")
    val cellAxioms = Set[OWLAxiom](
      owlCell Type StandardCell,
      owlCell Fact (belongs_to_character, owlCharacter),
      owlCell Fact (belongs_to_TU, owlOTU)
    )
    // We are flattening uncertain/polymorphic states into multiple individual states related to the matrix cell
    val states =
      if (characterToOWLMap.contains(stateID)) Set(stateID)
      else getElementByID(stateID, cell).getChildren("member", nexmlNS).asScala.map(_.getAttributeValue("state")).toSet
    val stateAxioms = for {
      singleState <- states
      owlState <- characterToOWLMap.get(singleState)
    } yield {
      val taxonStateAxiomOpt =
        for (owlTaxon <- taxonForOTU(getElementByID(otuID, cell))) yield owlTaxon Fact (exhibits_state, owlState)
      taxonStateAxiomOpt.toSet + (owlCell Fact (has_state, owlState))
    }
    cellAxioms ++ stateAxioms.flatten
  }

  def translateOTUs(nexml: Element, matrix: OWLNamedIndividual): (Set[OWLAxiom], Map[String, OWLNamedIndividual]) = {
    val translations = nexml.getChild("otus", nexmlNS).getChildren("otu", nexmlNS).asScala.map(translateOTU(_, matrix))
    val (axioms, owlOTUAssociations) = translations.unzip
    (axioms.flatten.toSet, owlOTUAssociations.toMap)
  }

  def translateOTU(otu: Element, matrix: OWLNamedIndividual): (Set[OWLAxiom], (String, OWLNamedIndividual)) = {
    val owlOTU = OntUtil.nextIndividual
    val otuID = otu.getAttributeValue("id")
    val otuLabel = optString(otu.getAttributeValue("label")).getOrElse("")
    val axioms = Set(matrix Fact (has_TU, owlOTU), owlOTU Type TU, owlOTU Annotation (rdfsLabel, otuLabel))
    val otuToValidTaxonOption = for { taxon <- taxonForOTU(otu) } yield owlOTU Fact (has_external_reference, taxon)
    val otuComments = for {
      comment <- getLiteralMetaValues(otu, "comment", rdfsNS)
    } yield owlOTU Annotation (rdfsComment, comment)
    val specimensAxioms = translateSpecimens(otu, owlOTU)
    (axioms ++ otuToValidTaxonOption ++ otuComments ++ specimensAxioms, otuID -> owlOTU)
  }

  def taxonForOTU(otu: Element): Option[OWLNamedIndividual] =
    getResourceMetaValues(otu, "taxonID", dwcNS).headOption.map(Individual)

  def translateSpecimens(otu: Element, owlOTU: OWLNamedIndividual): Set[OWLAxiom] = {
    val axioms = for {
      specimen <- getResourceMetasForProperty(otu, "individualID", dwcNS)
      axiom <- translateSpecimen(specimen, owlOTU)
    } yield axiom
    axioms.toSet
  }

  def translateSpecimen(specimenElement: Element, owlOTU: OWLNamedIndividual): Set[OWLAxiom] = {
    val owlSpecimen = Individual()
    val axioms = Set[OWLAxiom](owlOTU Fact (individual_id, owlSpecimen), owlSpecimen Type Specimen)
    val collectionAxiom = for {
      iri <- getResourceMetaValues(specimenElement, "collectionID", dwcNS)
    } yield owlSpecimen Fact (collectionID, Individual(
      iri
    ))
    val catalogNumAxiom =
      for { value <- getLiteralMetaValues(specimenElement, "catalogNumber", dwcNS) } yield factory
        .getOWLDataPropertyAssertionAxiom(catalogNumber, owlSpecimen, value)
    axioms ++ collectionAxiom ++ catalogNumAxiom
  }

  def translateCharacters(
    nexml: Element,
    matrix: OWLNamedIndividual,
    labelRenderer: LabelRenderer
  ): (Set[OWLAxiom], Map[String, OWLNamedIndividual]) = {
    val characterElements =
      nexml.getChild("characters", nexmlNS).getChild("format", nexmlNS).getChildren("char", nexmlNS).asScala
    val charactersWithIndex = characterElements.zipWithIndex
    val translations = charactersWithIndex.map {
      case (characterElement, index) => translateCharacter(characterElement, index, matrix, labelRenderer)
    }
    val (axioms, nexmlToOWLMaps) = translations.unzip
    (axioms.flatten.toSet, nexmlToOWLMaps.flatten.toMap)
  }

  def translateCharacter(
    character: Element,
    index: Int,
    matrix: OWLNamedIndividual,
    labelRenderer: LabelRenderer
  ): (Set[OWLAxiom], Map[String, OWLNamedIndividual]) = {
    val owlCharacter = OntUtil.nextIndividual
    val characterID = character.getAttributeValue("id")
    val characterLabel = optString(character.getAttributeValue("label")).getOrElse("")
    val axioms = Set(
      matrix Fact (has_character, owlCharacter),
      owlCharacter Type StandardCharacter,
      owlCharacter Annotation (list_index, factory.getOWLLiteral(index + 1)),
      owlCharacter Annotation (rdfsLabel, characterLabel)
    )
    val characterComments = for {
      comment <- getLiteralMetaValues(character, "comment", rdfsNS)
    } yield owlCharacter Annotation (rdfsComment, comment)
    val statesBlockID = character.getAttributeValue("states")
    val statesBlock = getElementByID(statesBlockID, character)
    val (statesAxioms, stateToOWLMap) = translateStates(statesBlock, owlCharacter, characterLabel, labelRenderer)
    (axioms ++ characterComments ++ statesAxioms, stateToOWLMap + (characterID -> owlCharacter))
  }

  def translateStates(
    statesBlock: Element,
    owlCharacter: OWLNamedIndividual,
    characterLabel: String,
    labelRenderer: LabelRenderer
  ): (Set[OWLAxiom], Map[String, OWLNamedIndividual]) = {
    val stateElements = statesBlock.getChildren("state", nexmlNS).asScala
    val translations = stateElements.map(translateState(_, owlCharacter, characterLabel, labelRenderer))
    val (axioms, owlStateAssociations) = translations.unzip
    (axioms.flatten.toSet, owlStateAssociations.toMap)
  }

  def translateState(
    state: Element,
    owlCharacter: OWLNamedIndividual,
    characterLabel: String,
    labelRenderer: LabelRenderer
  ): (Set[OWLAxiom], (String, OWLNamedIndividual)) = {
    val owlState = OntUtil.nextIndividual
    val stateID = state.getAttributeValue("id")
    val stateLabel = optString(state.getAttributeValue("label")).getOrElse("")
    val stateSymbol = optString(state.getAttributeValue("symbol")).getOrElse("<?>")
    val stateDescription = s"$characterLabel: $stateLabel"
    val stateAxioms = Set(
      owlState Type StandardState,
      owlCharacter Fact (may_have_state_value, owlState),
      owlState Annotation (rdfsLabel, stateLabel),
      owlState Annotation (state_symbol, stateSymbol),
      owlState Annotation (dcDescription, stateDescription)
    )
    val stateComments = for {
      comment <- getLiteralMetaValues(state, "comment", rdfsNS)
    } yield owlState Annotation (rdfsComment, comment)
    val phenotypeAxioms = translatePhenotypes(state, owlState, labelRenderer)
    (stateAxioms ++ stateComments ++ phenotypeAxioms, stateID -> owlState)
  }

  def translatePhenotypes(state: Element, owlState: OWLNamedIndividual, labelRenderer: LabelRenderer): Set[OWLAxiom] = {
    val phenotypeElements =
      state.getDescendants(new ElementFilter("phenotype_character", phenoNS)).iterator.asScala.toSet
    val (phenotypeOptions, phenotypeAxiomSets) =
      phenotypeElements.map(phenotypeElement => translatePhenotypeSemantics(phenotypeElement, labelRenderer)).unzip
    val linksToPhenotypes = phenotypeOptions.flatten.map(owlState Annotation (describes_phenotype, _))
    phenotypeAxiomSets.flatten ++ linksToPhenotypes
  }

  def translatePhenotypeSemantics(
    phenotypeElement: Element,
    labelRenderer: LabelRenderer
  ): (Option[OWLClass], Set[OWLAxiom]) = {
    val annotations = mutable.Set.empty[OWLAnnotation]
    val (entityAndLabelOption, entityAxioms) = optionWithSet(
      for {
        bearerElement <- Option(phenotypeElement.getChild("bearer", phenoNS))
        bearerType <- Option(bearerElement.getChild("typeref", phenoNS))
      } yield {
        val (entity, entityLabel, axioms) = namedClassFromTyperef(bearerType, labelRenderer)
        annotations += factory.getOWLAnnotation(entity_term, entity.getIRI)
        ((entity, entityLabel), axioms ++ AbsenceClassGenerator.generateAllAbsenceAxiomsForEntity(entity))
      }
    )
    val entityLabel = entityAndLabelOption.map(_._2).getOrElse("")
    val qualityElementOption = Option(phenotypeElement.getChild("quality", phenoNS))
    val (qualityAndLabelOption, qualityAxioms) = optionWithSet(
      for {
        qualityElement <- qualityElementOption
        qualityType <- Option(qualityElement.getChild("typeref", phenoNS))
      } yield {
        val (quality, qualityLabel, axioms) = namedClassFromTyperef(qualityType, labelRenderer)
        annotations += factory.getOWLAnnotation(quality_term, quality.getIRI)
        ((quality, qualityLabel), axioms)
      }
    )
    var qualityLabel = qualityAndLabelOption.map(_._2).getOrElse("")
    val (relatedEntityAndLabelOption, relatedEntityAxioms) = optionWithSet(
      for {
        qualityElement <- qualityElementOption
        relatedEntityElement <- Option(qualityElement.getChild("related_entity", phenoNS))
        relatedEntityType <- Option(relatedEntityElement.getChild("typeref", phenoNS))
      } yield {
        val (relatedEntity, relatedEntityLabel, axioms) = namedClassFromTyperef(relatedEntityType, labelRenderer)
        annotations += factory.getOWLAnnotation(related_entity_term, relatedEntity.getIRI)
        (
          (relatedEntity, relatedEntityLabel),
          axioms ++ AbsenceClassGenerator.generateAllAbsenceAxiomsForEntity(relatedEntity)
        )
      }
    )
    val relatedEntityLabel = relatedEntityAndLabelOption.map(_._2).map(re => s" $re").getOrElse("")
    val eqPhenotypeOption =
      (entityAndLabelOption.map(_._1), qualityAndLabelOption.map(_._1), relatedEntityAndLabelOption.map(_._1)) match {
        case (None, None, _) => None
        case (Some(entity), None, None) =>
          qualityLabel = "present"
          Option(has_part some (Present and (inheres_in some entity)))
        case (Some(entity), None, Some(_)) =>
          scribe.warn("Related entity with no quality. Shouldn't be possible.")
          qualityLabel = "present"
          Option(has_part some (Present and (inheres_in some entity)))
        case (Some(entity), Some(Absent), None) =>
          Option(
            (has_part some (LacksAllPartsOfType and (inheres_in some MultiCellularOrganism) and (towards value Individual(
              entity.getIRI
            ))))
              and
                (phenotype_of some entity)
          )
        case (Some(entity), Some(LacksAllPartsOfType), Some(relatedEntity)) =>
          Option(
            (has_part some (LacksAllPartsOfType and (inheres_in some entity) and (towards value Individual(
              relatedEntity.getIRI
            ))))
              and
                (phenotype_of some relatedEntity)
          )
        case (None, Some(quality), None) => Option(has_part some quality)
        case (None, Some(quality), Some(relatedEntity)) =>
          Option(has_part some (quality and (towards some relatedEntity)))
        case (Some(entity), Some(quality), None) => Option(has_part some (quality and (inheres_in some entity)))
        case (Some(entity), Some(quality), Some(relatedEntity)) =>
          Option(has_part some (quality and (inheres_in some entity) and (towards some relatedEntity)))
        //TODO comparisons, etc.
      }
    val phenotypeLabel = s"$entityLabel $qualityLabel$relatedEntityLabel"
    val (phenotypeClass, phenotypeAxioms) = optionWithSet(
      eqPhenotypeOption.map(ExpressionUtil.nameForExpressionWithAxioms)
    )
    annotations += factory.getOWLAnnotation(RDFSLabel, factory.getOWLLiteral(phenotypeLabel))
    val annotationAxioms =
      phenotypeClass.map(term => annotations.map(factory.getOWLAnnotationAssertionAxiom(term.getIRI, _))).toSet.flatten
    (phenotypeClass, entityAxioms ++ qualityAxioms ++ relatedEntityAxioms ++ phenotypeAxioms ++ annotationAxioms)
  }

  def classFromTyperef(typeref: Element): OWLClassExpression = {
    val genusID = typeref.getAttributeValue("about")
    val qualifiers = typeref.getChildren("qualifier", phenoNS).asScala
    val genus = Class(OBOUtil.iriForTermID(genusID))
    if (qualifiers.isEmpty) genus
    else factory.getOWLObjectIntersectionOf((qualifiers.map(restrictionFromQualifier).toSet + genus).asJava)
  }

  def namedClassFromTyperef(typeref: Element, labelRenderer: LabelRenderer): (OWLClass, String, Set[OWLAxiom]) =
    classFromTyperef(typeref) match {
      case named: OWLClass => (named, labelRenderer(named), Set.empty)
      case expression =>
        val (named, axioms) = ExpressionUtil.nameForSubClassWithAxioms(expression)
        val expressionLabel = labelRenderer(expression)
        (named, expressionLabel, axioms + (named Annotation (rdfsLabel, expressionLabel)))
    }

  def restrictionFromQualifier(qualifier: Element): OWLClassExpression = {
    val propertyIRI = OBOUtil.iriForTermID(qualifier.getAttributeValue("relation"))
    val filler = classFromTyperef(qualifier.getChild("holds_in_relation_to", phenoNS).getChild("typeref", phenoNS))
    if (propertyIRI == phenoscapeComplement) factory.getOWLObjectComplementOf(filler)
    else {
      val property = factory.getOWLObjectProperty(propertyIRI)
      factory.getOWLObjectSomeValuesFrom(property, filler)
    }
  }

  def getLiteralMetaValues(element: Element, property: String, namespace: Namespace): Set[String] = {
    val values = for {
      meta <- element.getChildren("meta", nexmlNS).asScala
      if meta.getAttributeValue("property") == (prefixForNamespace(meta, namespace) + property)
      value <- optString(meta.getValue) ++ optString(meta.getAttributeValue("content"))
    } yield value
    values.toSet
  }

  def getResourceMetasForProperty(element: Element, property: String, propertyNamespace: Namespace): Iterable[Element] =
    for {
      meta <- element.getChildren("meta", nexmlNS).asScala
      if meta.getAttributeValue("rel") == (prefixForNamespace(meta, propertyNamespace) + property)
    } yield meta

  def getResourceMetaValues(element: Element, property: String, namespace: Namespace): Set[IRI] = {
    val values = for {
      meta <- getResourceMetasForProperty(element, property, namespace)
      href <- optString(meta.getAttributeValue("href"))
    } yield IRI.create(href)
    values.toSet
  }

  def prefixForNamespace(element: Element, namespace: Namespace): String = {
    val prefixes = for {
      localNamespace <- element.getNamespacesInScope.asScala
      if localNamespace.getURI == namespace.getURI
    } yield localNamespace.getPrefix + ":"
    prefixes.headOption.getOrElse("")
  }

  def optString(text: String): Option[String] = Option(StringUtils.stripToNull(text))

  def getElementByID(id: String, elementInDoc: Element): Element = {
    val elements = elementInDoc.getDocument.getRootElement.getDescendants(new ElementFilter()).iterator.asScala
    elements.find(_.getAttributeValue("id") == id).get
  }

}
