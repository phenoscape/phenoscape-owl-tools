package org.phenoscape.owl

import java.io.File
import java.util.UUID
import scala.Option.option2Iterable
import scala.collection.JavaConversions._
import scala.collection.mutable
import org.apache.commons.lang3.StringUtils
import org.jdom2.filter.ElementFilter
import org.jdom2.input.SAXBuilder
import org.jdom2.Element
import org.jdom2.Namespace
import org.phenoscape.owl.util.OBOUtil
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAnnotationSubject
import org.semanticweb.owlapi.model.OWLAnnotationValue
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLIndividual
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom
import org.semanticweb.owlapi.model.OWLQuantifiedObjectRestriction
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary
import org.semanticweb.owlapi.model.OWLOntology
import scala.io.Source
import org.semanticweb.owlapi.io.RDFXMLOntologyFormat
import org.apache.log4j.Logger

class PhenexToOWL extends OWLTask {

  val dcTermsNS = Namespace.getNamespace("http://purl.org/dc/terms/")
  val dwcNS = Namespace.getNamespace("http://rs.tdwg.org/dwc/terms/")
  val nexmlNS = Namespace.getNamespace("http://www.nexml.org/2009")
  val phenoNS = Namespace.getNamespace("http://www.bioontologies.org/obd/schema/pheno")
  val rdfsNS = Namespace.getNamespace("http://www.w3.org/2000/01/rdf-schema#")
  val phenoscapeComplement = IRI.create("http://purl.obolibrary.org/obo/PHENOSCAPE_complement_of")
  val towards = ObjectProperty(Vocab.TOWARDS)
  val hasPart = ObjectProperty(Vocab.HAS_PART)
  val bearerOf = ObjectProperty(Vocab.BEARER_OF)
  val inheres_in = ObjectProperty(Vocab.INHERES_IN)
  val exhibits = ObjectProperty(Vocab.EXHIBITS)
  val denotes = ObjectProperty(Vocab.DENOTES)
  val denotes_exhibiting = ObjectProperty(Vocab.DENOTES_EXHIBITING)
  val present = Class(Vocab.PRESENT)
  val absent = Class(Vocab.ABSENT)
  val eqCharacterToken = Class(Vocab.EQ_CHARACTER_TOKEN)
  val lacksAllPartsOfType = Class(Vocab.LACKS_ALL_PARTS_OF_TYPE)
  val organism = Class(Vocab.MULTI_CELLULAR_ORGANISM)
  val characterToOWLMap = mutable.Map[String, OWLNamedIndividual]()
  val stateToOWLMap = mutable.Map[String, OWLNamedIndividual]()
  val taxonOTUToOWLMap = mutable.Map[String, OWLNamedIndividual]()
  val taxonOTUToValidTaxonMap = mutable.Map[String, OWLNamedIndividual]()
  val stateToOWLPhenotypeMap = mutable.Map[String, mutable.Set[OWLClass]]()
  val manager = OWLManager.createOWLOntologyManager
  val ontology = manager.createOntology(IRI.create("http://example.org/" + UUID.randomUUID().toString()))
  var nexml: Element = null
  //nodeIncrementer = 0

  def convert(file: File): OWLOntology = {
    val builder = new SAXBuilder()
    val nexml = builder.build(file)
    convert(nexml.getRootElement(), file.getName())
    return ontology
  }

  def convert(root: Element, matrixLabel: String): Unit = {
    nexml = root
    val matrix = this.nextIndividual()
    addAnnotation(OWLRDFVocabulary.RDFS_LABEL.getIRI(), matrix.getIRI(), factory.getOWLLiteral(matrixLabel))
    this.addClass(matrix, this.factory.getOWLClass(Vocab.CHARACTER_STATE_DATA_MATRIX))
    val publicationNotes = getLiteralMetaValues(nexml, "description", dcTermsNS)
    publicationNotes.foreach(note => {
      val comment = factory.getOWLLiteral(note)
      addAnnotation(OWLRDFVocabulary.RDFS_COMMENT.getIRI(), matrix.getIRI(), comment)
    })
    val otus = nexml.getChild("otus", nexmlNS).getChildren("otu", nexmlNS)
    otus.foreach(translateOTU(_, matrix))
    val chars = nexml.getChild("characters", nexmlNS).getChild("format", nexmlNS).getChildren("char", nexmlNS)
    chars.foreach(translateCharacter(_, matrix))
    val matrixRows = nexml.getChild("characters", nexmlNS).getChild("matrix", nexmlNS).getChildren("row", nexmlNS)
    matrixRows.foreach(translateMatrixRow(_))
  }

  def translateOTU(otu: Element, matrix: OWLNamedIndividual): Unit = {
    val owlOTU = nextIndividual()
    val otuID = otu.getAttributeValue("id")
    taxonOTUToOWLMap.put(otuID, owlOTU)
    addPropertyAssertion(Vocab.HAS_TU, matrix, owlOTU)
    addClass(owlOTU, factory.getOWLClass(Vocab.TU))
    val label = otu.getAttributeValue("label")
    if (StringUtils.isNotBlank(label)) {
      addAnnotation(OWLRDFVocabulary.RDFS_LABEL.getIRI(), owlOTU.getIRI(), factory.getOWLLiteral(label))
    }
    val validTaxon = getResourceMetaValues(otu, "taxonID", dwcNS).headOption
    validTaxon.foreach(taxon => {
      val owlTaxon = factory.getOWLNamedIndividual(taxon)
      addPropertyAssertion(Vocab.HAS_EXTERNAL_REFERENCE, owlOTU, owlTaxon)
      taxonOTUToValidTaxonMap.put(otuID, owlTaxon)
    })
    val comments = getLiteralMetaValues(otu, "comment", rdfsNS)
    comments.foreach(c => addAnnotation(OWLRDFVocabulary.RDFS_COMMENT.getIRI(), owlOTU.getIRI(), factory.getOWLLiteral(c)))
    val specimenMetas = getResourceMetasForProperty(otu, "individualID", dwcNS)
    specimenMetas.foreach(translateSpecimen(_, owlOTU))
  }

  def translateSpecimen(specimen: Element, owlOTU: OWLNamedIndividual): Unit = {
    val owlSpecimen = factory.getOWLAnonymousIndividual()
    addPropertyAssertion(Vocab.INDIVIDUAL_ID, owlOTU, owlSpecimen)
    addClass(owlSpecimen, factory.getOWLClass(Vocab.SPECIMEN))
    val collectionIRI = getResourceMetaValues(specimen, "collectionID", dwcNS).headOption
    collectionIRI.foreach(iri => addPropertyAssertion(Vocab.SPECIMEN_TO_COLLECTION, owlSpecimen, factory.getOWLNamedIndividual(iri)))
    val catalogNumber = getLiteralMetaValues(specimen, "catalogNumber", dwcNS).headOption
    catalogNumber.foreach(num => {
      val property = factory.getOWLDataProperty(Vocab.SPECIMEN_TO_CATALOG_ID)
      val axiom = factory.getOWLDataPropertyAssertionAxiom(property, owlSpecimen, num)
      manager.addAxiom(ontology, axiom)
    })
  }

  def translateCharacter(character: Element, matrix: OWLNamedIndividual): Unit = {
    val owlCharacter = nextIndividual()
    val charID = character.getAttributeValue("id")
    characterToOWLMap.put(charID, owlCharacter)
    addPropertyAssertion(Vocab.HAS_CHARACTER, matrix, owlCharacter)
    addClass(owlCharacter, factory.getOWLClass(Vocab.STANDARD_CHARACTER))
    val label = character.getAttributeValue("label")
    if (StringUtils.isNotBlank(label)) {
      addAnnotation(OWLRDFVocabulary.RDFS_LABEL.getIRI(), owlCharacter.getIRI(), factory.getOWLLiteral(label))
    }
    val comments = getLiteralMetaValues(character, "comment", rdfsNS)
    comments.foreach(c => addAnnotation(OWLRDFVocabulary.RDFS_COMMENT.getIRI(), owlCharacter.getIRI(), factory.getOWLLiteral(c)))
    val statesBlockID = character.getAttributeValue("states")
    val statesBlock = getElementByID(statesBlockID)
    val states = statesBlock.getChildren("state", nexmlNS)
    states.foreach(translateState(_, owlCharacter, label))
  }

  def translateState(state: Element, owlCharacter: OWLNamedIndividual, characterLabel: String): Unit = {
    val owlState = nextIndividual()
    val stateID = state.getAttributeValue("id")
    stateToOWLMap.put(stateID, owlState)
    addClass(owlState, factory.getOWLClass(Vocab.STANDARD_STATE))
    addPropertyAssertion(Vocab.MAY_HAVE_STATE_VALUE, owlCharacter, owlState)
    val descBuffer = new StringBuffer()
    if (StringUtils.isNotBlank(characterLabel)) {
      descBuffer.append(characterLabel + ": ")
    }
    val label = state.getAttributeValue("label")
    if (StringUtils.isNotBlank(label)) {
      descBuffer.append(label)
      addAnnotation(OWLRDFVocabulary.RDFS_LABEL.getIRI(), owlState.getIRI(), factory.getOWLLiteral(label))
    }
    val completeDescription = descBuffer.toString() // for full-text indexing
    if (StringUtils.isNotBlank(completeDescription)) {
      addAnnotation(DublinCoreVocabulary.DESCRIPTION.getIRI(), owlState.getIRI(), factory.getOWLLiteral(completeDescription))
    }
    val comments = getLiteralMetaValues(state, "comment", rdfsNS)
    comments.map(c => addAnnotation(OWLRDFVocabulary.RDFS_COMMENT.getIRI(), owlState.getIRI(), factory.getOWLLiteral(c)))
    val phenotypes = state.getDescendants(new ElementFilter("phenotype_character", phenoNS)).iterator()
    phenotypes.foreach(translatePhenotype(_, stateID, owlState))
  }

  def translatePhenotype(phenotype: Element, stateID: String, owlState: OWLNamedIndividual): Unit = {
    val owlPhenotype = nextClass()
    stateToOWLPhenotypeMap.getOrElseUpdate(stateID, mutable.Set()).add(owlPhenotype)
    translatePhenotypeSemantics(phenotype, owlPhenotype, owlState)
    //TODO perhaps state should denote phenotype, not organism modify property chain
    manager.addAxiom(ontology, owlState Type (denotes only (exhibits some owlPhenotype)))
    val phenotypeInstance = nextIndividual()
    manager.addAxiom(ontology, owlState Fact (denotes_exhibiting, phenotypeInstance))
    manager.addAxiom(ontology, phenotypeInstance Type owlPhenotype)
  }

  def translatePhenotypeSemantics(phenotype: Element, owlPhenotype: OWLClass, owlState: OWLNamedIndividual): Unit = {
    val involved = mutable.Set[OWLClass]()
    val bearerElement = phenotype.getChild("bearer", phenoNS)
    val entityTerm = if (bearerElement != null) {
      val bearerType = bearerElement.getChild("typeref", phenoNS)
      if (bearerType != null) {
        val e = namedClassFromTyperef(bearerType)
        manager.addAxioms(ontology, AbsenceClassGenerator.generateAllAbsenceAxiomsForEntity(e))
        e
      } else { null }
    } else { null }
    val qualityElement = phenotype.getChild("quality", phenoNS)
    val qualityTerm = if (qualityElement != null) {
      val qualityType = qualityElement.getChild("typeref", phenoNS)
      if (qualityType != null) {
        namedClassFromTyperef(qualityType)
      } else { null }
    } else { null }
    val relatedEntityTerm = if (qualityElement != null) {
      val relatedEntityElement = qualityElement.getChild("related_entity", phenoNS)
      if (relatedEntityElement != null) {
        val relatedEntityType = relatedEntityElement.getChild("typeref", phenoNS)
        if (relatedEntityType != null) {
          val re = namedClassFromTyperef(relatedEntityType)
          manager.addAxioms(ontology, AbsenceClassGenerator.generateAllAbsenceAxiomsForEntity(re))
          re
        } else { null }
      } else { null }
    } else { null }
    val eq_phenotype = (entityTerm, qualityTerm, relatedEntityTerm) match {
      case (null, null, _) => null
      case (entity: OWLClass, null, null) => (present and (inheres_in some entity))
      case (entity: OWLClass, null, relatedEntity: OWLClass) => {
        logger.warn("Related entity with no quality.")
        (present and (inheres_in some entity))
      }
      case (entity: OWLClass, `absent`, null) => (lacksAllPartsOfType and (inheres_in some organism) and (towards value Individual(entity.getIRI())))
      case (entity: OWLClass, `lacksAllPartsOfType`, relatedEntity: OWLClass) => (lacksAllPartsOfType and (inheres_in some entity) and (towards value Individual(relatedEntity.getIRI())))
      case (null, quality: OWLClass, null) => quality
      case (null, quality: OWLClass, relatedEntity: OWLClass) => (quality and (towards some relatedEntity))
      case (entity: OWLClass, quality: OWLClass, null) => (quality and (inheres_in some entity))
      case (entity: OWLClass, quality: OWLClass, relatedEntity: OWLClass) => (quality and (inheres_in some entity) and (towards some relatedEntity))
      //TODO comparisons, etc.
    }
    if (eq_phenotype == null) {
      return 
    } else {
      manager.addAxiom(ontology, owlPhenotype SubClassOf eq_phenotype)
      manager.addAxiom(ontology, owlPhenotype SubClassOf eqCharacterToken)
    }
  }

  def translateMatrixRow(row: Element): Unit = {
    val otuID = row.getAttributeValue("otu")
    val owlOTU = taxonOTUToOWLMap(otuID)
    val cells = row.getChildren("cell", nexmlNS)
    cells.foreach(translateMatrixCell(_, otuID, owlOTU))
  }

  def translateMatrixCell(cell: Element, otuID: String, owlOTU: OWLNamedIndividual): Unit = {
    val owlCell = nextIndividual()
    addClass(owlCell, factory.getOWLClass(Vocab.STANDARD_CELL))
    val characterID = cell.getAttributeValue("char")
    val owlCharacter = characterToOWLMap(characterID)
    val stateID = cell.getAttributeValue("state")
    addPropertyAssertion(Vocab.BELONGS_TO_CHARACTER, owlCell, owlCharacter)
    addPropertyAssertion(Vocab.BELONGS_TO_TU, owlCell, owlOTU)
    // We are flattening uncertain/polymorphic states into multiple individual states related to the matrix cell
    val states = if (stateToOWLMap.containsKey(stateID)) {
      Set(stateID)
    } else {
      getElementByID(stateID).getChildren("member", nexmlNS).map(_.getAttributeValue("state"))
    }
    states.foreach(singleState => {
      stateToOWLMap.get(singleState).foreach(owlState => {
        addPropertyAssertion(Vocab.HAS_STATE, owlCell, owlState)
      })
      taxonOTUToValidTaxonMap.get(otuID).foreach(owlTaxon => {
        val phenotypes = stateToOWLPhenotypeMap.get(singleState).getOrElse(Set[OWLClass]())
        phenotypes.foreach(owlPhenotype => {
          val organism = nextIndividual()
          val phenotype = nextIndividual()
          addClass(phenotype, owlPhenotype)
          addPropertyAssertion(Vocab.HAS_MEMBER, owlTaxon, organism)
          addPropertyAssertion(Vocab.EXHIBITS, organism, phenotype)
        })
      })
    })
  }

  def classFromTyperef(typeref: Element): OWLClassExpression = {
    val genusID = typeref.getAttributeValue("about")
    val qualifiers = typeref.getChildren("qualifier", phenoNS)
    val genus = factory.getOWLClass(OBOUtil.iriForTermID(genusID))
    if (qualifiers.isEmpty()) {
      return genus
    } else {
      val operands: mutable.Set[OWLClassExpression] = mutable.Set(genus)
      operands.addAll(qualifiers.map(restrictionFromQualifier(_)))
      return factory.getOWLObjectIntersectionOf(operands)
    }
  }

  def namedClassFromTyperef(typeref: Element): OWLClass = {
    classFromTyperef(typeref) match {
      case named: OWLClass => named
      case expression => {
        val named = nextClass()
        manager.addAxiom(ontology, (named SubClassOf expression))
        named
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

  def addClass(individual: OWLIndividual, aClass: OWLClassExpression): Unit = {
    manager.addAxiom(ontology, factory.getOWLClassAssertionAxiom(aClass, individual))
  }

  def addAnnotation(property: IRI, subject: OWLAnnotationSubject, value: OWLAnnotationValue) {
    val annotationProperty = this.factory.getOWLAnnotationProperty(property)
    manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(annotationProperty))
    manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(annotationProperty, subject, value))
  }

  def addPropertyAssertion(propertyIRI: IRI, subject: OWLIndividual, value: OWLIndividual) {
    val property = factory.getOWLObjectProperty(propertyIRI)
    addPropertyAssertion(property, subject, value)
  }

  def addPropertyAssertion(property: OWLObjectPropertyExpression, subject: OWLIndividual, value: OWLIndividual) {
    if (!property.isAnonymous()) {
      manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(property.asOWLObjectProperty()))
    }
    manager.addAxiom(ontology, this.factory.getOWLObjectPropertyAssertionAxiom(property, subject, value))
  }

  def prefixForNamespace(element: Element, namespace: Namespace): String = {
    val prefix = element.getNamespacesInScope().filter(_.getURI() == namespace.getURI()).headOption.map(_.getPrefix() + ":")
    return prefix.getOrElse("")
  }

  def getLiteralMetaValues(element: Element, property: String, namespace: Namespace): Iterable[String] = {
    val allMetas = element.getChildren("meta", nexmlNS)
    val matchingMetas = allMetas.filter(meta => (meta.getAttributeValue("property") == (prefixForNamespace(meta, namespace) + property)))
    return (matchingMetas.map(_.getValue()) ++ matchingMetas.map(_.getAttributeValue("content"))).filter(StringUtils.isNotBlank(_))
  }

  def getResourceMetasForProperty(element: Element, property: String, propertyNamespace: Namespace): Iterable[Element] = {
    val allMetas = element.getChildren("meta", nexmlNS)
    return allMetas.filter(meta => (meta.getAttributeValue("rel") == (prefixForNamespace(meta, propertyNamespace) + property)))
  }

  def getResourceMetaValues(element: Element, property: String, namespace: Namespace): Iterable[IRI] = {
    val matchingMetas = getResourceMetasForProperty(element, property, namespace)
    return matchingMetas.map(_.getAttributeValue("href")).filter(StringUtils.isNotBlank(_)).map(IRI.create(_))
  }

  def getElementByID(id: String): Element = {
    return nexml.getDescendants(new ElementFilter()).iterator().filter(id == _.getAttributeValue("id")).next()
  }

  def instantiateClassAssertion(individual: OWLIndividual, aClass: OWLClassExpression, expandNamedClass: Boolean): Unit = {
    if (aClass.isInstanceOf[OWLClass]) {
      if (expandNamedClass) {
        val relatedClasses = mutable.Set[OWLClassExpression]()
        val equivClasses = ontology.getEquivalentClassesAxioms(aClass.asOWLClass()).map(_.getClassExpressionsMinus(aClass)).flatten
        relatedClasses.addAll(equivClasses)
        val superClasses = ontology.getSubClassAxiomsForSubClass(aClass.asOWLClass()).map(_.getSuperClass())
        relatedClasses.addAll(superClasses)
        for (expression <- relatedClasses) {
          if (expression.isInstanceOf[OWLObjectSomeValuesFrom]) {
            instantiateClassAssertion(individual, expression, false)
          }
        }
      } else {
        manager.addAxiom(this.ontology, factory.getOWLClassAssertionAxiom(aClass, individual))
      }
    } else if (aClass.isInstanceOf[OWLQuantifiedObjectRestriction]) { // either someValuesFrom or allValuesFrom
      val restriction = aClass.asInstanceOf[OWLQuantifiedObjectRestriction]
      val filler = restriction.getFiller()
      val property = restriction.getProperty()
      // need IRIs for individuals for type materialization
      val value = this.nextIndividual()
      addPropertyAssertion(property, individual, value)
      instantiateClassAssertion(value, filler, false)
    } else if (aClass.isInstanceOf[OWLObjectIntersectionOf]) {
      for (operand <- (aClass.asInstanceOf[OWLObjectIntersectionOf]).getOperands()) {
        instantiateClassAssertion(individual, operand, false)
      }
    } else {
      manager.addAxiom(this.ontology, this.factory.getOWLClassAssertionAxiom(aClass, individual))
    }
  }

}