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

object PhenexToOWL extends OWLTask {

	val dcTermsNS = Namespace.getNamespace("http://purl.org/dc/terms/");
	val dwcNS = Namespace.getNamespace("http://rs.tdwg.org/dwc/terms/");
	val nexmlNS = Namespace.getNamespace("http://www.nexml.org/2009");
	val phenoNS = Namespace.getNamespace("http://www.bioontologies.org/obd/schema/pheno");
	val rdfsNS = Namespace.getNamespace("http://www.w3.org/2000/01/rdf-schema#");
	var uuid: String = UUID.randomUUID().toString();
	var nodeIncrementer: Int = 0;
	val characterToOWLMap = mutable.Map[String, OWLNamedIndividual]();
	val stateToOWLMap = mutable.Map[String, OWLNamedIndividual]();
	val taxonOTUToOWLMap = mutable.Map[String, OWLNamedIndividual]();
	val taxonOTUToValidTaxonMap = mutable.Map[String, OWLNamedIndividual]();
	val stateToOWLPhenotypeMap = mutable.Map[String, mutable.Set[OWLClass]]();
	var manager = OWLManager.createOWLOntologyManager();
	val factory = OWLManager.getOWLDataFactory();
	var ontology = manager.createOntology();
	var nexml: Element = null;

	def main(args: Array[String]): Unit = {
			val builder = new SAXBuilder();
			val nexml = builder.build(new File(args(0)));
			convert(nexml.getRootElement());
			manager.saveOntology(ontology, IRI.create(new File(args(1))));
	}

	def convert(root: Element): Unit = {
			this.nexml = root;
			val matrix = this.nextIndividual();
			this.addClass(matrix, this.factory.getOWLClass(Vocab.CHARACTER_STATE_DATA_MATRIX));
			val publicationNotes = getLiteralMetaValues(nexml, "description", dcTermsNS);
			publicationNotes.map(note => {
				val comment = factory.getOWLLiteral(note);
				addAnnotation(OWLRDFVocabulary.RDFS_COMMENT.getIRI(), matrix.getIRI(), comment);
			});
			val otus = nexml.getChild("otus", nexmlNS).getChildren("otu", nexmlNS);
			otus.foreach(translateOTU(_, matrix));
			val chars = nexml.getChild("characters", nexmlNS).getChild("format", nexmlNS).getChildren("char", nexmlNS);
			chars.foreach(translateCharacter(_, matrix));
			val matrixRows = nexml.getChild("characters", nexmlNS).getChild("matrix", nexmlNS).getChildren("row", nexmlNS);
			matrixRows.foreach(translateMatrixRow(_));
	}

	def translateOTU(otu: Element, matrix: OWLNamedIndividual): Unit = {
			val owlOTU = nextIndividual();
			val otuID = otu.getAttributeValue("id");
			taxonOTUToOWLMap.put(otuID, owlOTU);
			addPropertyAssertion(Vocab.HAS_TU, matrix, owlOTU);
			addClass(owlOTU, factory.getOWLClass(Vocab.TU));
			val label = otu.getAttributeValue("label");
			if (StringUtils.isNotBlank(label)) {
				addAnnotation(OWLRDFVocabulary.RDFS_LABEL.getIRI(), owlOTU.getIRI(), factory.getOWLLiteral(label));
			}
			val validTaxon = getResourceMetaValues(otu, "taxonID", dwcNS).headOption;
			validTaxon.foreach(taxon => {
				val owlTaxon = factory.getOWLNamedIndividual(taxon);
				addPropertyAssertion(Vocab.HAS_EXTERNAL_REFERENCE, owlOTU, owlTaxon);
				taxonOTUToValidTaxonMap.put(otuID, owlTaxon);
			});
			val comments = getLiteralMetaValues(otu, "comment", rdfsNS);
			comments.foreach(c => addAnnotation(OWLRDFVocabulary.RDFS_COMMENT.getIRI(), owlOTU.getIRI(), factory.getOWLLiteral(c)));
			val specimenMetas = getResourceMetasForProperty(otu, "individualID", dwcNS); 
			specimenMetas.foreach(translateSpecimen(_, owlOTU));
	}

	def translateSpecimen(specimen: Element, owlOTU: OWLNamedIndividual): Unit = {
			val owlSpecimen = factory.getOWLAnonymousIndividual();
			addPropertyAssertion(Vocab.INDIVIDUAL_ID, owlOTU, owlSpecimen);
			addClass(owlSpecimen, factory.getOWLClass(Vocab.SPECIMEN));
			val collectionIRI = getResourceMetaValues(specimen, "collectionID", dwcNS).headOption;
			collectionIRI.foreach(iri => addPropertyAssertion(Vocab.SPECIMEN_TO_COLLECTION, owlSpecimen, factory.getOWLNamedIndividual(iri)));
			val catalogNumber = getLiteralMetaValues(specimen, "catalogNumber", dwcNS).headOption;
			catalogNumber.foreach(num => {
				val property = factory.getOWLDataProperty(Vocab.SPECIMEN_TO_CATALOG_ID);
				val axiom = factory.getOWLDataPropertyAssertionAxiom(property, owlSpecimen, num);
				manager.addAxiom(ontology, axiom);
			});
	}

	def translateCharacter(character: Element, matrix: OWLNamedIndividual): Unit = {
			val owlCharacter = nextIndividual();
			val charID = character.getAttributeValue("id");
			characterToOWLMap.put(charID, owlCharacter);
			addPropertyAssertion(Vocab.HAS_CHARACTER, matrix, owlCharacter);
			addClass(owlCharacter, factory.getOWLClass(Vocab.STANDARD_CHARACTER));
			val label = character.getAttributeValue("label");
			if (StringUtils.isNotBlank(label)) {
				addAnnotation(OWLRDFVocabulary.RDFS_LABEL.getIRI(), owlCharacter.getIRI(), factory.getOWLLiteral(label));
			}
			val comments = getLiteralMetaValues(character, "comment", rdfsNS);
			comments.foreach(c => addAnnotation(OWLRDFVocabulary.RDFS_COMMENT.getIRI(), owlCharacter.getIRI(), factory.getOWLLiteral(c)));
			val statesBlockID = character.getAttributeValue("states");
			val statesBlock = getElementByID(statesBlockID);
			val states = statesBlock.getChildren("state", nexmlNS);
			states.foreach(translateState(_, owlCharacter, label));
	}

	def translateState(state: Element, owlCharacter: OWLNamedIndividual, characterLabel: String): Unit = {
			val owlState = nextIndividual();
			val stateID = state.getAttributeValue("id");
			stateToOWLMap.put(stateID, owlState);
			addClass(owlState, factory.getOWLClass(Vocab.STANDARD_STATE));
			addPropertyAssertion(Vocab.MAY_HAVE_STATE_VALUE, owlCharacter, owlState);
			val descBuffer = new StringBuffer();
			if (StringUtils.isNotBlank(characterLabel)) {
				descBuffer.append(characterLabel + ": ");
			}
			val label = state.getAttributeValue("label");
			if (StringUtils.isNotBlank(label)) {
				descBuffer.append(label);
				addAnnotation(OWLRDFVocabulary.RDFS_LABEL.getIRI(), owlState.getIRI(), factory.getOWLLiteral(label));
			}
			val completeDescription = descBuffer.toString(); // for full-text indexing
			if (StringUtils.isNotBlank(completeDescription)) {
				addAnnotation(DublinCoreVocabulary.DESCRIPTION.getIRI(), owlState.getIRI(), factory.getOWLLiteral(completeDescription));
			}
			val comments = getLiteralMetaValues(state, "comment", rdfsNS);
			comments.map(c => addAnnotation(OWLRDFVocabulary.RDFS_COMMENT.getIRI(), owlState.getIRI(), factory.getOWLLiteral(c)));
			val phenotypes = state.getDescendants(new ElementFilter("phenotype_character", phenoNS)).iterator();
			phenotypes.foreach(translatePhenotype(_, stateID, owlState));
	}

	def translatePhenotype(phenotype: Element, stateID: String, owlState: OWLNamedIndividual): Unit = {
			val owlPhenotype = nextClass();
			stateToOWLPhenotypeMap.getOrElseUpdate(stateID, mutable.Set()).add(owlPhenotype);
			translatePhenotypeSemantics(phenotype, owlPhenotype);
			val denotes = factory.getOWLObjectProperty(Vocab.DENOTES);
			val denotesExemplar = factory.getOWLObjectProperty(Vocab.DENOTES_EXEMPLAR);
			val denotesOnlyPhenotype = factory.getOWLObjectAllValuesFrom(denotes, owlPhenotype);
			manager.addAxiom(ontology, factory.getOWLClassAssertionAxiom(denotesOnlyPhenotype, owlState));
			val exemplar = nextIndividual();
			manager.addAxiom(ontology, factory.getOWLObjectPropertyAssertionAxiom(denotesExemplar, owlState, exemplar));
			manager.addAxiom(ontology, factory.getOWLClassAssertionAxiom(owlPhenotype, exemplar));
			instantiateClassAssertion(exemplar, owlPhenotype, true);
	}

	def translatePhenotypeSemantics(phenotype: Element, owlPhenotype: OWLClass): Unit = {
			val bearer = phenotype.getChild("bearer", phenoNS);
			val entityClass = if (bearer != null) {
				val bearerType = bearer.getChild("typeref", phenoNS);
				if (bearerType != null) {
					classFromTyperef(bearerType);
				} else { null; }
			} else { null; }
			val quality = phenotype.getChild("quality", phenoNS);
			val qualityClass = if (quality != null) {
				val qualityType = quality.getChild("typeref", phenoNS);
				val primaryQualityClass = if (qualityType != null) {
					classFromTyperef(qualityType);
				} else { null; }
				val relatedEntity = quality.getChild("related_entity", phenoNS);
				val relatedEntityClass = if (relatedEntity != null) {
					val relatedEntityType = relatedEntity.getChild("typeref", phenoNS);
					if (relatedEntityType != null) {
						classFromTyperef(relatedEntityType);
					} else { null; }
				} else { null; }
				if (relatedEntityClass != null) {
					val towards = factory.getOWLObjectProperty(Vocab.TOWARDS);
					factory.getOWLObjectIntersectionOf(primaryQualityClass, this.factory.getOWLObjectSomeValuesFrom(towards, relatedEntityClass));
				} else {
					primaryQualityClass;
				}
			} else { null; }
			if (entityClass == null) {
				return;
			}
			val eq = if (qualityClass == null) {
				entityClass;
			} else {
				val bearerOf = factory.getOWLObjectProperty(Vocab.BEARER_OF);
				factory.getOWLObjectIntersectionOf(entityClass, factory.getOWLObjectSomeValuesFrom(bearerOf, qualityClass));
			}
			val hasPart = factory.getOWLObjectProperty(Vocab.HAS_PART);
			val hasPartSomeEQ = factory.getOWLObjectSomeValuesFrom(hasPart, eq);
			manager.addAxiom(ontology, factory.getOWLSubClassOfAxiom(owlPhenotype, hasPartSomeEQ));
			eq.getClassesInSignature().foreach(createRestrictions(_));
	}

	def translateMatrixRow(row: Element): Unit = {
			val otuID = row.getAttributeValue("otu");
			val owlOTU = taxonOTUToOWLMap(otuID);
			val cells = row.getChildren("cell", nexmlNS);
			cells.foreach(translateMatrixCell(_, otuID, owlOTU));
	}

	def translateMatrixCell(cell: Element, otuID: String, owlOTU: OWLNamedIndividual): Unit = {
			val owlCell = nextIndividual();
			addClass(owlCell, factory.getOWLClass(Vocab.STANDARD_CELL));
			val characterID = cell.getAttributeValue("char");
			val owlCharacter = characterToOWLMap(characterID);
			val stateID = cell.getAttributeValue("state");
			stateToOWLMap.get(stateID).foreach(owlState => { //FIXME missing stateIDs in the map refer to polymorphisms: these are being skipped
				addPropertyAssertion(Vocab.BELONGS_TO_CHARACTER, owlCell, owlCharacter);
				addPropertyAssertion(Vocab.BELONGS_TO_TU, owlCell, owlOTU);
				addPropertyAssertion(Vocab.HAS_STATE, owlCell, owlState);
				taxonOTUToValidTaxonMap.get(otuID).foreach(owlTaxon => {
					stateToOWLPhenotypeMap.get(stateID).flatten.foreach(owlPhenotype => {
						val organism = nextIndividual();
						addClass(organism, owlPhenotype);
						addPropertyAssertion(Vocab.HAS_MEMBER, owlTaxon, organism);
						instantiateClassAssertion(organism, owlPhenotype, true);
					});
				});
			});
	}

	def createRestrictions(aClass: OWLClass): Unit = {
			val partOf = factory.getOWLObjectProperty(Vocab.PART_OF);
			manager.addAxiom(ontology, NamedRestrictionGenerator.createRestriction(partOf, aClass));
			val bearerOf = factory.getOWLObjectProperty(Vocab.BEARER_OF);
			manager.addAxiom(ontology, NamedRestrictionGenerator.createRestriction(bearerOf, aClass));
			manager.addAxiom(ontology, AbsenceClassGenerator.createAbsenceClassAxiom(aClass));
	}

	def classFromTyperef(typeref: Element): OWLClassExpression = {
			val genusID = typeref.getAttributeValue("about");
			val qualifiers = typeref.getChildren("qualifier", phenoNS);
			val genus = factory.getOWLClass(iriForTermID(genusID)); 
			if (qualifiers.isEmpty()) {
				return genus
			} else {
				val operands: mutable.Set[OWLClassExpression] = mutable.Set(genus);
			operands.addAll(qualifiers.map(restrictionFromQualifier(_)));
			return factory.getOWLObjectIntersectionOf(operands);
			}
	}

	def restrictionFromQualifier(qualifier: Element): OWLObjectSomeValuesFrom = {
			val propertyIRI = iriForTermID(qualifier.getAttributeValue("relation"));
			val property = factory.getOWLObjectProperty(propertyIRI);
			val filler = classFromTyperef(qualifier.getChild("holds_in_relation_to", phenoNS).getChild("typeref", phenoNS));
			return factory.getOWLObjectSomeValuesFrom(property, filler);
	}

	def iriForTermID(id: String): IRI = {
			return if (id.startsWith("http://")) {
				IRI.create(id);
			} else {
				IRI.create("http://purl.obolibrary.org/obo/" + id.replaceAll(":", "_"));
			}
	}

	private
	def initialize(): Unit = {
			uuid = UUID.randomUUID().toString();
			nodeIncrementer = 0;
			characterToOWLMap.clear();
			stateToOWLMap.clear();
			taxonOTUToOWLMap.clear();
			taxonOTUToValidTaxonMap.clear();
			stateToOWLPhenotypeMap.clear();
			manager = this.getOWLOntologyManager();
			ontology = manager.createOntology();
	}

	def nextIndividual(): OWLNamedIndividual = {
			return factory.getOWLNamedIndividual(nextIRI());
	}

	def nextClass(): OWLClass = {
			return factory.getOWLClass(this.nextIRI());
	}


	def nextIRI(): IRI = {
			this.nodeIncrementer += 1;
			val id = "http://kb.phenoscape.org/uuid/" + this.uuid + "-" + this.nodeIncrementer;
			return IRI.create(id);
	}

	def addClass(individual: OWLIndividual , aClass: OWLClassExpression): Unit = {
			manager.addAxiom(ontology, factory.getOWLClassAssertionAxiom(aClass, individual));
	}

	def addAnnotation(property: IRI, subject: OWLAnnotationSubject, value: OWLAnnotationValue) {
		val annotationProperty = this.factory.getOWLAnnotationProperty(property);
		manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(annotationProperty));
		manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(annotationProperty, subject, value));
	}

	def addPropertyAssertion(propertyIRI: IRI, subject: OWLIndividual, value: OWLIndividual) {
		val property = factory.getOWLObjectProperty(propertyIRI);
		addPropertyAssertion(property, subject, value);
	}

	def addPropertyAssertion(property: OWLObjectPropertyExpression, subject: OWLIndividual, value: OWLIndividual) {
		if (!property.isAnonymous()) {
			manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(property.asOWLObjectProperty()));
		}
		manager.addAxiom(ontology, this.factory.getOWLObjectPropertyAssertionAxiom(property, subject, value));
	}

	def prefixForNamespace(element: Element, namespace: Namespace): String = {
			val prefix = element.getNamespacesInScope().filter(_.getURI() == namespace.getURI()).headOption.map(_.getPrefix() + ":");
			return prefix.getOrElse("");
	}



	def getLiteralMetaValues(element: Element, property: String, namespace: Namespace): Iterable[String] = {
			val allMetas = element.getChildren("meta", nexmlNS);
			val matchingMetas = allMetas.filter(meta => (meta.getAttributeValue("property") == (prefixForNamespace(meta, namespace) + property)));
			return (matchingMetas.map(_.getValue()) ++ matchingMetas.map(_.getAttributeValue("content"))).filter(StringUtils.isNotBlank(_));
	}

	def getResourceMetasForProperty(element: Element, property: String, propertyNamespace: Namespace): Iterable[Element] = {
			val allMetas = element.getChildren("meta", nexmlNS);
			return allMetas.filter(meta => (meta.getAttributeValue("rel") == (prefixForNamespace(meta, propertyNamespace) + property)));
	}

	def getResourceMetaValues(element: Element, property: String, namespace: Namespace): Iterable[IRI] = {
			val matchingMetas = getResourceMetasForProperty(element, property, namespace);
			return matchingMetas.map(_.getAttributeValue("href")).filter(StringUtils.isNotBlank(_)).map(IRI.create(_));
	}

	def getElementByID(id: String): Element = {
			return nexml.getDescendants(new ElementFilter()).iterator().filter(id == _.getAttributeValue("id")).next();
	}

	def instantiateClassAssertion(individual: OWLIndividual, aClass: OWLClassExpression, expandNamedClass: Boolean): Unit = {
			if (aClass.isInstanceOf[OWLClass]) {
				if (expandNamedClass) {
					val relatedClasses = mutable.Set[OWLClassExpression]();
					val equivClasses = ontology.getEquivalentClassesAxioms(aClass.asOWLClass()).map(_.getClassExpressionsMinus(aClass)).flatten;
					relatedClasses.addAll(equivClasses);
					val superClasses = ontology.getSubClassAxiomsForSubClass(aClass.asOWLClass()).map(_.getSuperClass());
					relatedClasses.addAll(superClasses);
					for (expression <- relatedClasses) {
						if (expression.isInstanceOf[OWLObjectSomeValuesFrom]) {
							instantiateClassAssertion(individual, expression, false);
						}
					}
				} else {
					manager.addAxiom(this.ontology, factory.getOWLClassAssertionAxiom(aClass, individual));
				}
			} else if (aClass.isInstanceOf[OWLQuantifiedObjectRestriction]) { // either someValuesFrom or allValuesFrom
				val restriction = aClass.asInstanceOf[OWLQuantifiedObjectRestriction];
				val filler = restriction.getFiller();
				val property = restriction.getProperty();
				// need IRIs for individuals for type materialization
				val value = this.nextIndividual();
				addPropertyAssertion(property, individual, value);
				instantiateClassAssertion(value, filler, false);
			} else if (aClass.isInstanceOf[OWLObjectIntersectionOf]) {
				for (operand <- (aClass.asInstanceOf[OWLObjectIntersectionOf]).getOperands()) {
					instantiateClassAssertion(individual, operand, false);
				}
			} else {
				manager.addAxiom(this.ontology, this.factory.getOWLClassAssertionAxiom(aClass, individual));
			}
	}

}