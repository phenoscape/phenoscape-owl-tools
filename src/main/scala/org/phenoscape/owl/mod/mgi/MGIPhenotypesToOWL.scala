package org.phenoscape.owl.mod.mgi

import java.io.File
import scala.collection.JavaConversions._
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.collection.Set
import scala.collection.mutable
import scala.io.Source
import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.util.ExpressionUtil
import org.phenoscape.owl.util.OBOUtil
import org.phenoscape.owl.NamedRestrictionGenerator
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.Vocab
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.AddImport

object MGIPhenotypesToOWL extends OWLTask {

	val involves = factory.getOWLObjectProperty(Vocab.INVOLVES);
	val partOf = factory.getOWLObjectProperty(Vocab.PART_OF);
	val hasPart = factory.getOWLObjectProperty(Vocab.HAS_PART);
	val annotatedGene = factory.getOWLObjectProperty(Vocab.ANNOTATED_GENE);
	val annotatedTaxon = factory.getOWLObjectProperty(Vocab.ANNOTATED_TAXON);
	val annotatedOrganism = factory.getOWLObjectProperty(Vocab.ANNOTATED_ORGANISM);
	val mouse = factory.getOWLNamedIndividual(Vocab.MOUSE);
	val towards = factory.getOWLObjectProperty(Vocab.TOWARDS);
	val bearerOf = factory.getOWLObjectProperty(Vocab.BEARER_OF);
	val manager = this.getOWLOntologyManager();

	def main(args: Array[String]): Unit = {
			val file = Source.fromFile(args(0), "utf-8");
			val ontology = convert(file);
			file.close();
			manager.saveOntology(ontology, IRI.create(new File(args(1))));
	}

	def convert(phenotypeData: Source): OWLOntology = {
			val ontology = manager.createOntology(IRI.create("http://purl.obolibrary.org/obo/phenoscape/mgi_phenotypes.owl"));
			manager.addAxioms(ontology, phenotypeData.getLines.drop(1).map(translate(_)).flatten.toSet[OWLAxiom]);
			manager.applyChange(new AddImport(ontology, factory.getOWLImportsDeclaration(IRI.create("http://purl.obolibrary.org/obo/phenoscape/tbox.owl"))));
			return ontology;
	}

	def translate(expressionLine: String): Set[OWLAxiom] = {
			val items = expressionLine.split("\t", -1);
			val involved = mutable.Set[OWLClass]();
			val axioms = mutable.Set[OWLAxiom]();
			val phenotypeAnnotation = nextIndividual();
			axioms.add(factory.getOWLDeclarationAxiom(phenotypeAnnotation));
			val structureItem = StringUtils.stripToNull(items(5));
			if (structureItem != null) {
				val structureID = structureItem.split(" ")(0);
				val structureType = factory.getOWLClass(OBOUtil.iriForTermID(structureID));
				val primaryQualityType = factory.getOWLClass(OBOUtil.iriForTermID(StringUtils.stripToNull(items(6)).split(" ")(0)));
				val relatedStructureID = if (StringUtils.isNotBlank(items(8))) StringUtils.stripToNull(items(8).split(" ")(0)) else null;
				val relatedStructureType = if (relatedStructureID != null) factory.getOWLClass(OBOUtil.iriForTermID(relatedStructureID)) else null;
				val qualityType = if (relatedStructureType != null) { //TODO ignoring "quality 2" for the moment, as it is always "abnormal"
					factory.getOWLObjectIntersectionOf(primaryQualityType, this.factory.getOWLObjectSomeValuesFrom(towards, relatedStructureType));
				} else {
					primaryQualityType;
				}
				val eq =  if (!qualityType.isAnonymous() && qualityType.asOWLClass().getIRI() == Vocab.ABSENT) { //TODO also handle lacks_all_parts_of_type
					involved.add(qualityType.asOWLClass());
					factory.getOWLObjectComplementOf(factory.getOWLObjectSomeValuesFrom(hasPart, structureType));
				} else {
					factory.getOWLObjectSomeValuesFrom(hasPart, factory.getOWLObjectIntersectionOf(structureType, factory.getOWLObjectSomeValuesFrom(bearerOf, qualityType)));
				}
				involved.addAll(eq.getClassesInSignature());
				val organism = nextIndividual();
				axioms.add(factory.getOWLDeclarationAxiom(organism));
				axioms.add(factory.getOWLClassAssertionAxiom(eq, organism));
				axioms.addAll(ExpressionUtil.instantiateClassAssertion(organism, eq, this));
				axioms.add(factory.getOWLObjectPropertyAssertionAxiom(annotatedOrganism, phenotypeAnnotation, organism));
				val geneIRI = MGIGeneticMarkersToOWL.getGeneIRI(StringUtils.stripToNull(items(1)));
				val gene = factory.getOWLNamedIndividual(geneIRI);
				axioms.add(factory.getOWLDeclarationAxiom(gene));
				axioms.add(factory.getOWLObjectPropertyAssertionAxiom(annotatedGene, phenotypeAnnotation, gene));
				axioms.add(factory.getOWLObjectPropertyAssertionAxiom(annotatedTaxon, phenotypeAnnotation, mouse));
				axioms.addAll(involved.map(involvee => {
					factory.getOWLClassAssertionAxiom(factory.getOWLObjectSomeValuesFrom(involves, involvee), phenotypeAnnotation);
				}));
				//			axioms.addAll(structureType.getClassesInSignature().map(NamedRestrictionGenerator.createRestriction(partOf, _)));
				//			if (relatedStructureType != null) {
				//				axioms.addAll(relatedStructureType.getClassesInSignature().map(NamedRestrictionGenerator.createRestriction(partOf, _)));
				//			}
				//			axioms.addAll(primaryQualityType.getClassesInSignature().map(NamedRestrictionGenerator.createRestriction(bearerOf, _)));
				//			axioms.addAll(involved.map(NamedRestrictionGenerator.createRestriction(involves, _)));

			}
			return axioms;

	}

}