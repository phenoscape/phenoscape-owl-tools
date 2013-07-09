package org.phenoscape.owl.mod.zfin

import org.phenoscape.owl.OWLTask
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.Set
import org.semanticweb.owlapi.model.OWLOntology
import java.io.File
import scala.io.Source
import org.semanticweb.owlapi.model.OWLAxiom
import org.phenoscape.owl.Vocab
import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.util.OBOUtil
import org.semanticweb.owlapi.model.IRI
import org.phenoscape.owl.NamedRestrictionGenerator
import org.semanticweb.owlapi.model.OWLClass
import org.phenoscape.owl.util.ExpressionUtil
import org.semanticweb.owlapi.model.AddImport

object ZFINPhenotypesToOWL extends OWLTask {

	val involves = factory.getOWLObjectProperty(Vocab.INVOLVES);
	val partOf = factory.getOWLObjectProperty(Vocab.PART_OF);
	val hasPart = factory.getOWLObjectProperty(Vocab.HAS_PART);
	val annotatedGene = factory.getOWLObjectProperty(Vocab.ANNOTATED_GENE);
	val annotatedTaxon = factory.getOWLObjectProperty(Vocab.ANNOTATED_TAXON);
	val annotatedOrganism = factory.getOWLObjectProperty(Vocab.ANNOTATED_ORGANISM);
	val annotationClass = factory.getOWLClass(Vocab.PHENOTYPE_ANNOTATION);
	val zebrafish = factory.getOWLNamedIndividual(Vocab.ZEBRAFISH);
	val towards = factory.getOWLObjectProperty(Vocab.TOWARDS);
	val bearerOf = factory.getOWLObjectProperty(Vocab.BEARER_OF);
	val manager = this.getOWLOntologyManager();

	def main(args: Array[String]): Unit = {
			val file = Source.fromFile(args(0), "ISO-8859-1");
			val ontology = convert(file);
			file.close();
			manager.saveOntology(ontology, IRI.create(new File(args(1))));
	}

	def convert(phenotypeData: Source): OWLOntology = {
			val ontology = manager.createOntology(IRI.create("http://purl.obolibrary.org/obo/phenoscape/zfin_phenotypes.owl"));
			manager.addAxioms(ontology, phenotypeData.getLines.map(translate(_)).flatten.toSet[OWLAxiom]);
			manager.applyChange(new AddImport(ontology, factory.getOWLImportsDeclaration(IRI.create("http://purl.obolibrary.org/obo/phenoscape/tbox.owl"))));
			return ontology;
	}

	def translate(expressionLine: String): Set[OWLAxiom] = {
			val items = expressionLine.split("\t");
			val involved = mutable.Set[OWLClass]();
			val axioms = mutable.Set[OWLAxiom]();
			val phenotypeAnnotation = nextIndividual();
			axioms.add(factory.getOWLClassAssertionAxiom(annotationClass, phenotypeAnnotation));
			axioms.add(factory.getOWLDeclarationAxiom(phenotypeAnnotation));
			val superStructureID = StringUtils.stripToNull(items(3));
			val subStructureID = StringUtils.stripToNull(items(5));
			val structureType = if (subStructureID == null) {
				factory.getOWLClass(OBOUtil.iriForTermID(superStructureID));
			} else {
				val superStructure = factory.getOWLClass(OBOUtil.iriForTermID(superStructureID));
				val subStructure = factory.getOWLClass(OBOUtil.iriForTermID(subStructureID));
				factory.getOWLObjectIntersectionOf(subStructure, factory.getOWLObjectSomeValuesFrom(partOf, superStructure));
			}
			val primaryQualityType = factory.getOWLClass(OBOUtil.iriForTermID(StringUtils.stripToNull(items(11))));
			val relatedSuperStructureID = StringUtils.stripToNull(items(7));
			val relatedSubStructureID = StringUtils.stripToNull(items(9));
			val relatedStructureType = if (relatedSubStructureID == null) {
				if (relatedSuperStructureID != null) {
					factory.getOWLClass(OBOUtil.iriForTermID(relatedSuperStructureID));
				} else { null; }
			} else {
				val relatedSuperStructure = factory.getOWLClass(OBOUtil.iriForTermID(relatedSuperStructureID));
				val relatedSubStructure = factory.getOWLClass(OBOUtil.iriForTermID(relatedSubStructureID));
				factory.getOWLObjectIntersectionOf(relatedSubStructure, factory.getOWLObjectSomeValuesFrom(partOf, relatedSuperStructure));
			}
			val qualityType = if (relatedStructureType != null) {
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
			val geneIRI = IRI.create("http://zfin.org/" + StringUtils.stripToNull(items(2)));
			val gene = factory.getOWLNamedIndividual(geneIRI);
			axioms.add(factory.getOWLDeclarationAxiom(gene));
			axioms.add(factory.getOWLObjectPropertyAssertionAxiom(annotatedGene, phenotypeAnnotation, gene));
			axioms.add(factory.getOWLObjectPropertyAssertionAxiom(annotatedTaxon, phenotypeAnnotation, zebrafish));
			axioms.addAll(involved.map(involvee => {
				factory.getOWLClassAssertionAxiom(factory.getOWLObjectSomeValuesFrom(involves, involvee), phenotypeAnnotation);
			}));
			//axioms.addAll(structureType.getClassesInSignature().map(NamedRestrictionGenerator.createRestriction(partOf, _)));
//			if (relatedStructureType != null) {
//				axioms.addAll(relatedStructureType.getClassesInSignature().map(NamedRestrictionGenerator.createRestriction(partOf, _)));
//			}
//			axioms.addAll(primaryQualityType.getClassesInSignature().map(NamedRestrictionGenerator.createRestriction(bearerOf, _)));
			//FIXME add back absent invves
//			axioms.addAll(involved.map(NamedRestrictionGenerator.createRestriction(involves, _)));
			return axioms;
	}

}