package org.phenoscape.owl.mod.mgi

import java.io.File

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.Map
import scala.collection.Set
import scala.io.Source

import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.mod.xenbase.XenbaseGenesToOWL
import org.phenoscape.owl.util.OBOUtil
import org.phenoscape.owl.NamedRestrictionGenerator
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.Vocab
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

object MGIExpressionToOWL extends OWLTask {

	val occursIn = factory.getOWLObjectProperty(Vocab.OCCURS_IN);
	val partOf = factory.getOWLObjectProperty(Vocab.PART_OF);
	val annotatedGene = factory.getOWLObjectProperty(Vocab.ANNOTATED_GENE);
	val annotatedTaxon = factory.getOWLObjectProperty(Vocab.ANNOTATED_TAXON);
	val geneExpression = factory.getOWLClass(Vocab.GENE_EXPRESSION);
	val mouse = factory.getOWLNamedIndividual(Vocab.MOUSE);
	val manager = this.getOWLOntologyManager();

	def main(args: Array[String]): Unit = {
			val mgiExpressionFile = Source.fromFile(args(0));
			val ontology = convert(mgiExpressionFile);
			mgiExpressionFile.close();
			manager.saveOntology(ontology, IRI.create(new File(args(1))));
	}

	def convert(expressionData: Source): OWLOntology = {
			val ontology = manager.createOntology();
			manager.addAxioms(ontology, expressionData.getLines.drop(1).map(translate(_)).flatten.toSet[OWLAxiom]);
			val rdfsLabel = factory.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI());
			manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(rdfsLabel, mouse.getIRI(), factory.getOWLLiteral("Mus musculus")));
			return ontology;
	}

	def translate(expressionLine: String): Set[OWLAxiom] = {
			val items = expressionLine.split("\t");
			val axioms = mutable.Set[OWLAxiom]();
			if (StringUtils.isBlank(items(5))) { //no Uberon ID available
				return axioms;
			} else {
				val expression = nextIndividual();
				axioms.add(factory.getOWLDeclarationAxiom(expression));
				axioms.add(factory.getOWLClassAssertionAxiom(geneExpression, expression));
				val structure = nextIndividual();
				axioms.add(factory.getOWLDeclarationAxiom(structure));
				axioms.add(factory.getOWLObjectPropertyAssertionAxiom(occursIn, expression, structure));
				val structureID = StringUtils.stripToNull(items(5));
				val structureType =	factory.getOWLClass(OBOUtil.iriForTermID("UBERON:" + structureID));
				axioms.add(factory.getOWLClassAssertionAxiom(structureType, structure));
				val geneIRI = MGIGeneticMarkersToOWL.getGeneIRI(StringUtils.stripToNull(items(0)));
				val gene = factory.getOWLNamedIndividual(geneIRI);
				axioms.add(factory.getOWLDeclarationAxiom(gene));
				axioms.add(factory.getOWLObjectPropertyAssertionAxiom(annotatedGene, expression, gene));
				axioms.add(factory.getOWLObjectPropertyAssertionAxiom(annotatedTaxon, expression, mouse));
				axioms.addAll(structureType.getClassesInSignature().map(NamedRestrictionGenerator.createRestriction(partOf, _)));
				return axioms;
			}
	}

}