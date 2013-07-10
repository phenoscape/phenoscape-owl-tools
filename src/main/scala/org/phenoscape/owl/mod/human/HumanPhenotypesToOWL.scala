package org.phenoscape.owl.mod.human

import java.io.File

import scala.collection.JavaConversions._
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.collection.Set
import scala.collection.mutable
import scala.io.Source

import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.util.OBOUtil
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.Vocab
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

object HumanPhenotypesToOWL extends OWLTask {

	val rdfsLabel = factory.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI());
	val involves = factory.getOWLObjectProperty(Vocab.INVOLVES);
	val annotatedGene = factory.getOWLObjectProperty(Vocab.ANNOTATED_GENE);
	val annotatedTaxon = factory.getOWLObjectProperty(Vocab.ANNOTATED_TAXON);
	val human = factory.getOWLNamedIndividual(Vocab.HUMAN);
	val geneClass = factory.getOWLClass(Vocab.GENE);
	val annotationClass = factory.getOWLClass(Vocab.PHENOTYPE_ANNOTATION);
	val manager = this.getOWLOntologyManager();

	def main(args: Array[String]): Unit = {
			val file = Source.fromFile(args(0), "utf-8");
			val ontology = convert(file);
			file.close();
			manager.saveOntology(ontology, IRI.create(new File(args(1))));
	}

	def convert(phenotypeData: Source): OWLOntology = {
			val ontology = manager.createOntology(IRI.create("http://purl.obolibrary.org/obo/phenoscape/human_phenotypes.owl"));
			manager.addAxioms(ontology, phenotypeData.getLines.drop(1).map(translate(_)).flatten.toSet[OWLAxiom]);
			manager.applyChange(new AddImport(ontology, factory.getOWLImportsDeclaration(IRI.create("http://purl.obolibrary.org/obo/phenoscape/tbox.owl"))));
			return ontology;
	}

	def translate(expressionLine: String): Set[OWLAxiom] = {
			val items = expressionLine.split("\t");
			val axioms = mutable.Set[OWLAxiom]();
			val phenotypeAnnotation = nextIndividual();
			axioms.add(factory.getOWLClassAssertionAxiom(annotationClass, phenotypeAnnotation));
			axioms.add(factory.getOWLDeclarationAxiom(phenotypeAnnotation));
			val phenotypeID = StringUtils.stripToNull(items(3));
			val phenotypeClass = factory.getOWLClass(OBOUtil.iriForTermID(phenotypeID));
			axioms.add(factory.getOWLClassAssertionAxiom(phenotypeClass, phenotypeAnnotation));
			val geneIRI = IRI.create("http://www.ncbi.nlm.nih.gov/gene/" + StringUtils.stripToNull(items(0)));
			val geneSymbol = StringUtils.stripToNull(items(1));
			axioms.add(factory.getOWLAnnotationAssertionAxiom(rdfsLabel, geneIRI, factory.getOWLLiteral(geneSymbol)));
			val gene = factory.getOWLNamedIndividual(geneIRI);
			axioms.add(factory.getOWLClassAssertionAxiom(geneClass, gene));
			axioms.add(factory.getOWLDeclarationAxiom(gene));
			axioms.add(factory.getOWLObjectPropertyAssertionAxiom(annotatedGene, phenotypeAnnotation, gene));
			axioms.add(factory.getOWLObjectPropertyAssertionAxiom(annotatedTaxon, phenotypeAnnotation, human));
			return axioms;
	}

}