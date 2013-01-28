package org.phenoscape.owl.mod.zfin

import java.io.File

import scala.collection.JavaConversions._
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.collection.Set
import scala.collection.mutable
import scala.io.Source

import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.Vocab
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

object ZFINGeneticMarkersToOWL extends OWLTask {

	val manager = this.getOWLOntologyManager();
	val rdfsLabel = factory.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI());
	val hasExactSynonym = factory.getOWLAnnotationProperty(Vocab.HAS_EXACT_SYNONYM);
	val geneClass = factory.getOWLClass(Vocab.GENE);

	def main(args: Array[String]): Unit = {
			val file = Source.fromFile(args(0));
			val ontology = convert(file);
			file.close();
			manager.saveOntology(ontology, IRI.create(new File(args(1))));
	}

	def convert(markersData: Source): OWLOntology = {
			val ontology = manager.createOntology(IRI.create("http://purl.obolibrary.org/obo/phenoscape/zfin_genes.owl"));
			manager.addAxioms(ontology, markersData.getLines.map(translate(_)).flatten.toSet[OWLAxiom]);
			return ontology;
	}

	def translate(line: String): Set[OWLAxiom] = {
			val items = line.split("\t");
			val axioms = mutable.Set[OWLAxiom]();
			if (items(3) != "GENE") {
				return axioms;
			} else {
				val geneID = StringUtils.stripToNull(items(0));
				val geneSymbol = StringUtils.stripToNull(items(1));
				val geneFullName = StringUtils.stripToNull(items(2));
				val geneIRI = IRI.create("http://zfin.org/" + geneID);
				val gene = factory.getOWLNamedIndividual(geneIRI);
				axioms.add(factory.getOWLDeclarationAxiom(gene));
				axioms.add(factory.getOWLClassAssertionAxiom(geneClass, gene));
				axioms.add(factory.getOWLAnnotationAssertionAxiom(rdfsLabel, geneIRI, factory.getOWLLiteral(geneSymbol)));
				axioms.add(factory.getOWLAnnotationAssertionAxiom(hasExactSynonym, geneIRI, factory.getOWLLiteral(geneFullName)));
				return axioms;
			}
	}

}