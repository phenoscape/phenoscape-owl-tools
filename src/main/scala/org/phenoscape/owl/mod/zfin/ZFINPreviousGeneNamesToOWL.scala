package org.phenoscape.owl.mod.zfin

import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntology
import scala.io.Source
import scala.collection.JavaConversions._
import scala.collection.Set
import scala.collection.mutable
import org.phenoscape.owl.OWLTask
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary
import org.phenoscape.owl.Vocab
import java.io.File
import org.apache.commons.lang3.StringUtils

object ZFINPreviousGeneNamesToOWL extends OWLTask {

	val manager = this.getOWLOntologyManager();
	val hasRelatedSynonym = factory.getOWLAnnotationProperty(Vocab.HAS_RELATED_SYNONYM);

	def main(args: Array[String]): Unit = {
			val file = Source.fromFile(args(0));
			val ontology = convert(file);
			file.close();
			manager.saveOntology(ontology, IRI.create(new File(args(1))));
	}

	def convert(data: Source): OWLOntology = {
			val ontology = manager.createOntology();
			manager.addAxioms(ontology, data.getLines.map(translate(_)).flatten.toSet[OWLAxiom]);
			return ontology;
	}

	def translate(line: String): Set[OWLAxiom] = {
			val items = line.split("\t");
			val axioms = mutable.Set[OWLAxiom]();
			if (!items(0).startsWith("ZDB-GENE")) {
				return axioms;
			} else {
				val geneID = StringUtils.stripToNull(items(0));
				val previousName = StringUtils.stripToNull(items(3));
				val geneIRI = IRI.create("http://zfin.org/" + geneID);
				val gene = factory.getOWLNamedIndividual(geneIRI);
				axioms.add(factory.getOWLDeclarationAxiom(gene));
				axioms.add(factory.getOWLAnnotationAssertionAxiom(hasRelatedSynonym, geneIRI, factory.getOWLLiteral(previousName)));
				return axioms;
			}
	}

}