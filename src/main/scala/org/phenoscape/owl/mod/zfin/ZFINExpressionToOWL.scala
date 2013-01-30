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
import org.semanticweb.owlapi.model.AddImport

object ZFINExpressionToOWL extends OWLTask {

	val occursIn = factory.getOWLObjectProperty(Vocab.OCCURS_IN);
	val partOf = factory.getOWLObjectProperty(Vocab.PART_OF);
	val annotatedGene = factory.getOWLObjectProperty(Vocab.ANNOTATED_GENE);
	val annotatedTaxon = factory.getOWLObjectProperty(Vocab.ANNOTATED_TAXON);
	val geneExpression = factory.getOWLClass(Vocab.GENE_EXPRESSION);
	val zebrafish = factory.getOWLNamedIndividual(Vocab.ZEBRAFISH);
	val manager = this.getOWLOntologyManager();

	def main(args: Array[String]): Unit = {
			val file = Source.fromFile(args(0), "ISO-8859-1");
			val ontology = convert(file);
			file.close();
			manager.saveOntology(ontology, IRI.create(new File(args(1))));
	}

	def convert(expressionData: Source): OWLOntology = {
			val ontology = manager.createOntology(IRI.create("http://purl.obolibrary.org/obo/phenoscape/zfin_gene_expression.owl"));
			manager.addAxioms(ontology, expressionData.getLines.map(translate(_)).flatten.toSet[OWLAxiom]);
			manager.applyChange(new AddImport(ontology, factory.getOWLImportsDeclaration(IRI.create("http://purl.obolibrary.org/obo/phenoscape/tbox.owl"))));
			return ontology;
	}

	def translate(expressionLine: String): Set[OWLAxiom] = {
			val items = expressionLine.split("\t");
			val axioms = mutable.Set[OWLAxiom]();
			if (items(0).startsWith("ZDB-EFG")) {
				return axioms;
			} else {
				val expression = nextIndividual();
				axioms.add(factory.getOWLDeclarationAxiom(expression));
				axioms.add(factory.getOWLClassAssertionAxiom(geneExpression, expression));
				val structure = nextIndividual();
				axioms.add(factory.getOWLDeclarationAxiom(structure));
				axioms.add(factory.getOWLObjectPropertyAssertionAxiom(occursIn, expression, structure));
				val superStructureID = StringUtils.stripToNull(items(3));
				val subStructureID = StringUtils.stripToNull(items(5));
				val structureType = if (subStructureID == null) {
					factory.getOWLClass(OBOUtil.iriForTermID(superStructureID));
				} else {
					val superStructure = factory.getOWLClass(OBOUtil.iriForTermID(superStructureID));
					val subStructure = factory.getOWLClass(OBOUtil.iriForTermID(subStructureID));
					factory.getOWLObjectIntersectionOf(subStructure, factory.getOWLObjectSomeValuesFrom(partOf, superStructure));
				}
				axioms.add(factory.getOWLClassAssertionAxiom(structureType, structure));
				val geneIRI = IRI.create("http://zfin.org/" + StringUtils.stripToNull(items(0)));
				val gene = factory.getOWLNamedIndividual(geneIRI);
				axioms.add(factory.getOWLDeclarationAxiom(gene));
				axioms.add(factory.getOWLObjectPropertyAssertionAxiom(annotatedGene, expression, gene));
				axioms.add(factory.getOWLObjectPropertyAssertionAxiom(annotatedTaxon, expression, zebrafish));
				//axioms.addAll(structureType.getClassesInSignature().map(NamedRestrictionGenerator.createRestriction(partOf, _)));
				return axioms;
			}
	}

}