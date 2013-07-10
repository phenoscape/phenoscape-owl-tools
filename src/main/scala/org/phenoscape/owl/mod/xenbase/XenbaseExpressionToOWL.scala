package org.phenoscape.owl.mod.xenbase

import java.io.File

import scala.collection.JavaConversions._
import scala.collection.Map
import scala.collection.Set
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.collection.mutable
import scala.io.Source

import org.apache.commons.lang3.StringUtils
import org.nescent.strix.OWL._
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.util.OBOUtil
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLOntology

object XenbaseExpressionToOWL extends OWLTask {

    val occursIn = ObjectProperty(Vocab.OCCURS_IN);
    val partOf = ObjectProperty(Vocab.PART_OF);
    val annotatedGene = ObjectProperty(Vocab.ANNOTATED_GENE);
    val annotatedTaxon = ObjectProperty(Vocab.ANNOTATED_TAXON);
    val geneExpression = Class(Vocab.GENE_EXPRESSION);
    val laevis = Individual(Vocab.XENOPUS_LAEVIS);
    val tropicalis = Individual(Vocab.XENOPUS_TROPICALIS);
    val manager = this.getOWLOntologyManager();

    def main(args: Array[String]): Unit = {
            val genepageMappingsFile = Source.fromFile(args(0), "utf-8");
            val laevisExpressionFile = Source.fromFile(args(1), "utf-8");
            val tropicalisExpressionFile = Source.fromFile(args(2));            
            val ontology = convert(genepageMappingsFile, laevisExpressionFile, tropicalisExpressionFile);
            genepageMappingsFile.close();
            laevisExpressionFile.close();
            tropicalisExpressionFile.close();
            manager.saveOntology(ontology, IRI.create(new File(args(3))));
    }

    def convert(genepageMappingsFile: Source, laevisExpressionFile: Source, tropicalisExpressionFile: Source): OWLOntology = {
            val mappings = indexGenepageMappings(genepageMappingsFile);
            val ontology = convert(laevisExpressionFile, mappings, laevis);
            val tropicalisOntology = convert(tropicalisExpressionFile, mappings, tropicalis);
            manager.addAxioms(ontology, tropicalisOntology.getAxioms());
            return ontology;
    }

    def indexGenepageMappings(mappings: Source): Map[String, String] = {
            val index = mutable.Map[String, String]();
            for (mapping <- mappings.getLines()) {
                val items = mapping.split("\t");
                val genepageID = StringUtils.stripToNull(items(0));
                for (geneID <- items(1).split(",")) {
                    index(StringUtils.stripToNull(geneID)) = genepageID;
                }
            }
            return index;
    }

    def convert(expressionData: Source, genepageMappings: Map[String, String], species: OWLNamedIndividual): OWLOntology = {
            val id = if (species == laevis) "http://purl.obolibrary.org/obo/phenoscape/xenbase_gene_expression.owl" else "";
            val ontology = manager.createOntology(IRI.create(id));
            manager.addAxioms(ontology, expressionData.getLines.map(translate(_, genepageMappings, species)).flatten.toSet[OWLAxiom]);
            val rdfsLabel = factory.getRDFSLabel();
            manager.addAxiom(ontology, laevis Annotation (rdfsLabel, factory.getOWLLiteral("Xenopus laevis")));
            manager.addAxiom(ontology, tropicalis Annotation (rdfsLabel, factory.getOWLLiteral("Xenopus tropicalis")));
            return ontology;
    }

    def translate(expressionLine: String, genepageMappings: Map[String, String], species: OWLNamedIndividual): Set[OWLAxiom] = {
            val items = expressionLine.split("\t");
            val axioms = mutable.Set[OWLAxiom]();
            if (StringUtils.stripToEmpty(items(3)) == "unspecified") {
                return axioms;
            } else {
                val expression = nextIndividual();
                axioms.add(factory.getOWLDeclarationAxiom(expression));
                axioms.add(expression Type geneExpression);
                val structure = nextIndividual();
                axioms.add(factory.getOWLDeclarationAxiom(structure));
                axioms.add(expression Fact (occursIn, structure));
                val structureID = StringUtils.stripToNull(items(3).trim().split(" ")(0));
                val structureType =	Class(OBOUtil.iriForTermID(structureID));
                axioms.add(structure Type structureType);
                val genepageID = genepageMappings(StringUtils.stripToNull(items(0)));
                val geneIRI = XenbaseGenesToOWL.getGeneIRI(genepageID);
                val gene = Individual(geneIRI);
                axioms.add(factory.getOWLDeclarationAxiom(gene));
                axioms.add(expression Fact (annotatedGene, gene));
                axioms.add(expression Fact (annotatedTaxon, species));
                return axioms;
            }
    }

}