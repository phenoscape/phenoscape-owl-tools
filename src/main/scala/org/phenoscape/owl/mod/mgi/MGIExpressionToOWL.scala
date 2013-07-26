package org.phenoscape.owl.mod.mgi

import java.io.File
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.Map
import scala.collection.Set
import scala.io.Source
import org.nescent.strix.OWL._
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
import org.semanticweb.owlapi.model.AddImport

object MGIExpressionToOWL extends OWLTask {

    val occursIn = ObjectProperty(Vocab.OCCURS_IN);
    val partOf = ObjectProperty(Vocab.PART_OF);
    val associatedWithGene = ObjectProperty(Vocab.ASSOCIATED_WITH_GENE);
    val associatedWithTaxon = ObjectProperty(Vocab.ASSOCIATED_WITH_TAXON);
    val geneExpression = Class(Vocab.GENE_EXPRESSION);
    val mouse = Individual(Vocab.MOUSE);
    val manager = this.getOWLOntologyManager();

    def main(args: Array[String]): Unit = {
            val mgiExpressionFile = Source.fromFile(args(0), "utf-8");
            val ontology = convert(mgiExpressionFile);
            mgiExpressionFile.close();
            manager.saveOntology(ontology, IRI.create(new File(args(1))));
    }

    def convert(expressionData: Source): OWLOntology = {
            val ontology = manager.createOntology(IRI.create("http://purl.obolibrary.org/obo/phenoscape/mgi_gene_expression.owl"));
            manager.addAxioms(ontology, expressionData.getLines.drop(1).map(translate(_)).flatten.toSet[OWLAxiom]);
            val rdfsLabel = factory.getRDFSLabel();			
            manager.addAxiom(ontology, mouse Annotation (rdfsLabel, factory.getOWLLiteral("Mus musculus")));
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
                axioms.add(expression Type geneExpression);
                val structure = nextIndividual();
                axioms.add(factory.getOWLDeclarationAxiom(structure));
                axioms.add(expression Fact (occursIn, structure));
                val structureID = StringUtils.stripToNull(items(5));
                val structureType =	Class(OBOUtil.iriForTermID("UBERON:" + structureID));
                axioms.add(structure Type structureType);
                val geneIRI = MGIGeneticMarkersToOWL.getGeneIRI(StringUtils.stripToNull(items(0)));
                val gene = Individual(geneIRI);
                axioms.add(factory.getOWLDeclarationAxiom(gene));
                axioms.add(expression Fact (associatedWithGene, gene));
                axioms.add(expression Fact (associatedWithTaxon, mouse));
                return axioms;
            }
    }

}