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
import org.nescent.strix.OWL._
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.AddImport

object MGIPhenotypesToOWL extends OWLTask {

    val involves = ObjectProperty(Vocab.INVOLVES);
    val partOf = ObjectProperty(Vocab.PART_OF);
    val hasPart = ObjectProperty(Vocab.HAS_PART);
    val annotatedGene = ObjectProperty(Vocab.ANNOTATED_GENE);
    val annotatedTaxon = ObjectProperty(Vocab.ANNOTATED_TAXON);
    val annotatedOrganism = ObjectProperty(Vocab.ANNOTATED_ORGANISM);
    val annotationClass = Class(Vocab.PHENOTYPE_ANNOTATION);
    val mouse = Individual(Vocab.MOUSE);
    val towards = ObjectProperty(Vocab.TOWARDS);
    val bearerOf = ObjectProperty(Vocab.BEARER_OF);
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
            return ontology;
    }

    def translate(expressionLine: String): Set[OWLAxiom] = {
            val items = expressionLine.split("\t", -1);
            val involved = mutable.Set[OWLClass]();
            val axioms = mutable.Set[OWLAxiom]();
            val phenotypeAnnotation = nextIndividual();
            axioms.add(phenotypeAnnotation Type annotationClass);
            axioms.add(factory.getOWLDeclarationAxiom(phenotypeAnnotation));
            val structureItem = StringUtils.stripToNull(items(5));
            if (structureItem != null) {
                val structureID = structureItem.split(" ")(0);
                val structureType = Class(OBOUtil.iriForTermID(structureID));
                val primaryQualityType = Class(OBOUtil.iriForTermID(StringUtils.stripToNull(items(6)).split(" ")(0)));
                val relatedStructureID = if (StringUtils.isNotBlank(items(8))) StringUtils.stripToNull(items(8).split(" ")(0)) else null;
                val relatedStructureType = if (relatedStructureID != null) Class(OBOUtil.iriForTermID(relatedStructureID)) else null;
                val qualityType = if (relatedStructureType != null) { //TODO ignoring "quality 2" for the moment, as it is always "abnormal"
                    primaryQualityType and (towards some relatedStructureType)
                } else {
                    primaryQualityType;
                }
                val eq =  if (!qualityType.isAnonymous() && qualityType.asOWLClass().getIRI() == Vocab.ABSENT) { //TODO also handle lacks_all_parts_of_type
                    involved.add(qualityType.asOWLClass());
                    not (hasPart some structureType);
                } else {
                    hasPart some (structureType and (bearerOf some qualityType));
                }
                involved.addAll(eq.getClassesInSignature());
                val organism = nextIndividual();
                val phenotype = nextClass();
                axioms.add(phenotype SubClassOf eq);
                axioms.add(factory.getOWLDeclarationAxiom(organism));
                axioms.add(organism Type phenotype);
                axioms.addAll(ExpressionUtil.instantiateClassAssertion(organism, eq, this));
                axioms.add(phenotypeAnnotation Fact (annotatedOrganism, organism));
                val geneIRI = MGIGeneticMarkersToOWL.getGeneIRI(StringUtils.stripToNull(items(1)));
                val gene = Individual(geneIRI);
                axioms.add(factory.getOWLDeclarationAxiom(gene));
                axioms.add(phenotypeAnnotation Fact (annotatedGene, gene));
                axioms.add(phenotypeAnnotation Fact (annotatedTaxon, mouse));
                axioms.addAll(involved.map(involvee => {
                    val involvesClass = Class(NamedRestrictionGenerator.getRestrictionIRI(Vocab.INVOLVES, involvee.getIRI()));
                    phenotypeAnnotation Type involvesClass;
                }));
            }
            return axioms;
    }

}