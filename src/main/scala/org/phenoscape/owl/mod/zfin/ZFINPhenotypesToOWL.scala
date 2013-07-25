package org.phenoscape.owl.mod.zfin

import org.phenoscape.owl.OWLTask
import org.nescent.strix.OWL._
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

    val involves = ObjectProperty(Vocab.INVOLVES);
    val partOf = ObjectProperty(Vocab.PART_OF);
    val hasPart = ObjectProperty(Vocab.HAS_PART);
    val associatedWithGene = ObjectProperty(Vocab.ASSOCIATED_WITH_GENE);
    val associatedWithTaxon = ObjectProperty(Vocab.ASSOCIATED_WITH_TAXON);
    val annotatedOrganism = ObjectProperty(Vocab.ANNOTATED_ORGANISM);
    val annotationClass = Class(Vocab.ANNOTATED_PHENOTYPE);
    val zebrafish = Individual(Vocab.ZEBRAFISH);
    val towards = ObjectProperty(Vocab.TOWARDS);
    val bearerOf = ObjectProperty(Vocab.BEARER_OF);
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
            return ontology;
    }

    def translate(expressionLine: String): Set[OWLAxiom] = {
            val items = expressionLine.split("\t");
            val involved = mutable.Set[OWLClass]();
            val axioms = mutable.Set[OWLAxiom]();
            val phenotypeAnnotation = nextIndividual();
            axioms.add(phenotypeAnnotation Type annotationClass);
            axioms.add(factory.getOWLDeclarationAxiom(phenotypeAnnotation));
            val superStructureID = StringUtils.stripToNull(items(3));
            val subStructureID = StringUtils.stripToNull(items(5));
            val structureType = if (subStructureID == null) {
                Class(OBOUtil.iriForTermID(superStructureID));
            } else {
                val superStructure = Class(OBOUtil.iriForTermID(superStructureID));
                val subStructure = Class(OBOUtil.iriForTermID(subStructureID));
                subStructure and (partOf some superStructure);
            }
            val primaryQualityType = Class(OBOUtil.iriForTermID(StringUtils.stripToNull(items(11))));
            val relatedSuperStructureID = StringUtils.stripToNull(items(7));
            val relatedSubStructureID = StringUtils.stripToNull(items(9));
            val relatedStructureType = if (relatedSubStructureID == null) {
                if (relatedSuperStructureID != null) {
                    Class(OBOUtil.iriForTermID(relatedSuperStructureID));
                } else { null; }
            } else {
                val relatedSuperStructure = Class(OBOUtil.iriForTermID(relatedSuperStructureID));
                val relatedSubStructure = Class(OBOUtil.iriForTermID(relatedSubStructureID));
                relatedSubStructure and (partOf some relatedSuperStructure);
            }
            val qualityType = if (relatedStructureType != null) {
                primaryQualityType and (towards some relatedStructureType);
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
            axioms.add(factory.getOWLDeclarationAxiom(organism));
            val phenotype = nextClass();
            axioms.add(phenotype SubClassOf eq);
            axioms.add(organism Type phenotype);
            axioms.addAll(ExpressionUtil.instantiateClassAssertion(organism, eq, this));
            axioms.add(phenotypeAnnotation Fact (annotatedOrganism, organism));
            val geneIRI = IRI.create("http://zfin.org/" + StringUtils.stripToNull(items(2)));
            val gene = Individual(geneIRI);
            axioms.add(factory.getOWLDeclarationAxiom(gene));
            axioms.add(phenotypeAnnotation Fact (associatedWithGene, gene));
            axioms.add(phenotypeAnnotation Fact (associatedWithTaxon, zebrafish));
            axioms.addAll(involved.map(involvee => {
                val involvesClass = Class(NamedRestrictionGenerator.getRestrictionIRI(Vocab.INVOLVES, involvee.getIRI()));
                phenotypeAnnotation Type involvesClass;
            }));
            return axioms;
    }

}