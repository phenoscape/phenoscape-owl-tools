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
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.AddImport

object MGIPhenotypesToOWL extends OWLTask {

  val involves = ObjectProperty(Vocab.INVOLVES);
  val partOf = ObjectProperty(Vocab.PART_OF);
  val hasPart = ObjectProperty(Vocab.HAS_PART);
  val associatedWithGene = ObjectProperty(Vocab.ASSOCIATED_WITH_GENE);
  val associatedWithTaxon = ObjectProperty(Vocab.ASSOCIATED_WITH_TAXON);
  val annotationClass = Class(Vocab.ANNOTATED_PHENOTYPE);
  val mouse = Individual(Vocab.MOUSE);
  val towards = ObjectProperty(Vocab.TOWARDS);
  val bearerOf = ObjectProperty(Vocab.BEARER_OF);
  val inheres_in = ObjectProperty(Vocab.INHERES_IN);
  val present = Class(Vocab.PRESENT);
  val absent = Class(Vocab.ABSENT);
  val lacksAllPartsOfType = Class(Vocab.LACKS_ALL_PARTS_OF_TYPE);
  val organism = Class(Vocab.MULTI_CELLULAR_ORGANISM);
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
    val phenotype = nextIndividual();
    axioms.add(phenotype Type annotationClass);
    axioms.add(factory.getOWLDeclarationAxiom(phenotype));
    val structureItem = StringUtils.stripToNull(items(5));
    if (structureItem != null) {
      val structureID = structureItem.split(" ")(0);
      val entityTerm = Class(OBOUtil.iriForTermID(structureID));
      val qualityTerm = Class(OBOUtil.iriForTermID(StringUtils.stripToNull(items(6)).split(" ")(0)));
      val relatedStructureID = if (StringUtils.isNotBlank(items(8))) StringUtils.stripToNull(items(8).split(" ")(0)) else null;
      val relatedEntityTerm = if (relatedStructureID != null) Class(OBOUtil.iriForTermID(relatedStructureID)) else null;
      val eq_phenotype = (entityTerm, qualityTerm, relatedEntityTerm) match {
        case (null, null, _) => null;
        case (entity: OWLClass, null, null) => (present and (inheres_in some entity));
        case (entity: OWLClass, null, relatedEntity: OWLClass) => {
          logger.warn("Related entity with no quality.");
          (present and (inheres_in some entity));
        }
        case (entity: OWLClass, `absent`, null) => (lacksAllPartsOfType and (inheres_in some organism) and (towards value Individual(entity.getIRI())));
        case (entity: OWLClass, `lacksAllPartsOfType`, relatedEntity: OWLClass) => (lacksAllPartsOfType and (inheres_in some entity) and (towards value Individual(relatedEntity.getIRI())));
        case (null, quality: OWLClass, null) => quality;
        case (null, quality: OWLClass, relatedEntity: OWLClass) => (quality and (towards some relatedEntity));
        case (entity: OWLClass, quality: OWLClass, null) => (quality and (inheres_in some entity));
        case (entity: OWLClass, quality: OWLClass, relatedEntity: OWLClass) => (quality and (inheres_in some entity) and (towards some relatedEntity));
      }
      if (eq_phenotype != null) {
        val phenotypeClass = nextClass();
        axioms.add(factory.getOWLDeclarationAxiom(phenotypeClass));
        axioms.add(phenotypeClass SubClassOf eq_phenotype);
        axioms.add(phenotype Type phenotypeClass);
        val geneIRI = MGIGeneticMarkersToOWL.getGeneIRI(StringUtils.stripToNull(items(1)));
        val gene = Individual(geneIRI);
        axioms.add(factory.getOWLDeclarationAxiom(gene));
        axioms.add(phenotype Fact (associatedWithGene, gene));
        axioms.add(phenotype Fact (associatedWithTaxon, mouse));
      }
    }
    return axioms;
  }

}