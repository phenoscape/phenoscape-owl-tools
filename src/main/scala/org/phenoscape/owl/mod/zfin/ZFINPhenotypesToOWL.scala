package org.phenoscape.owl.mod.zfin

import org.phenoscape.owl.OWLTask
import org.phenoscape.scowl.OWL._
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
  val inheres_in = ObjectProperty(Vocab.INHERES_IN);
  val present = Class(Vocab.PRESENT);
  val absent = Class(Vocab.ABSENT);
  val lacksAllPartsOfType = Class(Vocab.LACKS_ALL_PARTS_OF_TYPE);
  val organism = Class(Vocab.MULTI_CELLULAR_ORGANISM);
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
    val phenotype = nextIndividual();
    axioms.add(phenotype Type annotationClass);
    axioms.add(factory.getOWLDeclarationAxiom(phenotype));
    val superStructureID = StringUtils.stripToNull(items(7));
    val subStructureID = StringUtils.stripToNull(items(3));
    val relationID = StringUtils.stripToNull(items(5));
    val entityTerm = if (subStructureID == null) {
      Class(OBOUtil.iriForTermID(superStructureID));
    } else {
      val superStructure = Class(OBOUtil.iriForTermID(superStructureID));
      val subStructure = Class(OBOUtil.iriForTermID(subStructureID));
      val relation = ObjectProperty(OBOUtil.iriForTermID(relationID));
      val namedComposition = nextClass();
      axioms.add(namedComposition EquivalentTo (subStructure and (relation some superStructure)));
      namedComposition;
    }
    val qualityTerm = Class(OBOUtil.iriForTermID(StringUtils.stripToNull(items(9))));
    val relatedSuperStructureID = StringUtils.stripToNull(items(16));
    val relatedSubStructureID = StringUtils.stripToNull(items(12));
    val relatedRelationID = StringUtils.stripToNull(items(14));
    val relatedEntityTerm = if (relatedSubStructureID == null) {
      if (relatedSuperStructureID != null) {
        Class(OBOUtil.iriForTermID(relatedSuperStructureID));
      } else { null; }
    } else {
      val relatedSuperStructure = Class(OBOUtil.iriForTermID(relatedSuperStructureID));
      val relatedSubStructure = Class(OBOUtil.iriForTermID(relatedSubStructureID));
      val relatedRelation = ObjectProperty(OBOUtil.iriForTermID(relatedRelationID));
      val namedComposition = nextClass();
      axioms.add(namedComposition EquivalentTo (relatedSubStructure and (relatedRelation some relatedSuperStructure)));
      namedComposition;
    }
    val eq_phenotype = (entityTerm, qualityTerm, relatedEntityTerm) match {
      case (null, null, _) => null;
      case (entity: OWLClass, null, null) => (present and (inheres_in some entity));
      case (entity: OWLClass, null, relatedEntity: OWLClass) => {
        log().warn("Related entity with no quality.");
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
      axioms.add(factory.getOWLDeclarationAxiom(organism));
      val phenotypeClass = nextClass();
      axioms.add(factory.getOWLDeclarationAxiom(phenotypeClass));
      axioms.add(phenotypeClass SubClassOf eq_phenotype);
      axioms.add(phenotype Type phenotypeClass);
      val geneIRI = IRI.create("http://zfin.org/" + StringUtils.stripToNull(items(2)));
      val gene = Individual(geneIRI);
      axioms.add(factory.getOWLDeclarationAxiom(gene));
      axioms.add(phenotype Fact (associatedWithGene, gene));
      axioms.add(phenotype Fact (associatedWithTaxon, zebrafish));
    }
    return axioms;
  }

}