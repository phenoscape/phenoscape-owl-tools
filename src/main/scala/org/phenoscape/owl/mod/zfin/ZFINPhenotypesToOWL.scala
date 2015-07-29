package org.phenoscape.owl.mod.zfin

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.io.Source

import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.ExpressionUtil
import org.phenoscape.owl.util.OBOUtil
import org.phenoscape.owl.util.OntologyUtil
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass

object ZFINPhenotypesToOWL extends OWLTask {

  def convert(phenotypeData: Source): Set[OWLAxiom] = phenotypeData.getLines.flatMap(translate).toSet[OWLAxiom]

  def translate(expressionLine: String): Set[OWLAxiom] = {
    val items = expressionLine.split("\t")
    val involved = mutable.Set[OWLClass]()
    val axioms = mutable.Set[OWLAxiom]()
    val phenotype = OntologyUtil.nextIndividual()
    axioms.add(phenotype Type AnnotatedPhenotype)
    axioms.add(factory.getOWLDeclarationAxiom(phenotype))
    val superStructureID = StringUtils.stripToNull(items(7))
    val subStructureID = StringUtils.stripToNull(items(3))
    val relationID = StringUtils.stripToNull(items(5))
    val entityTerm = if (subStructureID == null) {
      Class(OBOUtil.iriForTermID(superStructureID))
    } else {
      val superStructure = Class(OBOUtil.iriForTermID(superStructureID))
      val subStructure = Class(OBOUtil.iriForTermID(subStructureID))
      val relation = ObjectProperty(OBOUtil.iriForTermID(relationID))
      val (namedComposition, compositionAxioms) = ExpressionUtil.nameForExpressionWithAxioms(subStructure and (relation some superStructure))
      axioms.addAll(compositionAxioms)
      namedComposition
    }
    val qualityTerm = Class(OBOUtil.iriForTermID(StringUtils.stripToNull(items(9))))
    val relatedSuperStructureID = StringUtils.stripToNull(items(16))
    val relatedSubStructureID = StringUtils.stripToNull(items(12))
    val relatedRelationID = StringUtils.stripToNull(items(14))
    val relatedEntityTerm = if (relatedSubStructureID == null) {
      if (relatedSuperStructureID != null) {
        Class(OBOUtil.iriForTermID(relatedSuperStructureID))
      } else { null }
    } else {
      val relatedSuperStructure = Class(OBOUtil.iriForTermID(relatedSuperStructureID))
      val relatedSubStructure = Class(OBOUtil.iriForTermID(relatedSubStructureID))
      val relatedRelation = ObjectProperty(OBOUtil.iriForTermID(relatedRelationID))
      val (namedComposition, compositionAxioms) = ExpressionUtil.nameForExpressionWithAxioms(relatedSubStructure and (relatedRelation some relatedSuperStructure))
      axioms.addAll(compositionAxioms)
      namedComposition
    }
    val eq_phenotype = (entityTerm, qualityTerm, relatedEntityTerm) match {
      case (null, null, _)                => null
      case (entity: OWLClass, null, null) => (Present and (inheres_in some entity))
      case (entity: OWLClass, null, relatedEntity: OWLClass) => {
        logger.warn("Related entity with no quality.")
        (Present and (inheres_in some entity))
      }
      case (entity: OWLClass, Absent, null)                                 => (LacksAllPartsOfType and (inheres_in some MultiCellularOrganism) and (towards some entity))
      case (entity: OWLClass, LacksAllPartsOfType, relatedEntity: OWLClass) => (LacksAllPartsOfType and (inheres_in some entity) and (towards some relatedEntity))
      case (null, quality: OWLClass, null)                                  => quality
      case (null, quality: OWLClass, relatedEntity: OWLClass)               => (quality and (towards some relatedEntity))
      case (entity: OWLClass, quality: OWLClass, null)                      => (quality and (inheres_in some entity))
      case (entity: OWLClass, quality: OWLClass, relatedEntity: OWLClass)   => (quality and (inheres_in some entity) and (towards some relatedEntity))
    }
    if (eq_phenotype != null) {
      axioms.add(factory.getOWLDeclarationAxiom(MultiCellularOrganism))
      val (phenotypeClass, phenotypeAxioms) = ExpressionUtil.nameForExpressionWithAxioms(eq_phenotype)
      axioms.add(factory.getOWLDeclarationAxiom(phenotypeClass))
      axioms.addAll(phenotypeAxioms)
      axioms.add(phenotype Type phenotypeClass)
      val geneIRI = IRI.create("http://zfin.org/" + StringUtils.stripToNull(items(2)))
      val gene = Individual(geneIRI)
      axioms.add(factory.getOWLDeclarationAxiom(gene))
      axioms.add(phenotype Fact (associated_with_gene, gene))
      axioms.add(phenotype Fact (associated_with_taxon, Zebrafish))
    }
    val figureID = StringUtils.stripToNull(items(25))
    val figure = Individual(OBOUtil.zfinIRI(figureID))
    axioms.add(phenotype Fact (dcSource, figure))
    return axioms.toSet
  }

}