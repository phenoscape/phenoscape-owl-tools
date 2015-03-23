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
import org.phenoscape.owl.Vocab._
import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.util.OBOUtil
import org.semanticweb.owlapi.model.IRI
import org.phenoscape.owl.NamedRestrictionGenerator
import org.semanticweb.owlapi.model.OWLClass
import org.phenoscape.owl.util.ExpressionUtil
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.apibinding.OWLManager
import org.apache.log4j.Logger
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary
import org.phenoscape.owl.util.OntologyUtil

object ZFINPhenotypesToOWL extends OWLTask {

  val manager = OWLManager.createOWLOntologyManager()

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile(args(0), "ISO-8859-1")
    val ontology = convert(file)
    file.close()
    manager.saveOntology(ontology, IRI.create(new File(args(1))))
  }

  def convert(phenotypeData: Source): OWLOntology = {
    val ontology = manager.createOntology(IRI.create("http://purl.obolibrary.org/obo/phenoscape/zfin_phenotypes.owl"))
    manager.addAxioms(ontology, phenotypeData.getLines.map(translate(_)).flatten.toSet[OWLAxiom])
    return ontology
  }

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
      case (entity: OWLClass, Absent, null)                                 => (LacksAllPartsOfType and (inheres_in some MultiCellularOrganism) and (towards value Individual(entity.getIRI)))
      case (entity: OWLClass, LacksAllPartsOfType, relatedEntity: OWLClass) => (LacksAllPartsOfType and (inheres_in some entity) and (towards value Individual(relatedEntity.getIRI)))
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
    return axioms
  }

}