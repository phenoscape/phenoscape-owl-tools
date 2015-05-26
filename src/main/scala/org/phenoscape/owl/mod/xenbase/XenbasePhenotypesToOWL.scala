package org.phenoscape.owl.mod.xenbase

import scala.collection.Map
import scala.io.Source

import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.ExpressionUtil
import org.phenoscape.owl.util.OBOUtil
import org.phenoscape.owl.util.OntologyUtil
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLNamedIndividual

object XenbasePhenotypesToOWL extends OWLTask {

  val laevis = Individual(Vocab.XENOPUS_LAEVIS)
  val tropicalis = Individual(Vocab.XENOPUS_TROPICALIS)
  val manager = OWLManager.createOWLOntologyManager()

  def convertToAxioms(phenotypeData: Source): Set[OWLAxiom] = phenotypeData.getLines.drop(1).flatMap(translate).toSet

  def translate(annotationLine: String): Set[OWLAxiom] = {
    val items = annotationLine.split("\t")
    val geneText = StringUtils.stripToNull(items(11))
    if (geneText != null) {
      val phenotype = OntologyUtil.nextIndividual()
      val sourceText = StringUtils.stripToNull(items(0)).toUpperCase
      val source = if (sourceText.contains("IMG"))
        Individual(OBOUtil.xenbaseImageIRI(sourceText))
      else Individual(OBOUtil.xenbaseArticleIRI(sourceText))
      val species = taxon(StringUtils.stripToNull(items(1)))
      val gene = Individual(XenbaseGenesToOWL.getGeneIRI(fixGeneID(geneText)))
      val quality = Class(OBOUtil.iriForTermID(StringUtils.stripToNull(items(15))))
      val (entity, entityAxioms) = OBOUtil.translatePostCompositionNamed(StringUtils.stripToNull(items(13)))
      val (optionalRelatedEntity, relatedEntityAxioms) = OntologyUtil.optionWithSet(Option(StringUtils.stripToNull(items(17))).map(OBOUtil.translatePostCompositionNamed))
      val eqPhenotype = (entity, quality, optionalRelatedEntity) match {
        case (entity, Absent, None)                             => (LacksAllPartsOfType and (inheres_in some MultiCellularOrganism) and (towards value Individual(entity.getIRI)))
        case (entity, LacksAllPartsOfType, Some(relatedEntity)) => (LacksAllPartsOfType and (inheres_in some entity) and (towards value Individual(relatedEntity.getIRI)))
        case (entity, quality, Some(relatedEntity))             => quality and (inheres_in some entity) and (towards some relatedEntity)
        case (entity, quality, None)                            => quality and (inheres_in some entity)
      }
      val (phenotypeClass, phenotypeAxioms) = ExpressionUtil.nameForExpressionWithAxioms(eqPhenotype)
      Set(
        phenotype Type AnnotatedPhenotype,
        phenotype Fact (associated_with_gene, gene),
        phenotype Fact (associated_with_taxon, species),
        phenotype Fact (dcSource, source),
        phenotype Type phenotypeClass,
        factory.getOWLDeclarationAxiom(phenotypeClass),
        factory.getOWLDeclarationAxiom(gene),
        factory.getOWLDeclarationAxiom(species)) ++
        entityAxioms ++
        relatedEntityAxioms
    } else Set.empty
  }

  def fixGeneID(id: String): String = "XB-GENEPAGE-" + id.split(":")(1)

  val taxon: Map[String, OWLNamedIndividual] = Map(
    "XBTAXON:0000001" -> tropicalis,
    "XBTAXON:0000002" -> laevis)

}