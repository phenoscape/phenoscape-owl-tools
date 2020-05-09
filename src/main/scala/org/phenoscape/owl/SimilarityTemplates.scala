package org.phenoscape.owl

import org.phenoscape.kb.ingest.util.ExpressionUtil
import org.phenoscape.owl.Vocab._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.{OWLAxiom, OWLClass}

object SimilarityTemplates {

  def entity(entity: OWLClass): (OWLClass, Set[OWLAxiom]) =
    ExpressionUtil.nameForExpressionWithAxioms(phenotype_of some entity)

  def partsOfEntity(entity: OWLClass): (OWLClass, Set[OWLAxiom]) =
    ExpressionUtil.nameForExpressionWithAxioms(phenotype_of some (entity or (part_of some entity)))

  def developsFromEntity(entity: OWLClass): (OWLClass, Set[OWLAxiom]) =
    ExpressionUtil.nameForExpressionWithAxioms(phenotype_of some (entity or (DEVELOPS_FROM some entity)))

  def entityWithQuality(entity: OWLClass, quality: OWLClass): (OWLClass, Set[OWLAxiom]) =
    ExpressionUtil.nameForExpressionWithAxioms((has_part some quality) and (phenotype_of some entity))

  def partsOfEntityWithQuality(entity: OWLClass, quality: OWLClass): (OWLClass, Set[OWLAxiom]) =
    ExpressionUtil.nameForExpressionWithAxioms(
      (has_part some quality) and (phenotype_of some (entity or (part_of some entity)))
    )

  def quality(quality: OWLClass): (OWLClass, Set[OWLAxiom]) =
    ExpressionUtil.nameForExpressionWithAxioms(has_part some quality)

}
