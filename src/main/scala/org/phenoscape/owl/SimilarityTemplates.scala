package org.phenoscape.owl

import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.model.OWLClass
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.OntologyUtil
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import org.phenoscape.owl.util.ExpressionUtil
import org.semanticweb.owlapi.model.OWLAxiom

object SimilarityTemplates {

  def entity(entity: OWLClass): (OWLClass, Set[OWLAxiom]) = ExpressionUtil.nameForExpressionWithAxioms(phenotype_of some entity)

  def partsOfEntity(entity: OWLClass): (OWLClass, Set[OWLAxiom]) = ExpressionUtil.nameForExpressionWithAxioms(phenotype_of some (part_of some entity))
  
  def developsFromEntity(entity: OWLClass): (OWLClass, Set[OWLAxiom]) = ExpressionUtil.nameForExpressionWithAxioms(phenotype_of some (DEVELOPS_FROM some entity))

  def entityWithQuality(entity: OWLClass, quality: OWLClass): (OWLClass, Set[OWLAxiom]) = ExpressionUtil.nameForExpressionWithAxioms((has_part some quality) and (phenotype_of some entity))

  def partsOfEntityWithQuality(entity: OWLClass, quality: OWLClass): (OWLClass, Set[OWLAxiom]) = ExpressionUtil.nameForExpressionWithAxioms((has_part some quality) and (phenotype_of some (part_of some entity)))

  def quality(quality: OWLClass): (OWLClass, Set[OWLAxiom]) = ExpressionUtil.nameForExpressionWithAxioms(has_part some quality)

}