package org.phenoscape.owl

import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.model.OWLClass
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.OntologyUtil
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom

object SimilarityTemplates {

  def entity(entity: OWLClass) = name(has_part some (phenotype_of some entity))

  def entityAndParts(entity: OWLClass) = name(has_part some (phenotype_of some (part_of some entity)))

  def entityWithQuality(entity: OWLClass, quality: OWLClass) = name(has_part some (quality and (phenotype_of some entity)))

  def entityAndPartsWithQuality(entity: OWLClass, quality: OWLClass) = name(has_part some (quality and (phenotype_of some (part_of some entity))))

  private def name(expression: OWLClassExpression) = OntologyUtil.nextClass() EquivalentTo expression

}