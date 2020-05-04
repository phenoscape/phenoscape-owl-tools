package org.phenoscape.owl

import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLAxiom
import scala.collection.JavaConversions._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom
import org.phenoscape.scowl._
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import Vocab._

object EQCharactersGenerator {

  val factory              = OWLManager.getOWLDataFactory
  val manager              = OWLManager.createOWLOntologyManager()
  val entityTerm           = factory.getOWLAnnotationProperty(IRI.create("http://example.org/entity_term")) //FIXME better ID
  val qualityTerm          = factory.getOWLAnnotationProperty(IRI.create("http://example.org/quality_term")) //FIXME better ID
  val anatomicalProjection = Class("http://purl.obolibrary.org/obo/UBERON_0004529")
  val dcDescription        = factory.getOWLAnnotationProperty(DublinCoreVocabulary.DESCRIPTION.getIRI)

  def generateEQCharacters(entities: Iterable[OWLClass], qualities: Iterable[OWLClass]): Set[OWLAxiom] = {
    val axioms = for {
      entity  <- entities
      quality <- qualities
      axiom   <- composeEntityAndQuality(entity, quality)
    } yield axiom
    axioms.toSet
  }

  def composeEntityAndQuality(entity: OWLClass, quality: OWLClass): Set[OWLAxiom] =
    annotateComposedEntityAndQuality(entity, quality).toSet + composeEntityAndQualityInvolves(entity, quality)

  def composeEntityAndQualityInvolves(entity: OWLClass, quality: OWLClass): OWLEquivalentClassesAxiom = {
    val composition = Class(compositionIRI(entity, quality))
    composition EquivalentTo (quality and (inheres_in some entity) and EQCharacterToken)
  }

  def annotateComposedEntityAndQuality(entity: OWLClass, quality: OWLClass): Set[OWLAnnotationAssertionAxiom] = {
    val subject = compositionIRI(entity, quality)
    Set(subject Annotation (entityTerm, entity), subject Annotation (qualityTerm, quality))
  }

  def compositionIRI(entity: OWLClass, quality: OWLClass): IRI =
    IRI.create(s"http://example.org/involves?entity=${entity.getIRI}&quality=${quality.getIRI}")

}
