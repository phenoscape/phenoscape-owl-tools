package org.phenoscape.owl

import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLAxiom
import scala.collection.Set
import scala.collection.JavaConversions._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom

object EQCharactersGenerator {

  val factory = OWLManager.getOWLDataFactory
  val manager = OWLManager.createOWLOntologyManager()
  val partOf = ObjectProperty(Vocab.PART_OF)
  val involves = ObjectProperty(Vocab.INVOLVES)
  val eqCharacterToken = Class(Vocab.EQ_CHARACTER_TOKEN)
  val entityTerm = factory.getOWLAnnotationProperty(IRI.create("http://example.org/entity_term")) //FIXME better ID
  val qualityTerm = factory.getOWLAnnotationProperty(IRI.create("http://example.org/quality_term")) //FIXME better ID
  val anatomicalProjection = Class("http://purl.obolibrary.org/obo/UBERON_0004529")
  val mayHaveState = factory.getOWLObjectProperty(Vocab.MAY_HAVE_STATE_VALUE)
  val dcDescription = factory.getOWLAnnotationProperty(DublinCoreVocabulary.DESCRIPTION.getIRI)

  def generateEQCharacters(entities: Set[OWLClass], qualities: Set[OWLClass]): Set[OWLAxiom] = {
    for {
      entity <- entities
      quality <- qualities
      axiom <- composeEntityAndQuality(entity, quality)
    } yield axiom
  }

  def composeEntityAndQuality(entity: OWLClass, quality: OWLClass): Set[OWLAxiom] = {
    annotateComposedEntityAndQuality(entity, quality).toSet + composeEntityAndQualityInvolves(entity, quality)
  }

  def composeEntityAndQualityInvolves(entity: OWLClass, quality: OWLClass): OWLEquivalentClassesAxiom = {
    val composition = Class(compositionIRI(entity, quality))
    composition EquivalentTo ((involves some entity) and (involves some quality) and eqCharacterToken)
  }

  def annotateComposedEntityAndQuality(entity: OWLClass, quality: OWLClass): Set[OWLAnnotationAssertionAxiom] = {
    val subject = compositionIRI(entity, quality)
    val entityAxiom = subject Annotation (entityTerm, entity.getIRI)
    val qualityAxiom = subject Annotation (qualityTerm, quality.getIRI)
    Set(entityAxiom, qualityAxiom)
  }

  def compositionIRI(entity: OWLClass, quality: OWLClass): IRI = {
    IRI.create(s"http://example.org/involves?entity=${entity.getIRI}&quality=${quality.getIRI}")
  }

}