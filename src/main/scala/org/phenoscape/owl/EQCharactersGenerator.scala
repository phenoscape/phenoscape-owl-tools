package org.phenoscape.owl

import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLAxiom
import scala.collection.Set
import scala.collection.JavaConversions._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom
import org.nescent.strix.OWL._
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom

object EQCharactersGenerator {

    val factory = OWLManager.getOWLDataFactory();
    val manager = OWLManager.createOWLOntologyManager();
    val partOf = ObjectProperty(Vocab.PART_OF);
    val involves = ObjectProperty(Vocab.INVOLVES);
    val limbFin = Class(Vocab.LIMB_FIN);
    val entityTerm = factory.getOWLAnnotationProperty(IRI.create("http://example.org/entity_term"));
    val qualityTerm = factory.getOWLAnnotationProperty(IRI.create("http://example.org/quality_term"));
    val mayHaveState = factory.getOWLObjectProperty(Vocab.MAY_HAVE_STATE_VALUE);
    val dcDescription = factory.getOWLAnnotationProperty(DublinCoreVocabulary.DESCRIPTION.getIRI());

    def generateEQCharacters(entities: Set[OWLClass], qualities: Set[OWLClass]): Set[OWLAxiom] = {
            (for (entity <- entities; quality <- qualities) yield composeEntityAndQuality(entity, quality)).flatten ;
    }

    def composeEntityAndQuality(entity: OWLClass, quality: OWLClass): Set[OWLAxiom] = {
            annotateComposedEntityAndQuality(entity, quality).toSet + composeEntityAndQualityInvolves(entity, quality);
    }

    def composeEntityAndQualityInvolves(entity: OWLClass, quality: OWLClass): OWLEquivalentClassesAxiom = {
            val composition = Class(compositionIRI(entity, quality));
            composition EquivalentTo ((involves some entity) and (involves some quality));
    }

    def annotateComposedEntityAndQuality(entity: OWLClass, quality: OWLClass): Set[OWLAnnotationAssertionAxiom] = {
            val subject = compositionIRI(entity, quality);
            val entityAxiom = factory.getOWLAnnotationAssertionAxiom(entityTerm, subject, entity.getIRI());
            val qualityAxiom = factory.getOWLAnnotationAssertionAxiom(qualityTerm, subject, quality.getIRI());
            Set(entityAxiom, qualityAxiom);
    }

    def compositionIRI(entity: OWLClass, quality: OWLClass): IRI = {
            IRI.create("http://example.org/involves?entity=%s&quality=%s".format(entity.getIRI(), quality.getIRI()));
    }

}