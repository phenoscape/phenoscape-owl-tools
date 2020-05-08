package org.phenoscape.owl

import java.io.File

import scala.collection.JavaConverters._
import scala.collection.Set

import org.phenoscape.scowl._
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary
import Vocab._

object MatrixGenerator extends OWLTask {

  val manager       = OWLManager.createOWLOntologyManager();
  val limbFin       = Class(Vocab.LIMB_FIN);
  val entityTerm    = factory.getOWLAnnotationProperty(IRI.create("http://example.org/entity_term"));
  val qualityTerm   = factory.getOWLAnnotationProperty(IRI.create("http://example.org/quality_term"));
  val dcDescription = factory.getOWLAnnotationProperty(DublinCoreVocabulary.DESCRIPTION.getIRI());

  def main(args: Array[String]): Unit = {
    val dataIRI        = IRI.create(new File(args(0)));
    val dataOntology   = manager.loadOntology(dataIRI);
    val attributesSlim = manager.loadOntologyFromOntologyDocument(new File("character_slims.obo"));
    println("Getting entities");
    val ro   = manager.loadOntologyFromOntologyDocument(new File("ro-slim.owl"));
    val pato = manager.loadOntologyFromOntologyDocument(new File("pato.owl"));
    manager.loadOntologyFromOntologyDocument(new File("references.owl"));
    manager.loadOntologyFromOntologyDocument(new File("merged.owl"));
    val uberon = manager.loadOntologyFromOntologyDocument(new File("ext.owl"));
    manager.applyChange(
      new AddImport(dataOntology, factory.getOWLImportsDeclaration(ro.getOntologyID.getOntologyIRI.get))
    );
    manager.applyChange(
      new AddImport(dataOntology, factory.getOWLImportsDeclaration(pato.getOntologyID.getOntologyIRI.get))
    );
    manager.applyChange(
      new AddImport(dataOntology, factory.getOWLImportsDeclaration(uberon.getOntologyID.getOntologyIRI.get))
    );
    //val partOfLimbFin = Class(IRI.create("http://example.org/partOfLimbFin"));
    val anatomicalEntity = Class(Vocab.ANATOMICAL_ENTITY);
    //manager.addAxiom(uberon, (partOfLimbFin EquivalentTo (partOf some limbFin)));
    val uberonReasoner = new ElkReasonerFactory().createReasoner(uberon);
    val entities       = uberonReasoner.getSubClasses(anatomicalEntity, false).getFlattened();
    uberonReasoner.dispose();
    val attributes = attributesSlim.getClassesInSignature();
    println("Creating phenotype classes");
    val newAxioms = (for {
      entity  <- entities.asScala
      quality <- attributes.asScala
    } yield composeEntityAndQuality(entity, quality)).flatten;
    val characterClasses = for {
      entity  <- entities.asScala
      quality <- attributes.asScala
    } yield Class(compositionIRI(entity, quality));
    manager.addAxioms(dataOntology, newAxioms.asJava);
    val dataReasoner   = new ElkReasonerFactory().createReasoner(dataOntology);
    val newManager     = OWLManager.createOWLOntologyManager();
    val resultOntology = newManager.createOntology();
    println("Creating class assertions with reasoner");
    val classAssertions = characterClasses
      .map(charClass =>
        dataReasoner
          .getInstances(charClass, true)
          .getFlattened()
          .asScala
          .map(inst => factory.getOWLClassAssertionAxiom(charClass, inst)))
      .flatten;
    newManager.addAxioms(resultOntology, classAssertions.asJava);
    newManager.addAxioms(resultOntology, newAxioms.asJava);
    newManager.addAxioms(resultOntology, dataOntology.getAxioms(dcDescription));
    println("Saving");
    newManager.saveOntology(resultOntology, IRI.create(new File(args(1))));
    dataReasoner.dispose();
    System.exit(0);
  }

  def composeEntityAndQuality(entity: OWLClass, quality: OWLClass): Set[OWLAxiom] =
    annotateComposedEntityAndQuality(entity, quality)
      .toSet[OWLAxiom] + composeEntityAndQualityInvolves(entity, quality);

  def annotateComposedEntityAndQuality(entity: OWLClass, quality: OWLClass): Set[OWLAnnotationAssertionAxiom] = {
    val subject      = compositionIRI(entity, quality);
    val entityAxiom  = factory.getOWLAnnotationAssertionAxiom(entityTerm, subject, entity.getIRI());
    val qualityAxiom = factory.getOWLAnnotationAssertionAxiom(qualityTerm, subject, quality.getIRI());
    Set(entityAxiom, qualityAxiom);
  }

  def composeEntityAndQualityInvolves(entity: OWLClass, quality: OWLClass): OWLEquivalentClassesAxiom = {
    val composition = Class(compositionIRI(entity, quality));
    composition EquivalentTo ((involves some entity) and (involves some quality));
  }

  def compositionIRI(entity: OWLClass, quality: OWLClass): IRI =
    IRI.create("http://example.org/involves?entity=%s&quality=%s".format(entity.getIRI(), quality.getIRI()));

}
