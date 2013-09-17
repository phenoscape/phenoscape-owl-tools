package org.phenoscape.owl

import java.io.File

import scala.collection.JavaConversions._
import scala.collection.Set

import org.nescent.strix.OWL.Class
import org.nescent.strix.OWL.OWLClassExpressionToClassExpression
import org.nescent.strix.OWL.OWLObjectPropertyToProperty
import org.nescent.strix.OWL.ObjectProperty
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary

object MatrixGenerator extends OWLTask {

  val manager = OWLManager.createOWLOntologyManager();
  val partOf = ObjectProperty(Vocab.PART_OF);
  val involves = ObjectProperty(Vocab.INVOLVES);
  val limbFin = Class(Vocab.LIMB_FIN);
  val entityTerm = factory.getOWLAnnotationProperty(IRI.create("http://example.org/entity_term"));
  val qualityTerm = factory.getOWLAnnotationProperty(IRI.create("http://example.org/quality_term"));
  val mayHaveState = factory.getOWLObjectProperty(Vocab.MAY_HAVE_STATE_VALUE);
  val dcDescription = factory.getOWLAnnotationProperty(DublinCoreVocabulary.DESCRIPTION.getIRI());

  def main(args: Array[String]): Unit = {
    val dataIRI = IRI.create(new File(args(0)));
    val dataOntology = manager.loadOntology(dataIRI);
    val attributesSlim = manager.loadOntologyFromOntologyDocument(new File("character_slims.obo"));
    println("Getting entities");
    val ro = manager.loadOntologyFromOntologyDocument(new File("ro-slim.owl"));
    val pato = manager.loadOntologyFromOntologyDocument(new File("pato.owl"));
    manager.loadOntologyFromOntologyDocument(new File("references.owl"));
    manager.loadOntologyFromOntologyDocument(new File("merged.owl"));
    val uberon = manager.loadOntologyFromOntologyDocument(new File("ext.owl"));
    manager.applyChange(new AddImport(dataOntology, factory.getOWLImportsDeclaration(ro.getOntologyID().getOntologyIRI())));
    manager.applyChange(new AddImport(dataOntology, factory.getOWLImportsDeclaration(pato.getOntologyID().getOntologyIRI())));
    manager.applyChange(new AddImport(dataOntology, factory.getOWLImportsDeclaration(uberon.getOntologyID().getOntologyIRI())));
    //val partOfLimbFin = Class(IRI.create("http://example.org/partOfLimbFin"));
    val anatomicalEntity = Class(Vocab.ANATOMICAL_ENTITY);
    //manager.addAxiom(uberon, (partOfLimbFin EquivalentTo (partOf some limbFin)));
    val uberonReasoner = new ElkReasonerFactory().createReasoner(uberon);
    val entities = uberonReasoner.getSubClasses(anatomicalEntity, false).getFlattened();
    uberonReasoner.dispose();
    val attributes = attributesSlim.getClassesInSignature();
    println("Creating phenotype classes");
    val newAxioms = (for (entity <- entities; quality <- attributes) yield composeEntityAndQuality(entity, quality)).flatten;
    val characterClasses = for (entity <- entities; quality <- attributes) yield Class(compositionIRI(entity, quality));
    manager.addAxioms(dataOntology, newAxioms);
    val dataReasoner = new ElkReasonerFactory().createReasoner(dataOntology);
    val newManager = OWLManager.createOWLOntologyManager();
    val resultOntology = newManager.createOntology();
    println("Creating class assertions with reasoner");
    val classAssertions = characterClasses.map(charClass => dataReasoner.getInstances(charClass, true).getFlattened().map(inst => factory.getOWLClassAssertionAxiom(charClass, inst))).flatten;
    newManager.addAxioms(resultOntology, classAssertions);
    newManager.addAxioms(resultOntology, newAxioms);
    newManager.addAxioms(resultOntology, dataOntology.getAxioms(dcDescription));
    println("Saving");
    newManager.saveOntology(resultOntology, IRI.create(new File(args(1))));
    dataReasoner.dispose();
    System.exit(0);
  }

  def composeEntityAndQuality(entity: OWLClass, quality: OWLClass): Set[OWLAxiom] = {
    annotateComposedEntityAndQuality(entity, quality).toSet + composeEntityAndQualityInvolves(entity, quality);
  }

  def annotateComposedEntityAndQuality(entity: OWLClass, quality: OWLClass): Set[OWLAnnotationAssertionAxiom] = {
    val subject = compositionIRI(entity, quality);
    val entityAxiom = factory.getOWLAnnotationAssertionAxiom(entityTerm, subject, entity.getIRI());
    val qualityAxiom = factory.getOWLAnnotationAssertionAxiom(qualityTerm, subject, quality.getIRI());
    Set(entityAxiom, qualityAxiom);
  }

  def composeEntityAndQualityInvolves(entity: OWLClass, quality: OWLClass): OWLEquivalentClassesAxiom = {
    val composition = Class(compositionIRI(entity, quality));
    composition EquivalentTo ((involves some entity) and (involves some quality));
  }

  def compositionIRI(entity: OWLClass, quality: OWLClass): IRI = {
    IRI.create("http://example.org/involves?entity=%s&quality=%s".format(entity.getIRI(), quality.getIRI()));
  }

  //	def characterForState(state: OWLNamedIndividual, ontology: OWLOntology): OWLNamedIndividual = {
  //			val axioms = ontology.getAxioms(AxiomType.OBJECT_PROPERTY_ASSERTION);
  //			axioms.find(_.getObject() == state).find(
  //					_.getProperty() == mayHaveState).map(
  //							_.getSubject().asOWLNamedIndividual()).getOrElse(null);
  //	}

  //	def getLabel(entity: OWLEntity, ontology: OWLOntology): String = {
  //			val axioms = ontology.getImportsClosure().map(_.getAxioms(AxiomType.ANNOTATION_ASSERTION)).flatten;
  //			val b = axioms.filter(_.getSubject() == entity.getIRI());
  //			println(b);
  //			axioms.filter(_.getSubject() == entity.getIRI()).find(
  //					_.getProperty() == factory.getRDFSLabel()).map(_.getValue().toString()).getOrElse(null);
  //	}
  //
  //	def formatState(state: OWLNamedIndividual, ontology: OWLOntology): String = {
  //			getLabel(characterForState(state, ontology), ontology) + ": " + getLabel(state, ontology);
  //	}

  /*
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX dc: <http://purl.org/dc/elements/1.1/>

SELECT DISTINCT ?entity  ?quality
FROM <http://purl.obolibrary.org/obo/uberon/merged.owl>
FROM <http://purl.obolibrary.org/obo/uberon/ext.owl>
FROM <file://alldata.owl>
FROM <file://state_groups.owl>
WHERE
{
?character <http://example.org/entity_term> ?entity .
?entity rdfs:label ?entity_label .
?character <http://example.org/quality_term> ?quality .
?quality rdfs:label ?quality_label .
?state rdf:type ?character .
?state dc:description ?state_label .
}
*/

}