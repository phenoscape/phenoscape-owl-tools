package org.phenoscape.owl

import java.io.File

import scala.collection.JavaConversions._

import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.reasoner.InferenceType
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.util.OWLEntityRemover

import com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory

import eu.trowl.owl.api3.{ReasonerFactory => TrOWLReasonerFactory}
import uk.ac.manchester.cs.factplusplus.owlapiv3.FaCTPlusPlusReasonerFactory

object MaterializeInferencesCustom extends OWLTask {


	val REASONER = "org.phenoscape.owl.reasoner";
	val PROPERTIES_ONLY = "org.phenoscape.owl.reasoner.propertiesonly";

	def main(args : Array[String]) : Unit = {
			val manager = this.getOWLOntologyManager();
			val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)));
			materializeInferences(ontology);
			if (args.size > 1) {
				manager.saveOntology(ontology, IRI.create(new File(args(1))));
			} else {
				manager.saveOntology(ontology);  
			}
	}

	def materializeInferences(ontology: OWLOntology): Unit = {
			val factory = OWLManager.getOWLDataFactory();
			val reasoner = if (propertiesOnly()) {
				val manager = ontology.getOWLOntologyManager();
				val classes = ontology.getClassesInSignature();
				val tempOntology = manager.createOntology();
				val entityRemover = new OWLEntityRemover(manager, Set(tempOntology));
				manager.addAxioms(tempOntology, ontology.getImportsClosure().map(_.getAxioms()).reduce(_ ++ _));
				tempOntology.getClassesInSignature().foreach(entityRemover.visit(_));
				manager.applyChanges(entityRemover.getChanges());
				createReasoner(tempOntology);
			} else {
				createReasoner(ontology);
			}
			reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY); // this must be called first for ELK
			reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS);
			reasoner.precomputeInferences(InferenceType.OBJECT_PROPERTY_ASSERTIONS);
			this.addInferredAxioms(this.getSuperClassAxioms(ontology, reasoner), ontology);
			this.addInferredAxioms(this.getEquivalentClassAxioms(ontology, reasoner), ontology);
			this.addInferredAxioms(this.getPropertyAssertionAxioms(ontology, reasoner), ontology);
			this.addInferredAxioms(this.getClassAssertionAxioms(ontology, reasoner), ontology);
	}

	def createReasoner(ontology: OWLOntology): OWLReasoner = {
			if (System.getProperties().containsKey(REASONER)) {
				val reasoner = System.getProperty(REASONER);
				reasoner match {
				//case "hermit" => new ReasonerFactory().createReasoner(ontology);
				case "fact++" => new FaCTPlusPlusReasonerFactory().createReasoner(ontology);
				case "pellet" => new PelletReasonerFactory().createReasoner(ontology);
				case "elk" => new ElkReasonerFactory().createReasoner(ontology);
				case "trowl" => new TrOWLReasonerFactory().createReasoner(ontology);
				}
			} else {
				new FaCTPlusPlusReasonerFactory().createReasoner(ontology);
			}
	}

	def propertiesOnly(): Boolean = {
			if (System.getProperties().containsKey(PROPERTIES_ONLY)) {
				System.getProperty(PROPERTIES_ONLY).toBoolean;
			} else {
				false;
			}
	}

	def addInferredAxioms(axioms: Iterable[OWLAxiom], ontology: OWLOntology): Unit = {
			val manager = ontology.getOWLOntologyManager();
			val inferredAxioms = axioms.filter(!ontology.containsAxiom(_, true)).toSet;
			manager.addAxioms(ontology, inferredAxioms);

	}

	def getSuperClassAxioms(ontology: OWLOntology, reasoner: OWLReasoner): Iterable[OWLAxiom] = {
			val factory = OWLManager.getOWLDataFactory();
			val classes = ontology.getClassesInSignature(true);
			return classes.map((owlClass) => reasoner.getSuperClasses(owlClass, true).getFlattened().map(factory.getOWLSubClassOfAxiom(owlClass, _))).flatten;
	}

	def getEquivalentClassAxioms(ontology: OWLOntology, reasoner: OWLReasoner): Iterable[OWLAxiom] = {
			val factory = OWLManager.getOWLDataFactory();
			val classes = ontology.getClassesInSignature(true);
			return classes.map((owlClass) => reasoner.getEquivalentClasses(owlClass).getEntities()).map(factory.getOWLEquivalentClassesAxiom(_));
	}

	def getPropertyAssertionAxioms(ontology: OWLOntology, reasoner: OWLReasoner): Iterable[OWLAxiom] = {
			val factory = OWLManager.getOWLDataFactory();
			val individuals = ontology.getIndividualsInSignature(true);
			val properties = ontology.getObjectPropertiesInSignature(true);
			def getAssertionAxioms(property: OWLObjectProperty, individual: OWLNamedIndividual) = {
				val values = reasoner.getObjectPropertyValues(individual, property);
				values.getFlattened().map(factory.getOWLObjectPropertyAssertionAxiom(property, individual, _));
			}
			return individuals.map((ind) => properties.map(getAssertionAxioms(_, ind)).flatten).flatten;
	}

	def getClassAssertionAxioms(ontology: OWLOntology, reasoner: OWLReasoner): Iterable[OWLAxiom] = {
			val factory = OWLManager.getOWLDataFactory();
			val individuals = ontology.getIndividualsInSignature(true);
			return individuals.map((ind) => (reasoner.getTypes(ind, true)).getFlattened().map(factory.getOWLClassAssertionAxiom(_, ind))).flatten;
	}

}