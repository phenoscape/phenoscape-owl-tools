package org.phenoscape.owl;

import java.io.File

import scala.collection.JavaConversions._

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.model.OWLOntology

object NamedRestrictionGenerator extends OWLTask {

	def main(args : Array[String]) : Unit = {
			val manager = this.getOWLOntologyManager();
			val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)));
			val property = manager.getOWLDataFactory().getOWLObjectProperty(IRI.create(args(1)));
			val restrictionsOntology = generateRestrictions(ontology, property);
			manager.saveOntology(restrictionsOntology, IRI.create(new File(args(2))));
	}

	def generateRestrictions(ontology: OWLOntology, property: OWLObjectProperty): OWLOntology = {
			val manager = ontology.getOWLOntologyManager();
			val newIRI = property.getIRI().toString() + "_some_" + ontology.getOntologyID().getOntologyIRI().toString();
			val restrictionsOntology = manager.createOntology(IRI.create(newIRI));
			val factory = manager.getOWLDataFactory();
			ontology.getClassesInSignature(false).map(createRestriction(property, _)).foreach(manager.addAxiom(restrictionsOntology, _));
			return restrictionsOntology;
	}

	def createRestriction(property: OWLObjectProperty, ontClass: OWLClass): OWLEquivalentClassesAxiom = {
			val factory = OWLManager.getOWLDataFactory();
			val newClassIRI = getRestrictionIRI(property.getIRI(), ontClass.getIRI());
			val restriction = factory.getOWLClass(newClassIRI);
			return factory.getOWLEquivalentClassesAxiom(restriction, factory.getOWLObjectSomeValuesFrom(property, ontClass));
	}

	def getRestrictionIRI(propertyIRI: IRI, classIRI: IRI): IRI = {
			return IRI.create(propertyIRI.toString() + "_some_" + classIRI.toString());
	}

}
