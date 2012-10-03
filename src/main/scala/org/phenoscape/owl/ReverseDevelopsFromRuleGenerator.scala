package org.phenoscape.owl

import java.io.File

import scala.collection.JavaConversions._

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom

object ReverseDevelopsFromRuleGenerator extends OWLTask {


	val hasPart = OWLManager.getOWLDataFactory().getOWLObjectProperty(Vocab.HAS_PART);
	val developsFrom = OWLManager.getOWLDataFactory().getOWLObjectProperty(Vocab.DEVELOPS_FROM);

	def main(args: Array[String]): Unit = {
			val manager = this.getOWLOntologyManager();
			val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)));
			val developsFromOntology = generateDevelopsFromRules(ontology);
			manager.saveOntology(developsFromOntology, IRI.create(new File(args(1))));
	}

	def generateDevelopsFromRules(ontology: OWLOntology): OWLOntology = {
			val manager = ontology.getOWLOntologyManager();
			val factory = manager.getOWLDataFactory();
			val newIRI = ontology.getOntologyID().getOntologyIRI().toString() + "/reverse_develops_from_rules.owl";
			val developsFromOntology = manager.createOntology(IRI.create(newIRI));
			ontology.getClassesInSignature(false).map(createRule(_)).foreach(manager.addAxiom(developsFromOntology, _));
			return developsFromOntology;  
	}

	def createRule(ontClass: OWLClass): OWLSubClassOfAxiom = {
			val factory = OWLManager.getOWLDataFactory();
			val hasPartSomeX = factory.getOWLObjectSomeValuesFrom(hasPart, ontClass);
			val hasPartSomeDevelopsFromSomeX = factory.getOWLObjectSomeValuesFrom(hasPart, factory.getOWLObjectSomeValuesFrom(developsFrom, ontClass));
			return factory.getOWLSubClassOfAxiom(hasPartSomeDevelopsFromSomeX, hasPartSomeX);
	}

}