package org.phenoscape.owl

import java.io.File

import scala.collection.JavaConversions._

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom

object DevelopsFromRuleGenerator extends OWLTask {

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
			val newIRI = ontology.getOntologyID().getOntologyIRI().toString() + "/develops_from_rules";
			val developsFromOntology = manager.createOntology(IRI.create(newIRI));
			manager.applyChange(new AddImport(developsFromOntology, factory.getOWLImportsDeclaration(ontology.getOntologyID().getOntologyIRI())));
			manager.applyChange(new AddImport(developsFromOntology, factory.getOWLImportsDeclaration(AbsenceClassGenerator.getAbsenceOntologyIRI(ontology))));
			ontology.getClassesInSignature(false).map(createRule(_)).foreach(manager.addAxiom(developsFromOntology, _));
			return developsFromOntology;  
	}

	def createRule(ontClass: OWLClass): OWLSubClassOfAxiom = {
			val factory = OWLManager.getOWLDataFactory();
			val missingX = factory.getOWLClass(AbsenceClassGenerator.getAbsenceIRI(ontClass.getIRI()));
			val developsFromX = factory.getOWLObjectSomeValuesFrom(developsFrom, ontClass);
			val missingDevelopsFromX = AbsenceClassGenerator.createAbsenceClassExpression(developsFromX);
			return factory.getOWLSubClassOfAxiom(missingX, missingDevelopsFromX);
	}

}