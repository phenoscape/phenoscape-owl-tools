package org.phenoscape.owl

import scala.collection.JavaConversions._
import scala.collection.Set
import java.io.File
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLAxiom

object AbsenceClassGenerator extends OWLTask {

	val hasPart = OWLManager.getOWLDataFactory().getOWLObjectProperty(Vocab.HAS_PART);

	def main(args: Array[String]): Unit = {
			val manager = this.getOWLOntologyManager();
			val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)));
			val absenceOntology = generateAbsenceClasses(ontology);
			manager.saveOntology(absenceOntology, IRI.create(new File(args(1))));
	}

	def generateAbsenceClasses(ontology: OWLOntology): OWLOntology = {
			val manager = ontology.getOWLOntologyManager();
			val factory = manager.getOWLDataFactory();
			val newIRI = getAbsenceOntologyIRI(ontology);
			val absenceOntology = manager.createOntology(newIRI);
			//manager.applyChange(new AddImport(absenceOntology, factory.getOWLImportsDeclaration(ontology.getOntologyID().getOntologyIRI())));
			ontology.getClassesInSignature(false).map(createAbsenceClassAxiom(_)).foreach(manager.addAxiom(absenceOntology, _));
			return absenceOntology;
	}
	
	def createAbsenceClass(ontClass: OWLClass): Set[OWLAxiom] = {
	        return null;
	}

	def createAbsenceClassAxiom(ontClass: OWLClass): OWLEquivalentClassesAxiom = {
			val factory = OWLManager.getOWLDataFactory();
			val absence = factory.getOWLClass(getAbsenceIRI(ontClass.getIRI()));
			val desc = createAbsenceClassExpression(ontClass);
			factory.getOWLEquivalentClassesAxiom(absence, desc);
	}

	def createAbsenceClassExpression(ontClass: OWLClassExpression): OWLClassExpression = {
			val factory = OWLManager.getOWLDataFactory();
			return factory.getOWLObjectComplementOf(factory.getOWLObjectSomeValuesFrom(hasPart, ontClass));
	}

	def getAbsenceIRI(classIRI: IRI): IRI = {
			return NegationClassGenerator.getNegationIRI(NamedRestrictionGenerator.getRestrictionIRI(hasPart.getIRI(), classIRI));
	}

	def getAbsenceOntologyIRI(ontology: OWLOntology): IRI = {
			return IRI.create("http://phenoscape.org/not_has_part/" + ontology.getOntologyID().getOntologyIRI().toString());
	}

}