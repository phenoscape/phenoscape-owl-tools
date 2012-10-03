package org.phenoscape.owl

import java.io.File

import scala.collection.JavaConversions._
import scala.collection.Set

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom

object NegationHierarchyAsserter extends OWLTask {

	val negates = OWLManager.getOWLDataFactory().getOWLAnnotationProperty(Vocab.NEGATES);

	def main(args: Array[String]): Unit = {
			val manager = this.getOWLOntologyManager();
			val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)));
			assertNegationHierarchy(ontology);
			if (args.size > 1) {
				manager.saveOntology(ontology, IRI.create(new File(args(1))));
			} else {
				manager.saveOntology(ontology);  
			}
	}

	def assertNegationHierarchy(ontology: OWLOntology): Unit = {
			val manager = ontology.getOWLOntologyManager();
			val ontologies = ontology.getImportsClosure();
			val allClasses = ontology.getClassesInSignature(true);
			val axioms = allClasses.par.map(createSubclassOfAxioms(_, ontologies));
			axioms.seq.foreach(manager.addAxioms(ontology, _));
	}

	def getNegatedClasses(ontClass: OWLClass, ontologies: Set[OWLOntology]): Set[OWLClass] = {
			val factory = OWLManager.getOWLDataFactory();
			return ontologies.map(ont => ontClass.getAnnotations(ont, negates).map(_.getValue()).filter(_.isInstanceOf[IRI]).map(_.asInstanceOf[IRI]).map(factory.getOWLClass(_))).flatten;
	}

	def getNegatingClasses(ontClass: OWLClass, ontologies: Set[OWLOntology]): Set[OWLClass] = {
			val factory = OWLManager.getOWLDataFactory();
			val annotation = factory.getOWLAnnotation(negates, ontClass.getIRI());
			return ontologies.map(_.getAxioms(AxiomType.ANNOTATION_ASSERTION, false).filter(_.getAnnotation() == annotation).map(_.getSubject()).filter(_.isInstanceOf[IRI]).map(iri => factory.getOWLClass(iri.asInstanceOf[IRI]))).flatten;			
	}

	def createSubclassOfAxioms(ontClass: OWLClass, ontologies: Set[OWLOntology]): Set[OWLSubClassOfAxiom] = {
			val factory = OWLManager.getOWLDataFactory();
			val superClasses = getNegatedClasses(ontClass, ontologies).map(_.getSubClasses(ontologies).filter(!_.isAnonymous()).map(_.asOWLClass())).flatten.map(getNegatingClasses(_, ontologies)).flatten;
			superClasses.map(factory.getOWLSubClassOfAxiom(ontClass, _));
	}

}