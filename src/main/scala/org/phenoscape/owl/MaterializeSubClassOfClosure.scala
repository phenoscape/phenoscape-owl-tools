package org.phenoscape.owl

import java.io.File

import scala.collection.JavaConversions._
import scala.collection.Set

import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory
import org.semanticweb.owlapi.reasoner.OWLReasoner

object MaterializeSubClassOfClosure extends OWLTask {

	val manager = this.getOWLOntologyManager();

	def main(args: Array[String]): Unit = {
			val targetFile = new File(System.getProperty("org.phenoscape.owl.MaterializeSubClassOfClosure.target"));
			val source = manager.loadOntologyFromOntologyDocument(new File(args(0)));
			val reasoner = new StructuralReasonerFactory().createReasoner(source);
			val axioms: Set[OWLAxiom] = source.getClassesInSignature(true).map(createSubClassOfAxioms(_, reasoner)).flatten;
			val target = manager.createOntology(axioms);
			manager.saveOntology(target, IRI.create(targetFile));
			System.exit(0);
	}

	def createSubClassOfAxioms(owlClass: OWLClass, reasoner: OWLReasoner): Set[OWLSubClassOfAxiom] = {
			val ontology = reasoner.getRootOntology();
			val axioms = reasoner.getSuperClasses(owlClass, false).getFlattened().map(factory.getOWLSubClassOfAxiom(owlClass, _)).filterNot(ontology.containsAxiomIgnoreAnnotations(_, true));
			axioms.add(factory.getOWLSubClassOfAxiom(owlClass, owlClass));
			return axioms;
	}


}