package org.phenoscape.owl

import java.io.File

import scala.Array.canBuildFrom
import scala.collection.JavaConversions._
import scala.collection.Set

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory
import org.semanticweb.owlapi.reasoner.OWLReasoner

object MaterializeSubClassOfClosure extends OWLTask {

	def main(args: Array[String]): Unit = {
			val targetFile = new File(System.getProperty("org.phenoscape.owl.MaterializeSubClassOfClosure.target"));
			val manager = this.getOWLOntologyManager();
			val source = manager.createOntology();
			args.map(filename => loadOntology(new File(filename))).foreach(ont => manager.addAxioms(source, ont.getAxioms()));
			val reasoner = new StructuralReasonerFactory().createReasoner(source);
			val axioms = source.getClassesInSignature().map(createSubClassOfAxioms(_, reasoner)).flatten;
			val target = manager.createOntology(axioms.asInstanceOf[Set[OWLAxiom]]);
			manager.saveOntology(target, IRI.create(targetFile));
	}

	def loadOntology(file: File): OWLOntology = {
			val manager = OWLManager.createOWLOntologyManager();
			manager.clearIRIMappers();
			manager.setSilentMissingImportsHandling(true);
			manager.loadOntologyFromOntologyDocument(file);
	}

	def createSubClassOfAxioms(owlClass: OWLClass, reasoner: OWLReasoner): Set[OWLSubClassOfAxiom] = {
			val factory = OWLManager.getOWLDataFactory();
			val axioms = reasoner.getSuperClasses(owlClass, false).getFlattened().map(factory.getOWLSubClassOfAxiom(owlClass, _));
			axioms.add(factory.getOWLSubClassOfAxiom(owlClass, owlClass));
			return axioms;
	}


}