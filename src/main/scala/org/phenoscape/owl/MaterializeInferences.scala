package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConversions._
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.reasoner.InferenceType
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.util.InferredClassAssertionAxiomGenerator
import org.semanticweb.owlapi.util.InferredEquivalentClassAxiomGenerator
import org.semanticweb.owlapi.util.InferredOntologyGenerator
import org.semanticweb.owlapi.util.InferredPropertyAssertionGenerator
import org.semanticweb.owlapi.util.InferredSubClassAxiomGenerator
import org.semanticweb.owlapi.util.OWLEntityRemover
import com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory
import eu.trowl.owlapi3.rel.reasoner.dl.RELReasonerFactory
import uk.ac.manchester.cs.factplusplus.owlapiv3.FaCTPlusPlusReasonerFactory

object MaterializeInferences extends OWLTask {

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
			materializeInferences(ontology, reasoner);
	}

	def materializeInferences(ontology: OWLOntology, reasoner: OWLReasoner): Unit = {
			reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY); // this must be called first for ELK
			val axiomGenerators = List(
					new InferredClassAssertionAxiomGenerator(),
					new InferredEquivalentClassAxiomGenerator(),
					new InferredPropertyAssertionGenerator(),
					new InferredSubClassAxiomGenerator()
					);
			val generator = new InferredOntologyGenerator(reasoner, axiomGenerators);
			generator.fillOntology(ontology.getOWLOntologyManager(), ontology);
	}

	def createReasoner(ontology: OWLOntology): OWLReasoner = {
			if (System.getProperties().containsKey(REASONER)) {
				val reasoner = System.getProperty(REASONER);
				createReasoner(ontology, reasoner);
			} else {
				new FaCTPlusPlusReasonerFactory().createReasoner(ontology);
			}
	}

	def createReasoner(ontology: OWLOntology, kind: String): OWLReasoner = {
			kind match {
			//case "hermit" => new ReasonerFactory().createReasoner(ontology);
			case "fact++" => new FaCTPlusPlusReasonerFactory().createReasoner(ontology);
			case "pellet" => new PelletReasonerFactory().createReasoner(ontology);
			case "elk" => new ElkReasonerFactory().createReasoner(ontology);
			case "trowl" => new RELReasonerFactory().createReasoner(ontology);
			}
	}

	def propertiesOnly(): Boolean = {
			if (System.getProperties().containsKey(PROPERTIES_ONLY)) {
				System.getProperty(PROPERTIES_ONLY).toBoolean;
			} else {
				false;
			}
	}

}