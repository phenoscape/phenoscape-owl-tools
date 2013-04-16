package org.phenoscape.owl

import java.io.File

import scala.Array.canBuildFrom
import scala.collection.JavaConversions._
import scala.collection.Set

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper

object MergeOntologies {

	def main(args: Array[String]): Unit = {
			val manager = OWLManager.createOWLOntologyManager();
			val mergedOntology = manager.createOntology(args.drop(1).flatMap(process(_)).toSet);
			manager.saveOntology(mergedOntology, IRI.create(new File(args(0))));
	}

	def process(path: String): Set[OWLAxiom] = {
			val f = new File(path);
			if (f.isDirectory()) {
				processFolder(f);
			} else {
				processFile(f);
			}
	}

	def processFile(file: File): Set[OWLAxiom] = {
			val manager = OWLManager.createOWLOntologyManager();
			manager.clearIRIMappers();
			manager.addIRIMapper(NoImportsIRIMapper);
			manager.setSilentMissingImportsHandling(true);
			val ontology = manager.loadOntologyFromOntologyDocument(file);
			ontology.getAxioms();
	}

	def processFolder(folder: File): Set[OWLAxiom] = {
			folder.listFiles().flatMap(processFile(_)).toSet;
	}

	object NoImportsIRIMapper extends OWLOntologyIRIMapper {

		override
		def getDocumentIRI(ontologyIRI: IRI): IRI = {
				return null;
		}

	}

}