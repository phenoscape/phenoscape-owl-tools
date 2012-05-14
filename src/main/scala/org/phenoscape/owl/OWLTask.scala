package org.phenoscape.owl

import java.io.File
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntologyManager
import org.semanticweb.owlapi.util.AutoIRIMapper

class OWLTask {

	val ONTOLOGY_FILES = "org.phenoscape.owl.files";

	def getOWLOntologyManager(): OWLOntologyManager = {
			val manager = OWLManager.createOWLOntologyManager();
			if (System.getProperties().containsKey(ONTOLOGY_FILES)) {
				val downloadsFolder = new File(System.getProperty(ONTOLOGY_FILES));
				manager.clearIRIMappers();
				manager.addIRIMapper(new BuilderIRIMapper(downloadsFolder));
			}
			return manager;
	}
	
}