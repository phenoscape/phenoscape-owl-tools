package org.phenoscape.owl

import java.io.File

import scala.collection.JavaConversions._

import org.apache.tools.ant.BuildException
import org.apache.tools.ant.PropertyHelper
import org.apache.tools.ant.Task
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologyAlreadyExistsException

class CheckImportsTask extends Task {

	private var ontologiesFolder: File = null;

def setFolder(file: File): Unit = {
		this.ontologiesFolder = file;
}

override
def execute(): Unit = {
		super.execute();
		if (this.ontologiesFolder == null) {
			throw new BuildException("Folder attribute is required", getLocation());
		}
		val manager = OWLManager.createOWLOntologyManager();
		manager.clearIRIMappers();
		manager.addIRIMapper(new BuilderIRIMapper(ontologiesFolder));
		for (file <- this.ontologiesFolder.listFiles()) {
			try {
				manager.loadOntologyFromOntologyDocument(file);
			} catch {
			case e: OWLOntologyAlreadyExistsException => ();
			}
		}
		manager.getOntologies().foreach(checkModified);
}

def checkModified(ontology: OWLOntology): Unit = {
		val helper = PropertyHelper.getPropertyHelper(getProject());
		if (this.isModified(ontology)) {
			helper.setNewProperty(propertyName(ontology), "true");
		}
}

def isModified(ontology: OWLOntology): Boolean = {
		val helper = PropertyHelper.getPropertyHelper(getProject());
		helper.getProperty(ontology.getOntologyID().getOntologyIRI() + ".modified") match {
		case null => ontology.getImports().exists(isModified);
		case _ => true;
		}

}

def propertyName(ontology: OWLOntology): String = {
		return ontology.getOntologyID().getOntologyIRI() + ".modified";
}

}