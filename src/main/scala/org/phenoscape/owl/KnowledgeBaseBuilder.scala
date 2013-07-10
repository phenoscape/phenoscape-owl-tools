package org.phenoscape.owl

import scala.collection.JavaConversions._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.io.RDFXMLOntologyFormat
import org.semanticweb.owlapi.io.FileDocumentTarget
import java.io.File
import scala.io.Source
import java.io.BufferedReader
import java.net.URL
import org.phenoscape.owl.util.NullIRIMapper
import java.util.Date

class KnowledgeBaseBuilder extends App {

    val manager = OWLManager.createOWLOntologyManager();
    manager.clearIRIMappers();
    manager.addIRIMapper(NullIRIMapper);

    def iri(string: String): IRI = { IRI.create(string); }

    def combine(ontologies: OWLOntology*): OWLOntology = {
            ontologies.size match {
            case 1 => ontologies(0);
            case _ => {
                val newManager = OWLManager.createOWLOntologyManager();
                newManager.createOntology(ontologies.flatMap(_.getAxioms()).toSet);
            }
            }
    }

    def reasoner(ontology: OWLOntology): OWLReasoner = {
            reasoner(Seq(ontology));
    }

    def reasoner(ontologies: Seq[OWLOntology]): OWLReasoner = {
            val allAxioms = combine(ontologies:_*);
            new ElkReasonerFactory().createReasoner(allAxioms);
    }

    def load(location: String): OWLOntology = {
            val ont = manager.loadOntologyFromOntologyDocument(iri(location));
            PropertyNormalizer.normalize(ont);
    }

    def load(location: File): OWLOntology = {
            val ont = manager.loadOntologyFromOntologyDocument(location);
            PropertyNormalizer.normalize(ont);
    }

    def write(ontology: OWLOntology, filename: String): Unit = {
            val ontManager = ontology.getOWLOntologyManager();
            ontManager.saveOntology(ontology, new RDFXMLOntologyFormat(), new FileDocumentTarget(new File(filename)));
    }

    def cd(dir: String): Unit = {
            cd(new File(dir));
    }

    def cd(dir: File): Unit = {
            System.setProperty("user.dir", dir.getAbsolutePath());
    }

    def step(message: String): Unit = {
            println(new Date() + ": " + message);
    }

}