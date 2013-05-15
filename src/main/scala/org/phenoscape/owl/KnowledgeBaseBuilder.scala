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

class KnowledgeBaseBuilder extends App {

    def iri(string: String): IRI = { IRI.create(string); }
    
    def combine(ontologies: OWLOntology*): OWLOntology = {
            val manager = OWLManager.createOWLOntologyManager();
            manager.createOntology(ontologies.flatMap(_.getAxioms()).toSet);
    }
    
    def reasoner(ontologies: Seq[OWLOntology]): OWLReasoner = {
            val allAxioms = combine(ontologies:_*);
            new ElkReasonerFactory().createReasoner(allAxioms);
    }
    
    def load(location: String): OWLOntology = {
            val manager = OWLManager.createOWLOntologyManager();
            val ont = manager.loadOntologyFromOntologyDocument(iri(location));
            PropertyNormalizer.normalize(ont);
    }
    
    def write(ontology: OWLOntology, filename: String): Unit = {
            val manager = OWLManager.createOWLOntologyManager();
            manager.saveOntology(ontology, new RDFXMLOntologyFormat(), new FileDocumentTarget(new File(filename)));
    }

}