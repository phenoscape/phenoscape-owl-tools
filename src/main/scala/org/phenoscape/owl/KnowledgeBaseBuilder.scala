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
import org.phenoscape.owl.util.OBOUtil
import org.semanticweb.owlapi.model.OWLOntologyManager
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.AxiomType

class KnowledgeBaseBuilder extends App {

  private[this] val globalManager = OWLManager.createOWLOntologyManager
  globalManager.clearIRIMappers()
  globalManager.addIRIMapper(NullIRIMapper)

  def getManager: OWLOntologyManager = globalManager

  def iri(string: String): IRI = { IRI.create(string) }

  def combine(ontologies: OWLOntology*): OWLOntology = {
    if (ontologies.size == 1)
      ontologies(0)
    else {
      val newManager = OWLManager.createOWLOntologyManager
      newManager.createOntology(ontologies.flatMap(_.getAxioms).toSet)
    }
  }

  def combine(ontology: SourcedAxioms, ontologies: SourcedAxioms*): OWLOntology = OWLManager.createOWLOntologyManager().createOntology(ontologies.flatMap(_.axioms).toSet)

  def reasoner(ontologies: OWLOntology*): OWLReasoner = {
    val allAxioms = combine(ontologies: _*)
    new ElkReasonerFactory().createReasoner(allAxioms)
  }

  def reasoner(axioms: Set[OWLAxiom]): OWLReasoner = new ElkReasonerFactory().createReasoner(OWLManager.createOWLOntologyManager().createOntology(axioms))

  def loadNormalized(location: File): OWLOntology = {
    val ont = globalManager.loadOntologyFromOntologyDocument(location)
    val definedByAxioms = ont.getClassesInSignature.flatMap(OBOUtil.createDefinedByAnnotation)
    globalManager.addAxioms(ont, definedByAxioms)
    PropertyNormalizer.normalize(ont)
  }

  def loadFromWebWithImports(iri: IRI): SourcedAxioms = {
    val manager = OWLManager.createOWLOntologyManager()
    val ont = manager.loadOntology(iri)
    val importsAxioms = (ont.getImportsClosure - ont).flatMap(_.getAxioms)
    manager.addAxioms(ont, importsAxioms)
    PropertyNormalizer.normalize(ont)
    SourcedAxioms(ont.getAxioms.toSet, iri, Option(ont.getOntologyID.getVersionIRI))
  }

  def write(ontology: OWLOntology, filename: String): Unit = {
    val ontManager = ontology.getOWLOntologyManager()
    ontManager.saveOntology(ontology, new RDFXMLOntologyFormat(), new FileDocumentTarget(new File(filename)))
  }

  def cd(dir: String): Unit = {
    cd(new File(dir))
  }

  def cd(dir: File): Unit = {
    System.setProperty("user.dir", dir.getAbsolutePath())
  }

  def step(message: String): Unit = {
    println(new Date() + ": " + message)
  }

  def isTboxAxiom(axiom: OWLAxiom): Boolean = axiom.isOfType(AxiomType.TBoxAxiomTypes)

}

case class SourcedAxioms(axioms: Set[OWLAxiom], ont: IRI, ontVersion: Option[IRI])

object SourcedAxioms {

  def apply(ont: OWLOntology): SourcedAxioms = SourcedAxioms(ont.getAxioms.toSet, ont.getOntologyID.getOntologyIRI, Option(ont.getOntologyID.getVersionIRI))

}