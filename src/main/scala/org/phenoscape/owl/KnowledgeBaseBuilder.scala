package org.phenoscape.owl

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.util.Date

import scala.collection.JavaConverters._
import org.openrdf.model.URI
import org.openrdf.repository.sail.SailRepositoryConnection
import org.openrdf.rio.RDFFormat
import org.phenoscape.kb.ingest.util.OBOUtil
import org.phenoscape.owl.util.NullIRIMapper
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat
import org.semanticweb.owlapi.formats.RioRDFXMLDocumentFormat
import org.semanticweb.owlapi.io.FileDocumentTarget
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologyManager
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.model.OWLOntologyID

class KnowledgeBaseBuilder extends App {

  private[this] val globalManager = OWLManager.createOWLOntologyManager
  globalManager.clearIRIMappers()
  globalManager.addIRIMapper(NullIRIMapper)

  def getManager: OWLOntologyManager = globalManager

  def iri(string: String): IRI = IRI.create(string)

  def combine(ontologies: OWLOntology*): OWLOntology =
    if (ontologies.size == 1)
      ontologies(0)
    else {
      val newManager = OWLManager.createOWLOntologyManager
      newManager.createOntology(ontologies.flatMap(_.getAxioms().asScala).toSet.asJava)
    }

  def combine(ontology: SourcedAxioms, ontologies: SourcedAxioms*): OWLOntology =
    OWLManager.createOWLOntologyManager().createOntology((ontology.axioms ++ ontologies.flatMap(_.axioms)).asJava)

  def reasoner(ontologies: OWLOntology*): OWLReasoner = {
    val allAxioms = combine(ontologies: _*)
    new ElkReasonerFactory().createReasoner(allAxioms)
  }

  def reasoner(axioms: Set[OWLAxiom]): OWLReasoner =
    new ElkReasonerFactory().createReasoner(OWLManager.createOWLOntologyManager().createOntology(axioms.asJava))

  def loadNormalized(location: File): OWLOntology = {
    val ont             = globalManager.loadOntologyFromOntologyDocument(location)
    val definedByAxioms = ont.getClassesInSignature().asScala.flatMap(OBOUtil.createDefinedByAnnotation)
    globalManager.addAxioms(ont, definedByAxioms.asJava)
    PropertyNormalizer.normalize(ont)
  }

  def loadFromWeb(iri: IRI, excludeImports: Boolean): SourcedAxioms = {
    val manager = OWLManager.createOWLOntologyManager()
    // Even if we are excluding axioms from imports, we want to initially include imports in case property types rely on declarations there
    val ont = manager.loadOntology(iri)
    if (!excludeImports) {
      val importsAxioms = ont.getImports.asScala.flatMap(_.getAxioms().asScala)
      manager.addAxioms(ont, importsAxioms.asJava)
    }
    val definedByAxioms = ont.getClassesInSignature(Imports.EXCLUDED).asScala.flatMap(OBOUtil.createDefinedByAnnotation)
    manager.addAxioms(ont, definedByAxioms.asJava)
    PropertyNormalizer.normalize(ont)
    SourcedAxioms(ont.getAxioms(Imports.EXCLUDED).asScala.toSet, ont.getOntologyID)
  }

  def write(ontology: OWLOntology, file: File): Unit = {
    val ontManager = ontology.getOWLOntologyManager()
    ontManager.saveOntology(ontology, new RDFXMLDocumentFormat(), new FileDocumentTarget(file))
  }

  def step(message: String): Unit =
    println(new Date() + ": " + message)

  def isTboxAxiom(axiom: OWLAxiom): Boolean = axiom.isOfType(AxiomType.TBoxAxiomTypes)

  def addTriples(ontology: OWLOntology, db: SailRepositoryConnection, graph: URI): Unit = {
    val manager   = ontology.getOWLOntologyManager
    val outStream = new ByteArrayOutputStream()
    manager.saveOntology(ontology, new RioRDFXMLDocumentFormat(), outStream)
    outStream.close()
    val inStream = new ByteArrayInputStream(outStream.toByteArray())
    db.add(inStream, "", RDFFormat.RDFXML, graph)
    inStream.close()
  }

  def addTriples(axioms: SourcedAxioms, db: SailRepositoryConnection, graph: URI): Unit =
    addTriples(axioms.axioms, db, graph, axioms.ontologyID)

  def addTriples(
    axioms: Iterable[OWLAxiom],
    db: SailRepositoryConnection,
    graph: URI,
    ontID: OWLOntologyID = new OWLOntologyID()
  ): Unit = {
    val manager = OWLManager.createOWLOntologyManager()
    val ont     = manager.createOntology(ontID)
    manager.addAxioms(ont, axioms.toSet[OWLAxiom].asJava)
    addTriples(ont, db, graph)
  }

}

case class SourcedAxioms(axioms: Set[OWLAxiom], ontologyID: OWLOntologyID)

object SourcedAxioms {

  def apply(ont: OWLOntology): SourcedAxioms = SourcedAxioms(ont.getAxioms().asScala.toSet, ont.getOntologyID)

}
