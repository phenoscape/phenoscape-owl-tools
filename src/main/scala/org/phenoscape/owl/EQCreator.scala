package org.phenoscape.owl

import scala.collection.JavaConverters._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.model.IRI
import org.apache.log4j.BasicConfigurator
import org.apache.log4j.Logger
import org.apache.log4j.Level
import java.io.File
import java.util.Date
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLAxiom
import java.io.FileOutputStream
import java.io.BufferedOutputStream
import java.util.zip.GZIPOutputStream
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.reasoner.InferenceType
import Vocab._

//takes at least 200 GB memory
object EQCreator {

  def main(args: Array[String]): Unit = {
    BasicConfigurator.configure();
    Logger.getRootLogger().setLevel(Level.ERROR);
    println(new Date() + ": starting");
    val manager = OWLManager.createOWLOntologyManager();
    val factory = manager.getOWLDataFactory();
    val eqs = manager.createOntology();
    manager.addAxiom(eqs, factory.getOWLTransitiveObjectPropertyAxiom(has_part));
    manager.addAxiom(eqs, factory.getOWLReflexiveObjectPropertyAxiom(has_part));
    manager.addAxiom(eqs, factory.getOWLTransitiveObjectPropertyAxiom(part_of));
    manager.addAxiom(eqs, factory.getOWLReflexiveObjectPropertyAxiom(part_of));
    manager.addAxiom(eqs, factory.getOWLInverseObjectPropertiesAxiom(has_part, part_of));
    val uberon = manager.loadOntologyFromOntologyDocument(new File("uberon.owl"));
    val pato = manager.loadOntologyFromOntologyDocument(new File("pato.owl"));
    manager.applyChange(new AddImport(eqs, factory.getOWLImportsDeclaration(uberon.getOntologyID.getOntologyIRI.get)));
    manager.applyChange(new AddImport(eqs, factory.getOWLImportsDeclaration(pato.getOntologyID.getOntologyIRI.get)));
    val anatomicalEntity = Class(IRI.create("http://purl.obolibrary.org/obo/UBERON_0001062"));
    val qualityRoot = Class(IRI.create("http://purl.obolibrary.org/obo/PATO_0000001"));
    val uberonReasoner = new ElkReasonerFactory().createReasoner(uberon);
    val patoReasoner = new ElkReasonerFactory().createReasoner(pato);
    val anatomicalEntities = uberonReasoner.getSubClasses(anatomicalEntity, false).getFlattened();
    uberonReasoner.dispose();
    val qualities = patoReasoner.getSubClasses(qualityRoot, false).getFlattened();
    patoReasoner.dispose();
    println(new Date() + ": building");
    for (entity <- anatomicalEntities.asScala; quality <- qualities.asScala) {
      manager.addAxiom(eqs, createEQ(entity, quality));
    }
    println(new Date() + ": saving EQs");
    val outputStream = new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(new File(args(0) + ".gz"))));
    manager.saveOntology(eqs, outputStream);
    outputStream.close();
    println(new Date() + ": done saving");
    val hierarchy = manager.createOntology();
    println(new Date() + ": starting classification");
    val eqReasoner = new ElkReasonerFactory().createReasoner(eqs);
    eqReasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY); //
    println(new Date() + ": done classification");
    //MaterializeInferences.materializeInferences(hierarchy, eqReasoner);
    println(new Date() + ": starting axiom generation");
    val allClasses = eqs.getClassesInSignature(true).asScala;
    for (owlClass <- allClasses) {
      val newAxioms = eqReasoner.getSuperClasses(owlClass, true).getFlattened().asScala.map(owlClass SubClassOf _).filterNot(eqs.containsAxiom(_, true));
      manager.addAxioms(hierarchy, newAxioms.asJava);
    }
    println(new Date() + ": done axiom generation");
    println(new Date() + ": saving hierarchy");
    val hierarchyOutputStream = new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(new File(args(1) + ".gz"))));
    manager.saveOntology(hierarchy, hierarchyOutputStream);
    hierarchyOutputStream.close();
    println(new Date() + ": done saving");
    System.exit(0);
  }

  def createEQ(entity: OWLClass, quality: OWLClass): OWLAxiom = {
    val eqClass = Class(IRI.create(entity.getIRI().toString() + "+" + quality.getIRI().toString()));
    eqClass EquivalentTo (has_part some ((part_of some entity) and (bearer_of some quality)));
  }

}