package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConversions._
import scala.collection.Set
import scala.collection.mutable
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

object MaterializeSubClassOfClosure extends OWLTask {

  val manager = this.getOWLOntologyManager();

  def main(args: Array[String]): Unit = {
    val targetFile = new File(System.getProperty("org.phenoscape.owl.MaterializeSubClassOfClosure.target"));
    val source = manager.loadOntologyFromOntologyDocument(new File(args(0)));
    println(manager.getOntologies().map(_.getAxiomCount()).reduce(_ + _));
    //val reasoner = new ElkReasonerFactory().createReasoner(source);
    val target = oldMethod(source);
    //val target = newMethod(source);
    println("Done creating ontology.");
    manager.saveOntology(target, IRI.create(targetFile));
    System.exit(0);
  }

  def materialize(source: OWLOntology): OWLOntology = {
    oldMethod(source);
  }

  def oldMethod(source: OWLOntology): OWLOntology = {
    val reasoner = new ElkReasonerFactory().createReasoner(source);
    val allClasses = source.getClassesInSignature(true);
    val axioms = mutable.ListBuffer[OWLAxiom]();
    for (ontClass <- allClasses) {
      axioms.addAll(createSubClassOfAxioms(ontClass, reasoner));
    }
    reasoner.dispose();
    val axiomsSet = axioms.toSet;
    val target = manager.createOntology(axiomsSet);
    return target;
  }

  def newMethod(source: OWLOntology): OWLOntology = {
    val reasoner = new ElkReasonerFactory().createReasoner(source);
    val allClasses = source.getClassesInSignature(true);
    val doneClasses = mutable.Set[OWLClass]();
    val axioms: Set[OWLAxiom] = createSubClassOfAxioms(factory.getOWLThing(), Set[OWLClass](), reasoner, doneClasses);
    reasoner.dispose();
    val target = manager.createOntology(axioms);
    return target;
  }

  def createSubClassOfAxioms(owlClass: OWLClass, superClasses: Set[OWLClass], reasoner: OWLReasoner, doneClasses: mutable.Set[OWLClass]): Set[OWLAxiom] = {
    val axioms = mutable.Set[OWLAxiom]();
    val firstTime = doneClasses.add(owlClass);
    if (!firstTime) {
      return axioms;
    } else {
      val newSuperClasses = mutable.Set[OWLClass]();
      newSuperClasses.addAll(superClasses);
      newSuperClasses.add(owlClass);
      val directSubClasses = reasoner.getSubClasses(owlClass, true).getFlattened();
      for (subclass <- directSubClasses) {
        axioms.addAll(newSuperClasses.map(factory.getOWLSubClassOfAxiom(subclass, _)));
      }
      val recursiveAxioms: Set[OWLAxiom] = directSubClasses.map(createSubClassOfAxioms(_, newSuperClasses, reasoner, doneClasses)).flatten;
      axioms.addAll(recursiveAxioms);
      return axioms;
    }
  }

  def createSubClassOfAxioms(owlClass: OWLClass, reasoner: OWLReasoner): Set[OWLSubClassOfAxiom] = {
    //val ontology = reasoner.getRootOntology();
    //val axioms = reasoner.getSuperClasses(owlClass, false).getFlattened().map(factory.getOWLSubClassOfAxiom(owlClass, _)).filterNot(ontology.containsAxiomIgnoreAnnotations(_, true));
    val axioms = reasoner.getSuperClasses(owlClass, false).getFlattened().map(factory.getOWLSubClassOfAxiom(owlClass, _));
    axioms.add(factory.getOWLSubClassOfAxiom(owlClass, owlClass));
    return axioms;
  }

}