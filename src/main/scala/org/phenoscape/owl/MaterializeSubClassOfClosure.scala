package org.phenoscape.owl

import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.OWLReasoner

import java.io.File
import scala.collection.{mutable, Set}
import scala.jdk.CollectionConverters._

object MaterializeSubClassOfClosure {

  val factory = OWLManager.getOWLDataFactory
  val manager = OWLManager.createOWLOntologyManager

  def main(args: Array[String]): Unit = {
    val targetFile = new File(System.getProperty("org.phenoscape.owl.MaterializeSubClassOfClosure.target"))
    val source = manager.loadOntologyFromOntologyDocument(new File(args(0)))
    println(manager.getOntologies().asScala.map(_.getAxiomCount()).reduce(_ + _))
    //val reasoner = new ElkReasonerFactory().createReasoner(source)
    val target = oldMethod(source)
    //val target = newMethod(source)
    println("Done creating ontology.")
    manager.saveOntology(target, IRI.create(targetFile))
    System.exit(0)
  }

  def materialize(source: OWLOntology): OWLOntology =
    oldMethod(source)

  def oldMethod(source: OWLOntology): OWLOntology = {
    val reasoner = new ElkReasonerFactory().createReasoner(source)
    val allClasses = source.getClassesInSignature(Imports.INCLUDED)
    val axioms = for {
      ontClass <- allClasses.asScala
      axiom <- createSubClassOfAxioms(ontClass, reasoner)
    } yield axiom
    reasoner.dispose()
    val target = manager.createOntology(axioms.toSet[OWLAxiom].asJava)
    target
  }

  def newMethod(source: OWLOntology): OWLOntology = {
    val reasoner = new ElkReasonerFactory().createReasoner(source)
    val allClasses = source.getClassesInSignature(Imports.INCLUDED)
    val doneClasses = mutable.Set[OWLClass]()
    val axioms: Set[OWLAxiom] = createSubClassOfAxioms(factory.getOWLThing, Set[OWLClass](), reasoner, doneClasses)
    reasoner.dispose()
    val target = manager.createOntology(axioms.asJava)
    target
  }

  def createSubClassOfAxioms(
    owlClass: OWLClass,
    superClasses: Set[OWLClass],
    reasoner: OWLReasoner,
    doneClasses: mutable.Set[OWLClass]
  ): Set[OWLAxiom] = {
    val axioms = mutable.Set[OWLAxiom]()
    val firstTime = doneClasses.add(owlClass)
    if (!firstTime)
      axioms
    else {
      val newSuperClasses = mutable.Set[OWLClass]()
      newSuperClasses.asJava.addAll(superClasses.asJava)
      newSuperClasses.add(owlClass)
      val directSubClasses = reasoner.getSubClasses(owlClass, true).getFlattened
      for (subclass <- directSubClasses.asScala)
        axioms.asJava.addAll(newSuperClasses.map(factory.getOWLSubClassOfAxiom(subclass, _)).asJava)
      val recursiveAxioms: Set[OWLAxiom] =
        directSubClasses.asScala.map(createSubClassOfAxioms(_, newSuperClasses, reasoner, doneClasses)).flatten
      axioms.asJava.addAll(recursiveAxioms.asJava)
      axioms
    }
  }

  def createSubClassOfAxioms(owlClass: OWLClass, reasoner: OWLReasoner): Set[OWLSubClassOfAxiom] = {
    val axioms = for {
      superClass <- reasoner.getSuperClasses(owlClass, false).getFlattened.asScala
    } yield factory.getOWLSubClassOfAxiom(owlClass, superClass)
    axioms + factory.getOWLSubClassOfAxiom(owlClass, owlClass)
  }

}
