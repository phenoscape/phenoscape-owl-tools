package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConverters._
import scala.collection.Set
import scala.collection.mutable
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager

object MaterializeSubClassOfClosure extends OWLTask {

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

  def materialize(source: OWLOntology): OWLOntology = {
    oldMethod(source)
  }

  def oldMethod(source: OWLOntology): OWLOntology = {
    val reasoner = new ElkReasonerFactory().createReasoner(source)
    val allClasses = source.getClassesInSignature(true)
    val axioms = for {
      ontClass <- allClasses.asScala
      axiom <- createSubClassOfAxioms(ontClass, reasoner)
    } yield axiom
    reasoner.dispose()
    val target = manager.createOntology(axioms.toSet[OWLAxiom].asJava)
    return target
  }

  def newMethod(source: OWLOntology): OWLOntology = {
    val reasoner = new ElkReasonerFactory().createReasoner(source)
    val allClasses = source.getClassesInSignature(true)
    val doneClasses = mutable.Set[OWLClass]()
    val axioms: Set[OWLAxiom] = createSubClassOfAxioms(factory.getOWLThing(), Set[OWLClass](), reasoner, doneClasses)
    reasoner.dispose()
    val target = manager.createOntology(axioms.asJava)
    return target
  }

  def createSubClassOfAxioms(owlClass: OWLClass, superClasses: Set[OWLClass], reasoner: OWLReasoner, doneClasses: mutable.Set[OWLClass]): Set[OWLAxiom] = {
    val axioms = mutable.Set[OWLAxiom]()
    val firstTime = doneClasses.add(owlClass)
    if (!firstTime) {
      axioms
    } else {
      val newSuperClasses = mutable.Set[OWLClass]()
      newSuperClasses.asJava.addAll(superClasses.asJava)
      newSuperClasses.add(owlClass)
      val directSubClasses = reasoner.getSubClasses(owlClass, true).getFlattened()
      for (subclass <- directSubClasses.asScala) {
        axioms.asJava.addAll(newSuperClasses.map(factory.getOWLSubClassOfAxiom(subclass, _)).asJava)
      }
      val recursiveAxioms: Set[OWLAxiom] = directSubClasses.asScala.map(createSubClassOfAxioms(_, newSuperClasses, reasoner, doneClasses)).flatten
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