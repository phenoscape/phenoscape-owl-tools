package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConversions._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom
import org.semanticweb.owlapi.model.OWLAxiom
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom

object NegationHierarchyAsserter extends OWLTask {

  val negates = factory.getOWLAnnotationProperty(Vocab.NEGATES)

  def assertNegationHierarchy(axioms: Set[OWLAxiom]): Set[OWLAxiom] = {
    val negatesPairs = for {
      annotationAxiom <- axioms.collect { case ax: OWLAnnotationAssertionAxiom => ax }
      if annotationAxiom.getProperty == negates
    } yield (annotationAxiom.getSubject.asInstanceOf[IRI], annotationAxiom.getValue.asInstanceOf[IRI])
    val negatesIndex = buildIndex(negatesPairs)
    val negatedByIndex = buildReverseIndex(negatesPairs)
    val superToSubclassPairs = for {
      subClassOfAxiom <- axioms.collect { case ax: OWLSubClassOfAxiom => ax }
      if !subClassOfAxiom.getSubClass.isAnonymous
      if !subClassOfAxiom.getSuperClass.isAnonymous
    } yield (subClassOfAxiom.getSuperClass.asOWLClass, subClassOfAxiom.getSubClass.asOWLClass)
    val subclassesIndex = buildIndex(superToSubclassPairs)
    val subclassAxioms = for {
      term <- axioms.flatMap(_.getClassesInSignature)
      axiom <- createSubclassOfAxioms(term, negatesIndex, negatedByIndex, subclassesIndex)
    } yield axiom

    val equivalentClassAxioms = for {
      equivAxiom <- axioms.collect { case ax: OWLEquivalentClassesAxiom => ax }
      classes = equivAxiom.getNamedClasses
      if classes.size > 1
    } yield {
      val negations = classes.flatMap(c => negatedByIndex(c.getIRI))
      factory.getOWLEquivalentClassesAxiom(negations.map(Class(_)))
    }

    subclassAxioms ++ equivalentClassAxioms
  }

  def createSubclassOfAxioms(ontClass: OWLClass, negatesIndex: Map[IRI, Set[IRI]], negatedByIndex: Map[IRI, Set[IRI]], subclassesIndex: Map[OWLClass, Set[OWLClass]]): Set[OWLSubClassOfAxiom] = {
    for {
      negatedClassIRI <- negatesIndex(ontClass.getIRI)
      subClassOfNegatedClass <- subclassesIndex(Class(negatedClassIRI))
      if !subClassOfNegatedClass.isAnonymous
      superClassOfOntClassIRI <- negatedByIndex(subClassOfNegatedClass.asOWLClass.getIRI)
    } yield ontClass SubClassOf Class(superClassOfOntClassIRI)
  }

  def buildIndex[A, B](pairs: Iterable[(A, B)]): Map[A, Set[B]] =
    pairs.foldLeft(emptyIndex[A, B]()) { case (index, (a, b)) => index.updated(a, (index(a) + b)) }

  def buildReverseIndex[A, B](pairs: Iterable[(A, B)]): Map[B, Set[A]] =
    pairs.foldLeft(emptyIndex[B, A]()) { case (index, (a, b)) => index.updated(b, (index(b) + a)) }

  def emptyIndex[A, B](): Map[A, Set[B]] = Map.empty.withDefaultValue(Set.empty)

}