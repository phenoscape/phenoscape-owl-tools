package org.phenoscape.owl

import scala.collection.JavaConverters._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.phenoscape.owl.Vocab.has_part
import java.io.File
import java.io.FileOutputStream
import scala.collection.JavaConverters._
import org.semanticweb.owlapi.model.parameters.Imports


object NegationHierarchyAsserter {

  val factory = OWLManager.getOWLDataFactory
  val Negates = factory.getOWLAnnotationProperty(Vocab.NEGATES)


  def main(args: Array[String]): Unit = {

    val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
    val inputOntology: OWLOntology = manager.loadOntologyFromOntologyDocument(new File(args(0)))
    val axioms: Set[OWLAxiom] = inputOntology.getAxioms(Imports.INCLUDED).asScala.toSet
    val negationAxioms: Set[OWLAxiom] = assertNegationHierarchy(axioms)
    val negationOntology: OWLOntology = manager.createOntology(negationAxioms.asJava)
    manager.saveOntology(negationOntology, new FileOutputStream(("negationAxiomsFile")))
  }

  def assertNegationHierarchy(axioms: Set[OWLAxiom]): Set[OWLAxiom] = {
    //  create tuples (class expressions, named classes)
    val classTuples = for {
      EquivalentClasses(_, expr) <- axioms
      //  extract named classes and expressions
      expressions = expr.collect { case hasPartAxiom@ObjectSomeValuesFrom(`has_part`, _) => hasPartAxiom }
      if expressions.nonEmpty
      namedClasses = expr.collect { case owlClass: OWLClass => owlClass }
      namedClass <- namedClasses
      expression <- expressions
    } yield (expression, namedClass)
    // map (class expression -> named classes)
    val classMap = buildIndex(classTuples)

    //  create tuples (named class, named negation classes)
    val negatesPairs = for {
      EquivalentClasses(_, expr) <- axioms
      expressions = expr.collect { case ObjectComplementOf(hasPartAxiom@ObjectSomeValuesFrom(`has_part`, _)) => hasPartAxiom }
      if expressions.nonEmpty
      namedNegationClasses = expr.collect { case owlClass: OWLClass => owlClass }
      expression <- expressions
      namedClass <- classMap.getOrElse(expression, Set.empty)
      namedNegationClass <- namedNegationClasses
    } yield (namedNegationClass.getIRI, namedClass.getIRI)

    val negatesIndex = buildIndex(negatesPairs)
    val negatedByIndex = buildReverseIndex(negatesPairs)

    val superToSubclassPairs = for {
      subClassOfAxiom@SubClassOf(_, subclass@Class(_), superclass@Class(_)) <- axioms
    } yield (superclass, subclass)
    val subclassesIndex = buildIndex(superToSubclassPairs)

    val subclassAxioms = for {
      AnnotationAssertion(_, Negates, negater: IRI, negated: IRI) <- axioms
      subClassOfNegatedClass <- subclassesIndex(Class(negated))
      superClassOfOntClassIRI <- negatedByIndex(subClassOfNegatedClass.getIRI)
    } yield Class(negater) SubClassOf Class(superClassOfOntClassIRI)

    val equivalentClassAxioms = for {
      equivAxiom@EquivalentClasses(_, _) <- axioms
      classes = equivAxiom.getNamedClasses.asScala
      if classes.size > 1
    } yield {
      val negations = classes.flatMap(c => negatedByIndex(c.getIRI))
      factory.getOWLEquivalentClassesAxiom(negations.map(Class(_)).asJava)
    }
    subclassAxioms ++ equivalentClassAxioms
  }

  def buildIndex[A, B](pairs: Iterable[(A, B)]): Map[A, Set[B]] =
    pairs.foldLeft(emptyIndex[A, B]()) { case (index, (a, b)) => index.updated(a, (index(a) + b)) }

  def buildReverseIndex[A, B](pairs: Iterable[(A, B)]): Map[B, Set[A]] =
    pairs.foldLeft(emptyIndex[B, A]()) { case (index, (a, b)) => index.updated(b, (index(b) + a)) }

  def emptyIndex[A, B](): Map[A, Set[B]] = Map.empty.withDefaultValue(Set.empty)


}