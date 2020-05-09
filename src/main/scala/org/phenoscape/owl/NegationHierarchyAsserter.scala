package org.phenoscape.owl

import java.io.{File, FileOutputStream}

import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports

import scala.collection.JavaConverters._

object NegationHierarchyAsserter {

  val factory = OWLManager.getOWLDataFactory
  val Negates = factory.getOWLAnnotationProperty(Vocab.NEGATES)

  def main(args: Array[String]): Unit = {

    val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
    val inputOntology: OWLOntology = manager.loadOntologyFromOntologyDocument(new File(args(0)))
    val axioms: Set[OWLAxiom] = inputOntology.getAxioms(Imports.INCLUDED).asScala.toSet
    val negationAxioms: Set[OWLAxiom] = assertNegationHierarchy(axioms)
    val negationOntology: OWLOntology = manager.createOntology(negationAxioms.asJava)
    manager.saveOntology(negationOntology, new FileOutputStream(args(1))) //  negation axioms file
  }

  def assertNegationHierarchy(axioms: Set[OWLAxiom]): Set[OWLAxiom] = {
    //  create tuples (class expressions, named classes)
    val classTuples = for {
      EquivalentClasses(_, expr) <- axioms
      //  extract named classes and expressions
      namedClasses: Set[OWLClass] = expr.collect { case owlClass: OWLClass => owlClass }
      namedClass: OWLClass <- namedClasses
      expression: OWLClassExpression <- expr
    } yield (expression, namedClass)
    // map (class expression -> named classes)
    val classMap = buildIndex(classTuples)

    //  create tuples (named class, named negation classes)
    val negatesPairs = for {
      EquivalentClasses(_, expr) <- axioms
      expressions = expr.collect { case ObjectComplementOf(negated) => negated }
      if expressions.nonEmpty
      namedNegationClasses = expr.collect { case owlClass: OWLClass => owlClass }
      expression <- expressions
      expressionAsNamed = Set(expression).collect { case n: OWLClass => n }
      namedClass <- classMap.getOrElse(expression, Set.empty) ++ expressionAsNamed
      namedNegationClass <- namedNegationClasses
    } yield (namedNegationClass.getIRI, namedClass.getIRI)

    val negatedByIndex = buildIndex(negatesPairs.map(_.swap))

    val superToSubclassPairs = for {
      SubClassOf(_, subclass @ Class(_), superclass @ Class(_)) <- axioms
    } yield (superclass, subclass)
    val subclassesIndex = buildIndex(superToSubclassPairs)

    val subclassAxioms = for {
      (negater, negated) <- negatesPairs
      subClassOfNegatedClass <- subclassesIndex(Class(negated))
      superClassOfOntClassIRI <- negatedByIndex(subClassOfNegatedClass.getIRI)
    } yield Class(negater) SubClassOf Class(superClassOfOntClassIRI)

    val equivalentClassAxioms = for {
      equivAxiom @ EquivalentClasses(_, _) <- axioms
      classes = equivAxiom.getNamedClasses.asScala
      if classes.size > 1
    } yield {
      val negations = classes.flatMap(c => negatedByIndex(c.getIRI))
      factory.getOWLEquivalentClassesAxiom(negations.map(Class(_)).asJava)
    }
    subclassAxioms ++ equivalentClassAxioms
  }

  def buildIndex[A, B](pairs: Iterable[(A, B)]): Map[A, Set[B]] =
    pairs.foldLeft(emptyIndex[A, B]) { case (index, (a, b)) => index.updated(a, index(a) + b) }

  def emptyIndex[A, B]: Map[A, Set[B]] = Map.empty.withDefaultValue(Set.empty)

}
