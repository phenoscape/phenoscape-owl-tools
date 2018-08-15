package org.phenoscape.owl

import scala.collection.JavaConverters._

import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom

object NegationHierarchyAsserter extends OWLTask {

  val Negates = factory.getOWLAnnotationProperty(Vocab.NEGATES)

  def assertNegationHierarchy(axioms: Set[OWLAxiom]): Set[OWLAxiom] = {
    val negatesPairs = for {
      AnnotationAssertion(_, Negates, subject: IRI, value: IRI) <- axioms
    } yield (subject, value)
    val negatesIndex = buildIndex(negatesPairs)
    val negatedByIndex = buildReverseIndex(negatesPairs)
    val superToSubclassPairs = for {
      subClassOfAxiom @ SubClassOf(_, subclass @ Class(_), superclass @ Class(_)) <- axioms
    } yield (superclass, subclass)
    val subclassesIndex = buildIndex(superToSubclassPairs)

    val subclassAxioms = for {
      AnnotationAssertion(_, Negates, negater: IRI, negated: IRI) <- axioms
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
    pairs.foldLeft(emptyIndex[A, B]()) { case (index, (a, b)) => index.updated(a, (index(a) + b)) }

  def buildReverseIndex[A, B](pairs: Iterable[(A, B)]): Map[B, Set[A]] =
    pairs.foldLeft(emptyIndex[B, A]()) { case (index, (a, b)) => index.updated(b, (index(b) + a)) }

  def emptyIndex[A, B](): Map[A, Set[B]] = Map.empty.withDefaultValue(Set.empty)

}