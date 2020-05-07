package org.phenoscape.owl.util

import java.io.StringWriter
import java.net.URI

import scala.collection.JavaConverters._
import org.apache.http.client.utils.URLEncodedUtils
import org.phenoscape.kb.ingest.util.OntUtil
import org.phenoscape.owlet.ManchesterSyntaxClassExpressionParser
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.renderer.ManchesterOWLSyntaxObjectRenderer
import org.semanticweb.owlapi.model.OWLAnnotationProperty
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLIndividual
import org.semanticweb.owlapi.model.OWLLiteral
import org.semanticweb.owlapi.model.OWLObject
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologySetProvider
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.search.EntitySearcher
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider

import scalaz._

object ExpressionsUtil {

  val factory = OWLManager.getOWLDataFactory

  def expressionForName(named: OWLClass): Validation[String, OWLClassExpression] = {
    val parameters = URLEncodedUtils.parse(URI.create(named.getIRI.toString), "UTF-8")
    parameters.asScala
      .collectFirst({
        case pair if pair.getName == "value" => pair.getValue
      })
      .map(ManchesterSyntaxClassExpressionParser.parse)
      .getOrElse(Validation.failure("Invalid expression URI"))
  }

  def instantiateClassAssertion(individual: OWLIndividual, aClass: OWLClassExpression): Set[OWLAxiom] = {
    var axioms = Set.empty[OWLAxiom]
    aClass match {
      case ObjectSomeValuesFrom(property, filler) =>
        val value = OntUtil.nextIndividual()
        axioms += factory.getOWLObjectPropertyAssertionAxiom(property, individual, value)
        axioms ++= instantiateClassAssertion(value, filler)
      case ObjectAllValuesFrom(property, filler)  =>
        val value = OntUtil.nextIndividual()
        axioms += factory.getOWLObjectPropertyAssertionAxiom(property, individual, value)
        axioms ++= instantiateClassAssertion(value, filler)
      case ObjectIntersectionOf(operands)         =>
        operands.foreach(o => axioms ++= instantiateClassAssertion(individual, o))
      case _                                      => axioms += factory.getOWLClassAssertionAxiom(aClass, individual)
    }
    axioms
  }

  def annotationsFor(obj: OWLEntity, property: OWLAnnotationProperty, ont: OWLOntology): Iterable[String] =
    EntitySearcher
      .getAnnotations(obj, ont, property)
      .asScala
      .map(_.getValue)
      .collect({ case literal: OWLLiteral => literal.getLiteral.toString })

  def labelFor(obj: OWLEntity, ont: OWLOntology): Option[String] =
    annotationsFor(obj, factory.getRDFSLabel, ont).headOption

  def labelFor(obj: OWLEntity, onts: Iterable[OWLOntology]): Option[String] =
    (onts flatMap (labelFor(obj, _))).headOption

  private class OntologyProvider(ont: OWLOntology) extends OWLOntologySetProvider {
    val onts = Set(ont).asJava
    override def getOntologies() = onts
  }

  def createEntityRenderer(property: OWLAnnotationProperty, ont: OWLOntology): OWLObject => String = {
    val shortFormProvider = new AnnotationValueShortFormProvider(
      List(property).asJava,
      Map.empty[OWLAnnotationProperty, java.util.List[String]].asJava,
      new OntologyProvider(ont)
    )
    entity => {
      val writer = new StringWriter()
      val renderer = new ManchesterOWLSyntaxObjectRenderer(writer, shortFormProvider)
      entity.accept(renderer)
      writer.close()
      writer.toString.replaceAll("\n", " ").replaceAll("\\s+", " ")
    }
  }

  def permute(expression: OWLClassExpression)(implicit reasoner: OWLReasoner): Set[OWLClassExpression] =
    expression match {
      case namedClass: OWLClass                    =>
        (reasoner.getSuperClasses(namedClass, true).getFlattened.asScala.toSet + namedClass).toSet[OWLClassExpression]
      case someValuesFrom: OWLObjectSomeValuesFrom => permute(someValuesFrom)
      case intersection: OWLObjectIntersectionOf   => permute(intersection)
      case _                                       => Set(expression)
    }

  def permute(expression: OWLObjectSomeValuesFrom)(implicit reasoner: OWLReasoner): Set[OWLObjectSomeValuesFrom] = {
    val property = expression.getProperty
    reasoner
      .getSuperClasses(expression.getFiller, true)
      .getFlattened
      .asScala
      .toSet[OWLClass]
      .map(property some _) + expression
  }

  def permute(expression: OWLObjectIntersectionOf)(implicit reasoner: OWLReasoner): Set[OWLObjectIntersectionOf] = {
    val permutedOperands = expression.getOperands.asScala.toSet[OWLClassExpression].map(permute)
    val combinations = allCombinations(permutedOperands)
    combinations.map(set => factory.getOWLObjectIntersectionOf(set.asJava)) + expression
  }

  def allCombinations[T](sets: Set[Set[T]]): Set[Set[T]] = {
    def combine[T](combinations: Set[Set[T]], itemsToCombine: Set[T]): Set[Set[T]] =
      for {
        combination <- combinations
        item <- itemsToCombine
      } yield combination + item
    sets.foldLeft(Set[Set[T]]())(combine)
  }

}
