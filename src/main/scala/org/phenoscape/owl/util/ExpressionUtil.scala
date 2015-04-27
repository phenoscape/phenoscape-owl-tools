package org.phenoscape.owl.util

import java.io.StringWriter
import java.util.ArrayList
import scala.collection.JavaConversions._
import scala.collection.mutable
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLAnnotationProperty
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLClassExpressionVisitor
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLIndividual
import org.semanticweb.owlapi.model.OWLLiteral
import org.semanticweb.owlapi.model.OWLObject
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologySetProvider
import org.semanticweb.owlapi.model.OWLQuantifiedObjectRestriction
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider
import org.semanticweb.owlapi.util.ShortFormProvider
import uk.ac.manchester.cs.owl.owlapi.mansyntaxrenderer.ManchesterOWLSyntaxOWLObjectRendererImpl
import uk.ac.manchester.cs.owl.owlapi.mansyntaxrenderer.ManchesterOWLSyntaxObjectRenderer
import java.net.URLEncoder
import java.net.URLDecoder
import java.util.regex.Pattern
import org.phenoscape.owlet.ManchesterSyntaxClassExpressionParser
import scalaz.Validation
import java.net.URL
import java.net.URI
import org.apache.http.client.utils.URLEncodedUtils
import java.util.UUID

object ExpressionUtil {

  val factory = OWLManager.getOWLDataFactory
  val namedExpressionPrefix = "http://purl.org/phenoscape/expression?value="
  val namedSubClassPrefix = "http://purl.org/phenoscape/subclassof?value="

  def nameForExpression(expression: OWLClassExpression): OWLClass = name(expression, namedExpressionPrefix, false)

  def nameForSubClassOf(expression: OWLClassExpression): OWLClass = name(expression, namedSubClassPrefix, false)

  private def name(expression: OWLClassExpression, prefix: String, unique: Boolean): OWLClass = expression match {
    case named: OWLClass => named
    case _ => {
      val writer = new StringWriter()
      val renderer = new ManchesterOWLSyntaxObjectRenderer(writer, FullIRIProvider)
      renderer.setUseWrapping(false)
      expression.accept(renderer: OWLClassExpressionVisitor)
      writer.close()
      val fragment = if (unique) s"#${UUID.randomUUID.toString}" else ""
      Class(s"$prefix${URLEncoder.encode(writer.toString, "UTF-8")}$fragment")
    }
  }

  def nameForExpressionWithAxioms(expression: OWLClassExpression): (OWLClass, Set[OWLAxiom]) = expression match {
    case named: OWLClass => (named, Set.empty)
    case _ => {
      val named = nameForExpression(expression)
      (named, Set(named EquivalentTo expression))
    }
  }

  def uniqueNameForExpressionWithAxioms(expression: OWLClassExpression): (OWLClass, Set[OWLAxiom]) = expression match {
    case named: OWLClass => (named, Set.empty)
    case _ => {
      val named = name(expression, namedExpressionPrefix, true)
      (named, Set(named EquivalentTo expression))
    }
  }

  def nameForSubClassWithAxioms(expression: OWLClassExpression): (OWLClass, Set[OWLAxiom]) = expression match {
    case named: OWLClass => (named, Set.empty)
    case _ => {
      val named = nameForSubClassOf(expression)
      (named, Set(named SubClassOf expression))
    }
  }

  def expressionForName(named: OWLClass): Validation[String, OWLClassExpression] = {
    val parameters = URLEncodedUtils.parse(URI.create(named.getIRI.toString), "UTF-8")
    parameters.collectFirst({
      case pair if pair.getName == "value" => pair.getValue
    }).map(ManchesterSyntaxClassExpressionParser.parse).getOrElse(Validation.failure("Invalid expression URI"))
  }

  def instantiateClassAssertion(individual: OWLIndividual, aClass: OWLClassExpression): scala.collection.Set[OWLAxiom] = {
    val axioms = mutable.Set[OWLAxiom]()
    if (aClass.isInstanceOf[OWLQuantifiedObjectRestriction]) { // either someValuesFrom or allValuesFrom
      val restriction = aClass.asInstanceOf[OWLQuantifiedObjectRestriction]
      val filler = restriction.getFiller
      val property = restriction.getProperty
      // need IRIs for individuals for type materialization
      val value = OntologyUtil.nextIndividual()
      axioms.add(factory.getOWLObjectPropertyAssertionAxiom(property, individual, value))
      axioms.addAll(instantiateClassAssertion(value, filler))
    } else if (aClass.isInstanceOf[OWLObjectIntersectionOf]) {
      for (operand <- (aClass.asInstanceOf[OWLObjectIntersectionOf]).getOperands) {
        axioms.addAll(instantiateClassAssertion(individual, operand))
      }
    } else {
      axioms.add(factory.getOWLClassAssertionAxiom(aClass, individual))
    }
    return axioms
  }

  def annotationsFor(obj: OWLEntity, property: OWLAnnotationProperty, ont: OWLOntology): Iterable[String] =
    obj.getAnnotations(ont, property).map(_.getValue).collect(
      { case literal: OWLLiteral => literal.getLiteral.toString })

  def labelFor(obj: OWLEntity, ont: OWLOntology): Option[String] = annotationsFor(obj, factory.getRDFSLabel, ont).headOption

  def labelFor(obj: OWLEntity, onts: Iterable[OWLOntology]): Option[String] = (onts flatMap (labelFor(obj, _))).headOption

  private class OntologyProvider(ont: OWLOntology) extends OWLOntologySetProvider {
    val onts = Set(ont)
    override def getOntologies() = onts
  }

  def createEntityRenderer(property: OWLAnnotationProperty, ont: OWLOntology): OWLObject => String = {
    val shortFormProvider = new AnnotationValueShortFormProvider(List(property), Map[OWLAnnotationProperty, ArrayList[String]](), new OntologyProvider(ont))
    val renderer = new ManchesterOWLSyntaxOWLObjectRendererImpl()
    renderer.setShortFormProvider(shortFormProvider)
    entity => renderer.render(entity).replaceAll("\n", " ").replaceAll("\\s+", " ")
  }

  def permute(expression: OWLClassExpression)(implicit reasoner: OWLReasoner): Set[OWLClassExpression] = expression match {
    case namedClass: OWLClass                    => reasoner.getSuperClasses(namedClass, true).getFlattened.toSet + namedClass
    case someValuesFrom: OWLObjectSomeValuesFrom => permute(someValuesFrom)
    case intersection: OWLObjectIntersectionOf   => permute(intersection)
    case _                                       => Set(expression)
  }

  def permute(expression: OWLObjectSomeValuesFrom)(implicit reasoner: OWLReasoner): Set[OWLObjectSomeValuesFrom] = {
    val property = expression.getProperty
    reasoner.getSuperClasses(expression.getFiller, true).getFlattened.toSet[OWLClass].map(property some _) + expression
  }

  def permute(expression: OWLObjectIntersectionOf)(implicit reasoner: OWLReasoner): Set[OWLObjectIntersectionOf] = {
    val permutedOperands = expression.getOperands.toSet[OWLClassExpression].map(permute)
    val combinations = allCombinations(permutedOperands)
    combinations.map(factory.getOWLObjectIntersectionOf(_)) + expression
  }

  def allCombinations[T](sets: Set[Set[T]]): Set[Set[T]] = {
    def combine[T](combinations: Set[Set[T]], itemsToCombine: Set[T]): Set[Set[T]] = for {
      combination <- combinations
      item <- itemsToCombine
    } yield combination + item
    sets.foldLeft(Set[Set[T]]())(combine)
  }

  object FullIRIProvider extends ShortFormProvider {

    def getShortForm(entity: OWLEntity): String = s"<${entity.getIRI.toString}>"

    def dispose(): Unit = Unit

  }

}