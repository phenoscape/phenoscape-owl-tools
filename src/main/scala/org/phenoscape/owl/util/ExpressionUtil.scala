package org.phenoscape.owl.util

import java.io.StringWriter
import java.util.ArrayList
import scala.collection.JavaConversions._
import scala.collection.Set
import scala.collection.mutable
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.OWLTask
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLAnnotationProperty
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLIndividual
import org.semanticweb.owlapi.model.OWLLiteral
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologySetProvider
import org.semanticweb.owlapi.model.OWLQuantifiedObjectRestriction
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider
import uk.ac.manchester.cs.owl.owlapi.mansyntaxrenderer.ManchesterOWLSyntaxObjectRenderer
import uk.ac.manchester.cs.owl.owlapi.mansyntaxrenderer.ManchesterOWLSyntaxOWLObjectRendererImpl
import org.semanticweb.owlapi.model.OWLObject

object ExpressionUtil {

  val factory = OWLManager.getOWLDataFactory

  def instantiateClassAssertion(individual: OWLIndividual, aClass: OWLClassExpression, context: OWLTask): Set[OWLAxiom] = {
    val axioms = mutable.Set[OWLAxiom]()
    if (aClass.isInstanceOf[OWLQuantifiedObjectRestriction]) { // either someValuesFrom or allValuesFrom
      val restriction = aClass.asInstanceOf[OWLQuantifiedObjectRestriction]
      val filler = restriction.getFiller
      val property = restriction.getProperty
      // need IRIs for individuals for type materialization
      val value = context.nextIndividual()
      axioms.add(factory.getOWLObjectPropertyAssertionAxiom(property, individual, value))
      axioms.addAll(instantiateClassAssertion(value, filler, context))
    } else if (aClass.isInstanceOf[OWLObjectIntersectionOf]) {
      for (operand <- (aClass.asInstanceOf[OWLObjectIntersectionOf]).getOperands) {
        axioms.addAll(instantiateClassAssertion(individual, operand, context))
      }
    } else {
      axioms.add(factory.getOWLClassAssertionAxiom(aClass, individual))
    }
    return axioms
  }

  def labelFor(obj: OWLEntity, ont: OWLOntology): Option[String] =
    obj.getAnnotations(ont, factory.getRDFSLabel).map(_.getValue).collect(
      { case literal: OWLLiteral => literal.getLiteral.toString }).headOption

  def labelFor(obj: OWLEntity, onts: Iterable[OWLOntology]): Option[String] = (onts flatMap (labelFor(obj, _))).headOption

  private class OntologyProvider(ont: OWLOntology) extends OWLOntologySetProvider {
    lazy val onts = Set(ont)
    override def getOntologies() = onts
  }

  def createEntityRenderer(property: OWLAnnotationProperty, ont: OWLOntology): OWLObject => String = {
    val shortFormProvider = new AnnotationValueShortFormProvider(List(property), Map[OWLAnnotationProperty, ArrayList[String]](), new OntologyProvider(ont))
    val renderer = new ManchesterOWLSyntaxOWLObjectRendererImpl()
    entity => renderer.render(entity)
  }

}