package org.phenoscape.owl.util

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.Set
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf
import org.semanticweb.owlapi.model.OWLIndividual
import org.semanticweb.owlapi.model.OWLQuantifiedObjectRestriction
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.OWLTask
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.phenoscape.owl.OWLTask

object ExpressionUtil {

  val factory = OWLManager.getOWLDataFactory();

  def instantiateClassAssertion(individual: OWLIndividual, aClass: OWLClassExpression, context: OWLTask): Set[OWLAxiom] = {
    val axioms = mutable.Set[OWLAxiom]();
    if (aClass.isInstanceOf[OWLQuantifiedObjectRestriction]) { // either someValuesFrom or allValuesFrom
      val restriction = aClass.asInstanceOf[OWLQuantifiedObjectRestriction];
      val filler = restriction.getFiller();
      val property = restriction.getProperty();
      // need IRIs for individuals for type materialization
      val value = context.nextIndividual();
      axioms.add(this.factory.getOWLObjectPropertyAssertionAxiom(property, individual, value));
      axioms.addAll(instantiateClassAssertion(value, filler, context));
    } else if (aClass.isInstanceOf[OWLObjectIntersectionOf]) {
      for (operand <- (aClass.asInstanceOf[OWLObjectIntersectionOf]).getOperands()) {
        axioms.addAll(instantiateClassAssertion(individual, operand, context));
      }
    } else {
      axioms.add(this.factory.getOWLClassAssertionAxiom(aClass, individual));
    }
    return axioms;
  }

}