package org.phenoscape.owl.scripts

import scala.io.Source
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.AxiomType
import scala.collection.JavaConversions._
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassExpression
import org.phenoscape.kb.ingest.util.ExpressionUtil
import org.phenoscape.owl.util.ExpressionsUtil
import org.phenoscape.scowl._
import scalaz.Success
import scalaz.Failure
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom

object ParseProfileSemantics {

  def tboxWithSemanticsForProfiles(profiles: OWLOntology): Set[OWLAxiom] = {
    for {
      axiom <- profiles.getAxioms(AxiomType.CLASS_ASSERTION).toSet[OWLClassAssertionAxiom]
      classAxiom <- axiomsFor(axiom.getClassExpression)
    } yield classAxiom
  }

  def axiomsFor(expression: OWLClassExpression): Set[OWLAxiom] = for {
    owlClass <- expression.getClassesInSignature.toSet[OWLClass]
    axiom <- axiomsForNamed(owlClass)
  } yield axiom

  def axiomsForNamed(owlClass: OWLClass): Set[OWLAxiom] = {
    val iriString = owlClass.getIRI.toString
    val expanded = if (iriString.startsWith(ExpressionUtil.namedExpressionPrefix)) {
      for {
        expression <- ExpressionsUtil.expressionForName(owlClass)
      } yield axiomsFor(expression) + (owlClass EquivalentTo expression)
    } else if (iriString.startsWith(ExpressionUtil.namedSubClassPrefix)) {
      for {
        expression <- ExpressionsUtil.expressionForName(owlClass)
      } yield axiomsFor(expression) + (owlClass SubClassOf expression)
    } else {
      Success(Set.empty[OWLAxiom])
    }
    expanded match {
      case Success(axioms) => axioms
      case Failure(message) => {
        println(s"Problem: $message")
        Set.empty[OWLAxiom]
      }
    }
  }

}