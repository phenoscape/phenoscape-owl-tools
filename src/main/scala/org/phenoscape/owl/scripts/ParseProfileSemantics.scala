package org.phenoscape.owl.scripts

import org.phenoscape.kb.ingest.util.ExpressionUtil
import org.phenoscape.owl.util.ExpressionsUtil
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model._
import scalaz.{Failure, Success}

import scala.collection.JavaConverters._

object ParseProfileSemantics {

  def tboxWithSemanticsForProfiles(profiles: OWLOntology): Set[OWLAxiom] =
    for {
      axiom <- profiles.getAxioms(AxiomType.CLASS_ASSERTION).asScala.toSet[OWLClassAssertionAxiom]
      classAxiom <- axiomsFor(axiom.getClassExpression)
    } yield classAxiom

  def axiomsFor(expression: OWLClassExpression): Set[OWLAxiom] =
    for {
      owlClass <- expression.getClassesInSignature.asScala.toSet[OWLClass]
      axiom <- axiomsForNamed(owlClass)
    } yield axiom

  def axiomsForNamed(owlClass: OWLClass): Set[OWLAxiom] = {
    val iriString = owlClass.getIRI.toString
    val expanded =
      if (iriString.startsWith(ExpressionUtil.namedExpressionPrefix))
        for {
          expression <- ExpressionsUtil.expressionForName(owlClass)
        } yield axiomsFor(expression) + (owlClass EquivalentTo expression)
      else if (iriString.startsWith(ExpressionUtil.namedSubClassPrefix))
        for {
          expression <- ExpressionsUtil.expressionForName(owlClass)
        } yield axiomsFor(expression) + (owlClass SubClassOf expression)
      else
        Success(Set.empty[OWLAxiom])
    expanded match {
      case Success(axioms) => axioms
      case Failure(message) =>
        println(s"Problem: $message")
        Set.empty[OWLAxiom]
    }
  }

}
