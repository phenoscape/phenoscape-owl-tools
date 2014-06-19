package org.phenoscape.owl.util

import scala.annotation.migration
import scala.collection.JavaConverters.setAsJavaSetConverter
import scala.util.parsing.combinator.RegexParsers

import org.apache.log4j.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom

object PostCompositionParser extends RegexParsers {

  private val factory = OWLManager.getOWLDataFactory

  def iri: Parser[IRI] = """[^\(\)\^]+""".r ^^ OBOUtil.iriForTermID

  def classIRI: Parser[OWLClass] = iri ^^ factory.getOWLClass

  def objectPropertyIRI: Parser[OWLObjectProperty] = iri ^^ factory.getOWLObjectProperty

  def conjunction: Parser[OWLClassExpression] = repsep(primary, "^") ^^ (in => in.size match {
    case 1 => in.head
    case _ => factory.getOWLObjectIntersectionOf(in.toSet.asJava)
  })
  
  def description: Parser[OWLClassExpression] = conjunction | primary

  def primary: Parser[OWLClassExpression] = someValuesFrom | classIRI

  def someValuesFrom: Parser[OWLObjectSomeValuesFrom] = objectPropertyIRI ~ ("(" ~> description <~ ")") ^^ {
    case prop ~ filler => factory.getOWLObjectSomeValuesFrom(prop, filler)
  }

  def parseExpression(expression: String): Option[OWLClassExpression] = {
    parseAll(conjunction, expression) match {
      case Success(result, remainder) => Option(result)
      case NoSuccess(message, remainder) => {
        logger.error("Failed to parse class expression: " + message)
        None
      }
    }
  }

  lazy val logger = Logger.getLogger(this.getClass)

}
