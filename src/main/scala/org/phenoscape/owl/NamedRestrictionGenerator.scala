package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConversions._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLAxiom
import org.phenoscape.scowl.OWL._

object NamedRestrictionGenerator extends OWLTask {

  val manager = this.createOWLOntologyManager

  def main(args: Array[String]): Unit = {
    val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)))
    val property = manager.getOWLDataFactory().getOWLObjectProperty(IRI.create(args(1)))
    val restrictionsOntology = generateRestrictions(ontology, property)
    manager.saveOntology(restrictionsOntology, IRI.create(new File(args(2))))
  }

  def generateRestrictions(ontology: OWLOntology, property: OWLObjectProperty): OWLOntology = {
    val newIRI = property.getIRI.toString + "_some_" + ontology.getOntologyID.getOntologyIRI.toString
    val newAxioms = for {
      ontClass <- ontology.getClassesInSignature(false)
      axiom <- createRestriction(property, ontClass)
    } yield axiom
    manager.createOntology(newAxioms, IRI.create(newIRI))
  }

  def createRestriction(property: OWLObjectProperty, ontClass: OWLClass): Set[OWLAxiom] = {
    val annotationProperty = factory.getOWLAnnotationProperty(IRI.create(property.getIRI.toString + "_some"))
    val newClassIRI = getRestrictionIRI(property.getIRI, ontClass.getIRI)
    val namedRestriction = factory.getOWLClass(newClassIRI)
    val equivAxiom = (namedRestriction EquivalentTo (property some ontClass))
    val annotation = namedRestriction Annotation (annotationProperty, ontClass.getIRI())
    Set(equivAxiom, annotation)
  }

  def getRestrictionIRI(propertyIRI: IRI, classIRI: IRI): IRI = {
    return IRI.create(propertyIRI.toString + "_some_" + classIRI.toString)
  }

}
