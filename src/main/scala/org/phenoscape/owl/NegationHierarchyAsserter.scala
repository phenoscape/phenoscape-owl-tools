package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConversions._
import scala.collection.Set
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom
import org.semanticweb.owlapi.model.OWLAxiom
import org.phenoscape.scowl.OWL._

object NegationHierarchyAsserter extends OWLTask {

  val negates = factory.getOWLAnnotationProperty(Vocab.NEGATES)

  def main(args: Array[String]): Unit = {
    val manager = this.getOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)))
    assertNegationHierarchy(ontology)
    if (args.size > 1) {
      manager.saveOntology(ontology, IRI.create(new File(args(1))))
    } else {
      manager.saveOntology(ontology)
    }
  }

  def assertNegationHierarchy(ontologies: OWLOntology*): Set[OWLAxiom] = {
    val allClasses = ontologies flatMap (_.getClassesInSignature(true))
    val negatesPairs = for {
      ont <- ontologies
      annotationAxiom <- ont.getAxioms(AxiomType.ANNOTATION_ASSERTION, false)
      if annotationAxiom.getProperty() == negates
    } yield (annotationAxiom.getSubject().asInstanceOf[IRI], annotationAxiom.getValue().asInstanceOf[IRI])
    val emptyIndex = Map[IRI, Set[IRI]]().withDefaultValue(Set())
    val negatesIndex = negatesPairs.foldLeft(emptyIndex) { case (index, (negator, negated)) => index.updated(negator, (index(negator) + negated)) }
    val negatedByIndex = negatesPairs.foldLeft(emptyIndex) { case (index, (negator, negated)) => index.updated(negated, (index(negated) + negator))}
    val axioms = allClasses.flatMap(createSubclassOfAxioms(_, negatesIndex, negatedByIndex, ontologies.toSet))
    axioms.toSet
  }

  def createSubclassOfAxioms(ontClass: OWLClass, negatesIndex: Map[IRI, Set[IRI]], negatedByIndex: Map[IRI, Set[IRI]], ontologies: Set[OWLOntology]): Set[OWLSubClassOfAxiom] = {
    for {
      negatedClassIRI <- negatesIndex.getOrElse(ontClass.getIRI, Set())
      subClassOfNegatedClass <- Class(negatedClassIRI).getSubClasses(ontologies)
      if !subClassOfNegatedClass.isAnonymous()
      superClassOfOntClassIRI <- negatedByIndex.getOrElse(subClassOfNegatedClass.asOWLClass.getIRI, Set())
    } yield ontClass SubClassOf Class(superClassOfOntClassIRI)
  }

}