package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.Set
import scala.collection.Map
import scala.collection.mutable
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom
import org.semanticweb.owlapi.model.OWLAxiom

object NegationHierarchyAsserter extends OWLTask {

  val negates = factory.getOWLAnnotationProperty(Vocab.NEGATES);

  def main(args: Array[String]): Unit = {
    val manager = this.getOWLOntologyManager();
    val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)));
    assertNegationHierarchy(ontology);
    if (args.size > 1) {
      manager.saveOntology(ontology, IRI.create(new File(args(1))));
    } else {
      manager.saveOntology(ontology);
    }
  }

  def assertNegationHierarchy(ontologies: OWLOntology*): Set[OWLAxiom] = {
    val allClasses = ontologies.map(_.getClassesInSignature(true)).flatten;
    val negatesIndex: mutable.Map[IRI, mutable.Set[IRI]] = mutable.Map();
    val negatedByIndex: mutable.Map[IRI, mutable.Set[IRI]] = mutable.Map();
    ontologies.foreach(ont => {
      val annotations = ont.getAxioms(AxiomType.ANNOTATION_ASSERTION, false).filter(_.getProperty() == negates);
      annotations.foreach(annot => {
        negatesIndex.getOrElseUpdate(annot.getSubject().asInstanceOf[IRI], mutable.Set()).add(annot.getValue().asInstanceOf[IRI]);
        negatedByIndex.getOrElseUpdate(annot.getValue().asInstanceOf[IRI], mutable.Set()).add(annot.getSubject().asInstanceOf[IRI]);
      });
    });
    val axioms = allClasses.map(createSubclassOfAxioms(_, negatesIndex, negatedByIndex, ontologies.toSet));
    axioms.toSet.flatten;
  }

  def createSubclassOfAxioms(ontClass: OWLClass, negatesIndex: Map[IRI, Set[IRI]], negatedByIndex: Map[IRI, Set[IRI]], ontologies: Set[OWLOntology]): Iterable[OWLSubClassOfAxiom] = {
    val factory = OWLManager.getOWLDataFactory();
    val superClasses = negatesIndex.get(ontClass.getIRI()).toSet.flatten.map(factory.getOWLClass(_)).map(_.getSubClasses(ontologies).filter(!_.isAnonymous()).map(_.asOWLClass())).flatten.map(c => negatedByIndex.get(c.getIRI()).toSet.flatten.map(factory.getOWLClass(_))).flatten;
    superClasses.map(factory.getOWLSubClassOfAxiom(ontClass, _));
  }

}