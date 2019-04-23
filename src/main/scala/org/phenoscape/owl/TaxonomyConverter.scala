package org.phenoscape.owl

//import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.Set
import scala.collection.mutable
import org.semanticweb.owlapi.model.OWLOntology
import java.io.File
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLAxiom
import java.util.UUID
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.apibinding.OWLManager
import org.phenoscape.owl.Vocab._
import org.semanticweb.owlapi.search.EntitySearcher

object TaxonomyConverter extends OWLTask {

  def main(args: Array[String]): Unit = {
    val manager = OWLManager.createOWLOntologyManager();
    val classOntology = manager.loadOntologyFromOntologyDocument(new File(args(0)));
    val instanceOntology = createInstanceOntology(classOntology);
    manager.saveOntology(instanceOntology, IRI.create(new File(args(1))));
  }

  def createInstanceOntology(classOntology: OWLOntology): OWLOntology = {
    val manager = classOntology.getOWLOntologyManager();
    val instanceOntology = manager.createOntology(IRI.create("http://example.org/" + UUID.randomUUID().toString()));
    val allClasses = classOntology.getClassesInSignature(false).asScala;
    val axioms = allClasses.map(translateTaxonClass(_, classOntology));
    axioms.foreach(axs => manager.addAxioms(instanceOntology, axs.asJava));
    return instanceOntology;
  }

  def translateTaxonClass(taxonClass: OWLClass, classOntology: OWLOntology): Set[OWLAxiom] = {
    val manager = classOntology.getOWLOntologyManager();
    val factory = manager.getOWLDataFactory();
    val axioms = Set[OWLAxiom]();
    onlyClasses(EntitySearcher.getSuperClasses(taxonClass, classOntology).asScala).map(createSubcladeRelationship(taxonClass, _)).toSet[OWLAxiom] + factory.getOWLClassAssertionAxiom(Taxon, factory.getOWLNamedIndividual(taxonClass.getIRI()));
//    axioms + onlyClasses(EntitySearcher.getSuperClasses(taxonClass, classOntology).asScala).map(createSubcladeRelationship(taxonClass, _));
//    return axioms;
  }

  def createSubcladeRelationship(subclade: OWLClass, superclade: OWLClass): OWLObjectPropertyAssertionAxiom = {
    val factory = OWLManager.getOWLDataFactory();
    val subcladeIndividual = factory.getOWLNamedIndividual(subclade.getIRI());
    val supercladeIndividual = factory.getOWLNamedIndividual(superclade.getIRI());
    return factory.getOWLObjectPropertyAssertionAxiom(subclade_of, subcladeIndividual, supercladeIndividual);
  }

  def onlyClasses(classes: Iterable[OWLClassExpression]): Iterable[OWLClass] = {
    classes.filter(!_.isAnonymous()).map(_.asOWLClass());
  }

}