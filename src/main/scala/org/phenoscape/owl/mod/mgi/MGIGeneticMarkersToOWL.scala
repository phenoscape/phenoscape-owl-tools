package org.phenoscape.owl.mod.mgi

import java.io.File

import scala.collection.JavaConversions._
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.collection.Set
import scala.collection.mutable
import scala.io.Source

import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.Vocab
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

object MGIGeneticMarkersToOWL extends OWLTask {

  val manager = this.getOWLOntologyManager();
  val rdfsLabel = factory.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI());
  val hasExactSynonym = factory.getOWLAnnotationProperty(Vocab.HAS_EXACT_SYNONYM);
  val hasRelatedSynonym = factory.getOWLAnnotationProperty(Vocab.HAS_RELATED_SYNONYM);
  val geneClass = factory.getOWLClass(Vocab.GENE);

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile(args(0), "utf-8");
    val ontology = convert(file);
    file.close();
    manager.saveOntology(ontology, IRI.create(new File(args(1))));
  }

  def convert(markersData: Source): OWLOntology = {
    val ontology = manager.createOntology(IRI.create("http://purl.obolibrary.org/obo/phenoscape/mgi_genes.owl"));
    manager.addAxioms(ontology, markersData.getLines.map(translate(_)).flatten.toSet[OWLAxiom]);
    return ontology;
  }

  def translate(line: String): Set[OWLAxiom] = {
    val items = line.split("\t");
    val axioms = mutable.Set[OWLAxiom]();
    if (items(9) != "Gene") {
      return axioms;
    } else {
      val geneID = StringUtils.stripToNull(items(0));
      val geneSymbol = StringUtils.stripToNull(items(6));
      val geneFullName = StringUtils.stripToNull(items(8));
      val geneIRI = getGeneIRI(geneID);
      val gene = factory.getOWLNamedIndividual(geneIRI);
      axioms.add(factory.getOWLDeclarationAxiom(gene));
      axioms.add(factory.getOWLClassAssertionAxiom(geneClass, gene));
      axioms.add(factory.getOWLAnnotationAssertionAxiom(rdfsLabel, geneIRI, factory.getOWLLiteral(geneSymbol)));
      axioms.add(factory.getOWLAnnotationAssertionAxiom(hasExactSynonym, geneIRI, factory.getOWLLiteral(geneFullName)));
      if (items.size > 11) {
        val synonymsField = StringUtils.stripToEmpty(items(11));
        synonymsField.split("\\|").foreach(synonym => {
          axioms.add(factory.getOWLAnnotationAssertionAxiom(hasRelatedSynonym, geneIRI, factory.getOWLLiteral(synonym)));
        });
      }
      return axioms;
    }
  }

  def getGeneIRI(geneID: String): IRI = {
    return IRI.create("http://www.informatics.jax.org/marker/" + geneID);
  }

}