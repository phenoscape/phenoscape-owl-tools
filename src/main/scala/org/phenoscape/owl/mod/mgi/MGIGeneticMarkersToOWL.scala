package org.phenoscape.owl.mod.mgi

import scala.collection.mutable
import scala.io.Source

import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.Vocab._
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

object MGIGeneticMarkersToOWL extends OWLTask {

  val rdfsLabel = factory.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI())
  val hasExactSynonym = factory.getOWLAnnotationProperty(HAS_EXACT_SYNONYM)
  val hasRelatedSynonym = factory.getOWLAnnotationProperty(HAS_RELATED_SYNONYM)

  def convert(markersData: Source): Set[OWLAxiom] = {
    markersData.getLines.flatMap(translate).toSet[OWLAxiom]
  }

  def translate(line: String): Set[OWLAxiom] = {
    val items = line.split("\t")
    val axioms = mutable.Set[OWLAxiom]()
    if (items(9) != "Gene") {
      return axioms.toSet
    } else {
      val geneID = StringUtils.stripToNull(items(0))
      val geneSymbol = StringUtils.stripToNull(items(6))
      val geneFullName = StringUtils.stripToNull(items(8))
      val geneIRI = getGeneIRI(geneID)
      val gene = factory.getOWLNamedIndividual(geneIRI)
      axioms.add(factory.getOWLDeclarationAxiom(gene))
      axioms.add(factory.getOWLClassAssertionAxiom(Gene, gene))
      axioms.add(factory.getOWLAnnotationAssertionAxiom(rdfsLabel, geneIRI, factory.getOWLLiteral(geneSymbol)))
      axioms.add(factory.getOWLAnnotationAssertionAxiom(hasExactSynonym, geneIRI, factory.getOWLLiteral(geneFullName)))
      if (items.size > 11) {
        val synonymsField = StringUtils.stripToEmpty(items(11))
        synonymsField.split("\\|").foreach(synonym => {
          axioms.add(factory.getOWLAnnotationAssertionAxiom(hasRelatedSynonym, geneIRI, factory.getOWLLiteral(synonym)))
        })
      }
      return axioms.toSet
    }
  }

  def getGeneIRI(geneID: String): IRI = {
    return IRI.create("http://www.informatics.jax.org/marker/" + geneID)
  }

}