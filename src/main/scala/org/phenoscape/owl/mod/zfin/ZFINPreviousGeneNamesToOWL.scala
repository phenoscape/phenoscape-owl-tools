package org.phenoscape.owl.mod.zfin

import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntology
import scala.io.Source
import scala.collection.JavaConversions._
import scala.collection.mutable
import org.phenoscape.owl.OWLTask
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary
import org.phenoscape.owl.Vocab
import java.io.File
import org.apache.commons.lang3.StringUtils
import org.semanticweb.owlapi.apibinding.OWLManager

object ZFINPreviousGeneNamesToOWL extends OWLTask {

  val manager = OWLManager.createOWLOntologyManager()
  val hasRelatedSynonym = factory.getOWLAnnotationProperty(Vocab.HAS_RELATED_SYNONYM)

  def convert(data: Source): Set[OWLAxiom] = data.getLines.flatMap(translate).toSet

  def translate(line: String): Set[OWLAxiom] = {
    val items = line.split("\t")
    val axioms = mutable.Set[OWLAxiom]()
    if (!items(0).startsWith("ZDB-GENE")) {
      axioms.toSet
    } else {
      val geneID = StringUtils.stripToNull(items(0))
      val previousName = StringUtils.stripToNull(items(3))
      val geneIRI = IRI.create("http://zfin.org/" + geneID)
      val gene = factory.getOWLNamedIndividual(geneIRI)
      axioms.add(factory.getOWLDeclarationAxiom(gene))
      axioms.add(factory.getOWLAnnotationAssertionAxiom(hasRelatedSynonym, geneIRI, factory.getOWLLiteral(previousName)))
      axioms.toSet
    }
  }

}