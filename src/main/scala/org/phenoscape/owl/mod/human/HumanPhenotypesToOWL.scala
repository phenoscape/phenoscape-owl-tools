package org.phenoscape.owl.mod.human

import scala.collection.mutable
import scala.io.Source

import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.OBOUtil
import org.phenoscape.owl.util.OntologyUtil
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

object HumanPhenotypesToOWL extends OWLTask {

  val rdfsLabel = factory.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI())
  val human = factory.getOWLNamedIndividual(Vocab.HUMAN)

  def convert(phenotypeData: Source): Set[OWLAxiom] = phenotypeData.getLines.drop(1).flatMap(translate).toSet[OWLAxiom]

  def translate(phenotypeLine: String): Set[OWLAxiom] = {
    val items = phenotypeLine.split("\t")
    val axioms = mutable.Set[OWLAxiom]()
    val phenotype = OntologyUtil.nextIndividual()
    axioms.add(phenotype Type AnnotatedPhenotype)
    axioms.add(factory.getOWLDeclarationAxiom(phenotype))
    val phenotypeID = StringUtils.stripToNull(items(3))
    val phenotypeClass = Class(OBOUtil.iriForTermID(phenotypeID))
    axioms.add(phenotype Type phenotypeClass)
    val geneIRI = IRI.create("http://www.ncbi.nlm.nih.gov/gene/" + StringUtils.stripToNull(items(0)))
    val geneSymbol = StringUtils.stripToNull(items(1))
    axioms.add(geneIRI Annotation (rdfsLabel, factory.getOWLLiteral(geneSymbol)))
    val gene = Individual(geneIRI)
    axioms.add(gene Type Gene)
    axioms.add(factory.getOWLDeclarationAxiom(gene))
    axioms.add(phenotype Fact (associated_with_gene, gene))
    axioms.add(phenotype Fact (associated_with_taxon, human))
    return axioms.toSet
  }

}