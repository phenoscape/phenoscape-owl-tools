package org.phenoscape.owl.mod.mgi

import java.io.File

import scala.collection.JavaConversions._
import scala.collection.Set
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.collection.mutable
import scala.io.Source

import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.Vocab.AnnotatedPhenotype
import org.phenoscape.owl.Vocab.associated_with_gene
import org.phenoscape.owl.Vocab.associated_with_taxon
import org.phenoscape.owl.Vocab.dcSource
import org.phenoscape.owl.util.OBOUtil
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology

object MGIPhenotypesToOWL extends OWLTask {

  val mouse = Individual(Vocab.MOUSE)
  val manager = OWLManager.createOWLOntologyManager()

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile(args(0), "utf-8")
    val ontology = convert(file)
    file.close()
    manager.saveOntology(ontology, IRI.create(new File(args(1))))
  }

  def convert(phenotypeData: Source): OWLOntology = manager.createOntology(
    phenotypeData.getLines.drop(1).map(translate).flatten.toSet[OWLAxiom],
    IRI.create("http://purl.obolibrary.org/obo/phenoscape/mgi_phenotypes.owl"))

  def translate(expressionLine: String): Set[OWLAxiom] = {
    val items = expressionLine.split("\t", -1)
    val involved = mutable.Set[OWLClass]()
    val axioms = mutable.Set[OWLAxiom]()
    val phenotype = nextIndividual()
    axioms.add(phenotype Type AnnotatedPhenotype)
    axioms.add(factory.getOWLDeclarationAxiom(phenotype))
    val phenotypeID = StringUtils.stripToNull(items(10))
    val phenotypeClass = Class(OBOUtil.iriForTermID(phenotypeID))
    axioms.add(phenotype Type phenotypeClass)
    val geneIRI = MGIGeneticMarkersToOWL.getGeneIRI(StringUtils.stripToNull(items(0)))
    val gene = Individual(geneIRI)
    axioms.add(factory.getOWLDeclarationAxiom(gene))
    axioms.add(phenotype Fact (associated_with_gene, gene))
    axioms.add(phenotype Fact (associated_with_taxon, mouse))
    val publicationID = StringUtils.stripToNull(items(11))
    val publication = Individual(OBOUtil.mgiReferenceIRI(publicationID))
    axioms.add(phenotype Fact (dcSource, publication))
    axioms
  }

}