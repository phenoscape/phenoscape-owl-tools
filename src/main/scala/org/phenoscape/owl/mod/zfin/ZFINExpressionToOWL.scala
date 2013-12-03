package org.phenoscape.owl.mod.zfin

import java.io.File
import scala.collection.JavaConversions._
import scala.collection.Set
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.collection.mutable
import scala.io.Source
import org.apache.commons.lang3.StringUtils
import org.phenoscape.scowl.OWL._
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.OBOUtil
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.apibinding.OWLManager

object ZFINExpressionToOWL extends OWLTask {

  val geneExpression = Class(Vocab.GENE_EXPRESSION)
  val zebrafish = Individual(Vocab.ZEBRAFISH)
  val manager = OWLManager.createOWLOntologyManager()

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile(args(0), "ISO-8859-1")
    val ontology = convert(file)
    file.close()
    manager.saveOntology(ontology, IRI.create(new File(args(1))))
  }

  def convert(expressionData: Source): OWLOntology = {
    val axioms = expressionData.getLines.map(translate(_)).flatten.toSet[OWLAxiom]
    manager.createOntology(axioms, IRI.create("http://purl.obolibrary.org/obo/phenoscape/zfin_gene_expression.owl"))
  }

  def translate(expressionLine: String): Set[OWLAxiom] = {
    val items = expressionLine.split("\t", -1)
    val axioms = mutable.Set[OWLAxiom]()
    if (items(0).startsWith("ZDB-EFG")) {
      return axioms
    } else {
      val expression = nextIndividual()
      axioms.add(factory.getOWLDeclarationAxiom(expression))
      axioms.add(expression Type geneExpression)
      val structure = nextIndividual()
      axioms.add(factory.getOWLDeclarationAxiom(structure))
      axioms.add(expression Fact (OCCURS_IN, structure))
      val superStructureID = StringUtils.stripToNull(items(3))
      val subStructureID = StringUtils.stripToNull(items(5))
      if (subStructureID == null) {
        val structureType = Class(OBOUtil.iriForTermID(superStructureID))
        axioms.add(structure Type structureType)
      } else {
        val superStructure = Class(OBOUtil.iriForTermID(superStructureID))
        val subStructure = Class(OBOUtil.iriForTermID(subStructureID))
        val structureType = nextClass()
        axioms.add(structureType SubClassOf (subStructure and (part_of some superStructure)))
        axioms.add(structure Type structureType)
      }
      val geneIRI = OBOUtil.zfinIRI(StringUtils.stripToNull(items(0)))
      val gene = Individual(geneIRI)
      axioms.add(factory.getOWLDeclarationAxiom(gene))
      axioms.add(expression Fact (ASSOCIATED_WITH_GENE, gene))
      axioms.add(expression Fact (ASSOCIATED_WITH_TAXON, zebrafish))
      val publicationID = StringUtils.stripToNull(items(10))
      val publication = Individual(OBOUtil.zfinIRI(publicationID))
      axioms.add(expression Fact (dcSource, publication))
      return axioms
    }
  }

}