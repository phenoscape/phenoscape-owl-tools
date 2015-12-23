package org.phenoscape.owl.mod.zfin

import java.io.File
import scala.collection.JavaConversions._
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
import org.phenoscape.owl.util.OntologyUtil
import org.phenoscape.owl.util.ExpressionUtil

object ZFINExpressionToOWL extends OWLTask {

  val manager = OWLManager.createOWLOntologyManager()

  def convert(expressionData: Source): Set[OWLAxiom] = expressionData.getLines.flatMap(translate).toSet[OWLAxiom]

  def translate(expressionLine: String): Set[OWLAxiom] = {
    val items = expressionLine.split("\t", -1)
    val axioms = mutable.Set[OWLAxiom]()
    if (items(0).startsWith("ZDB-EFG")) {
      return axioms.toSet
    } else {
      val expression = OntologyUtil.nextIndividual()
      axioms.add(factory.getOWLDeclarationAxiom(expression))
      axioms.add(expression Type GeneExpression)
      val structure = OntologyUtil.nextIndividual()
      axioms.add(factory.getOWLDeclarationAxiom(structure))
      axioms.add(expression Fact (occurs_in, structure))
      val superStructureID = Option(StringUtils.stripToNull(items(3))).filter(_ != "\\").get
      val subStructureIDOpt = Option(StringUtils.stripToNull(items(5))).filter(_ != "\\")
      subStructureIDOpt match {
        case Some(subStructureID) => {
          val superStructure = Class(OBOUtil.iriForTermID(superStructureID))
          val subStructure = Class(OBOUtil.iriForTermID(subStructureID))
          val (structureType, structureAxioms) = ExpressionUtil.nameForExpressionWithAxioms(subStructure and (part_of some superStructure))
          axioms.add(structure Type structureType)
          axioms ++= structureAxioms
        }
        case None => {
          val structureType = Class(OBOUtil.iriForTermID(superStructureID))
          axioms.add(structure Type structureType)
        }
      }
      val geneIRI = OBOUtil.zfinIRI(StringUtils.stripToNull(items(0)))
      val gene = Individual(geneIRI)
      axioms.add(factory.getOWLDeclarationAxiom(gene))
      axioms.add(expression Fact (associated_with_gene, gene))
      axioms.add(expression Fact (associated_with_taxon, Zebrafish))
      val publicationID = StringUtils.stripToNull(items(10))
      val publication = Individual(OBOUtil.zfinIRI(publicationID))
      axioms.add(expression Fact (dcSource, publication))
      return axioms.toSet
    }
  }

}