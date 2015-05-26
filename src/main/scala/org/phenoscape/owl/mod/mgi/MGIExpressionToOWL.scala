package org.phenoscape.owl.mod.mgi

import scala.collection.mutable
import scala.io.Source

import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.OBOUtil
import org.phenoscape.owl.util.OntologyUtil
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLAxiom

object MGIExpressionToOWL extends OWLTask {

  val mouse = Individual(Vocab.MOUSE)
  val manager = OWLManager.createOWLOntologyManager()
  val rdfsLabel = factory.getRDFSLabel()

  def convert(expressionData: Source): Set[OWLAxiom] =
    expressionData.getLines.drop(1).flatMap(translate).toSet[OWLAxiom] + (mouse Annotation (rdfsLabel, factory.getOWLLiteral("Mus musculus")))

  def translate(expressionLine: String): Set[OWLAxiom] = {
    val items = expressionLine.split("\t", -1)
    if (StringUtils.stripToNull(items(5)) == "Absent") {
      Set.empty
    } else {
      val axioms = mutable.Set[OWLAxiom]()
      val expression = OntologyUtil.nextIndividual()
      axioms.add(factory.getOWLDeclarationAxiom(expression))
      axioms.add(expression Type GeneExpression)
      val structure = OntologyUtil.nextIndividual()
      axioms.add(factory.getOWLDeclarationAxiom(structure))
      axioms.add(expression Fact (occurs_in, structure))
      val structureID = StringUtils.stripToNull(items(4))
      val structureType = Class(OBOUtil.mgiAnatomyIRI(structureID))
      axioms.add(structure Type structureType)
      val geneIRI = MGIGeneticMarkersToOWL.getGeneIRI(StringUtils.stripToNull(items(1)))
      val gene = Individual(geneIRI)
      axioms.add(factory.getOWLDeclarationAxiom(gene))
      axioms.add(expression Fact (associated_with_gene, gene))
      axioms.add(expression Fact (associated_with_taxon, mouse))
      val publicationID = StringUtils.stripToNull(items(10))
      val publication = Individual(OBOUtil.mgiReferenceIRI(publicationID))
      axioms.add(expression Fact (dcSource, publication))
      axioms.toSet
    }
  }

}