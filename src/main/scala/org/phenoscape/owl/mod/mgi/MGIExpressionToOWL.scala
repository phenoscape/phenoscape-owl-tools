package org.phenoscape.owl.mod.mgi

import java.io.File
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.Map
import scala.collection.Set
import scala.io.Source
import org.phenoscape.scowl.OWL._
import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.mod.xenbase.XenbaseGenesToOWL
import org.phenoscape.owl.util.OBOUtil
import org.phenoscape.owl.NamedRestrictionGenerator
import org.phenoscape.owl.OWLTask
import org.phenoscape.owl.Vocab
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.apibinding.OWLManager
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.OntologyUtil

object MGIExpressionToOWL extends OWLTask {

  val mouse = Individual(Vocab.MOUSE)
  val manager = OWLManager.createOWLOntologyManager()

  def main(args: Array[String]): Unit = {
    val mgiExpressionFile = Source.fromFile(args(0), "utf-8")
    val ontology = convert(mgiExpressionFile)
    mgiExpressionFile.close()
    manager.saveOntology(ontology, IRI.create(new File(args(1))))
  }

  def convert(expressionData: Source): OWLOntology = {
    val ontology = manager.createOntology(IRI.create("http://purl.obolibrary.org/obo/phenoscape/mgi_gene_expression.owl"))
    manager.addAxioms(ontology, expressionData.getLines.drop(1).map(translate(_)).flatten.toSet[OWLAxiom])
    val rdfsLabel = factory.getRDFSLabel()
    manager.addAxiom(ontology, mouse Annotation (rdfsLabel, factory.getOWLLiteral("Mus musculus")))
    return ontology
  }

  def translate(expressionLine: String): Set[OWLAxiom] = {
    val items = expressionLine.split("\t", -1)
    if (StringUtils.stripToNull(items(5)) != "Present") {
      Set()
    } else {
      val axioms = mutable.Set[OWLAxiom]()
      val expression = OntologyUtil.nextIndividual()
      axioms.add(factory.getOWLDeclarationAxiom(expression))
      axioms.add(expression Type GeneExpression)
      val structure = OntologyUtil.nextIndividual()
      axioms.add(factory.getOWLDeclarationAxiom(structure))
      axioms.add(expression Fact (OCCURS_IN, structure))
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
      axioms
    }
  }

}