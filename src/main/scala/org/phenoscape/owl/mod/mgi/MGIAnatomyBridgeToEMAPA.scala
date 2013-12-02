package org.phenoscape.owl.mod.mgi

import org.phenoscape.owl.OWLTask
import scala.io.Source
import scala.collection.JavaConversions._
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.phenoscape.owl.util.OBOUtil
import org.apache.commons.lang3.StringUtils
import org.phenoscape.scowl.OWL._
import java.io.File

object MGIAnatomyBridgeToEMAPA extends OWLTask {

  val ontologyName = "http://purl.org/phenoscape/mgi/anatomy.owl"
  val manager = OWLManager.createOWLOntologyManager();

  def convert(mappings: Source): OWLOntology = {
    val axioms = mappings.getLines.map(translate(_)).flatten.toSet[OWLAxiom]
    manager.createOntology(axioms, IRI.create(ontologyName))
  }

  def translate(mapping: String): Set[OWLAxiom] = {
    val items = mapping.split("\t")
    val mgiTerm = Class(OBOUtil.mgiAnatomyIRI(StringUtils.stripToNull(items(0))))
    val emapaTerm = Class(OBOUtil.iriForTermID(StringUtils.stripToNull(items(1))))
    Set(
      factory.getOWLDeclarationAxiom(mgiTerm),
      factory.getOWLDeclarationAxiom(emapaTerm),
      mgiTerm SubClassOf emapaTerm)
  }

  def main(args: Array[String]): Unit = {
    val mappingFile = Source.fromFile(args(0), "utf-8")
    val ontology = convert(mappingFile)
    mappingFile.close()
    manager.saveOntology(ontology, IRI.create(new File(args(1))))
  }

}