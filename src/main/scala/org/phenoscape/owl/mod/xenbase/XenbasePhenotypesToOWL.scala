package org.phenoscape.owl.mod.xenbase

import org.phenoscape.owl.OWLTask
import org.phenoscape.scowl.OWL._
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.Set
import org.semanticweb.owlapi.model.OWLOntology
import java.io.File
import scala.io.Source
import org.semanticweb.owlapi.model.OWLAxiom
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.Vocab._
import org.apache.commons.lang3.StringUtils
import org.phenoscape.owl.util.OBOUtil
import org.semanticweb.owlapi.model.IRI
import org.phenoscape.owl.NamedRestrictionGenerator
import org.semanticweb.owlapi.model.OWLClass
import org.phenoscape.owl.util.ExpressionUtil
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.apibinding.OWLManager

object XenbasePhenotypesToOWL extends OWLTask {

  val laevis = Individual(Vocab.XENOPUS_LAEVIS)
  val tropicalis = Individual(Vocab.XENOPUS_TROPICALIS)
  val present = Class(Vocab.PRESENT)
  val absent = Class(Vocab.ABSENT)
  val lacksAllPartsOfType = Class(Vocab.LACKS_ALL_PARTS_OF_TYPE)
  val organism = Class(Vocab.MULTI_CELLULAR_ORGANISM)
  val manager = OWLManager.createOWLOntologyManager()

  def convert(phenotypeData: Source): OWLOntology = {
    val ontology = manager.createOntology(IRI.create("http://purl.obolibrary.org/obo/phenoscape/xenbase_phenotypes.owl"))
    manager.addAxioms(ontology, phenotypeData.getLines.flatMap(translate).toSet[OWLAxiom])
    return ontology
  }

  def translate(expressionLine: String): Set[OWLAxiom] = ???

}