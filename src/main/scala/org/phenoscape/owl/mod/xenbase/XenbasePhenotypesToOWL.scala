package org.phenoscape.owl.mod.xenbase

import org.phenoscape.owl.OWLTask
import org.phenoscape.scowl.OWL._
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.Map
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
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLClassExpression
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.OntologyUtil

object XenbasePhenotypesToOWL extends OWLTask {

  val laevis = Individual(Vocab.XENOPUS_LAEVIS)
  val tropicalis = Individual(Vocab.XENOPUS_TROPICALIS)
  val Present = Class(Vocab.PRESENT)
  val Absent = Class(Vocab.ABSENT)
  val LacksAllPartsOfType = Class(Vocab.LACKS_ALL_PARTS_OF_TYPE)
  val Organism = Class(Vocab.MULTI_CELLULAR_ORGANISM)
  val manager = OWLManager.createOWLOntologyManager()

  def convertToOntology(genepageMappingsFile: Source, phenotypeData: Source): OWLOntology = {
    val ontology = manager.createOntology(IRI.create("http://purl.obolibrary.org/obo/phenoscape/xenbase_phenotypes.owl"))
    manager.addAxioms(ontology, convertToAxioms(phenotypeData))
    return ontology
  }

  def convertToAxioms(phenotypeData: Source): Set[OWLAxiom] = {
    phenotypeData.getLines.drop(1).flatMap(translate).toSet[OWLAxiom]
  }

  def translate(annotationLine: String): Set[OWLAxiom] = {
    val items = annotationLine.split("\t")
    val geneText = StringUtils.stripToNull(items(11))
    if (geneText != null) {
      val phenotype = OntologyUtil.nextIndividual()
      val source = Individual(OBOUtil.xenbaseImageIRI(StringUtils.stripToNull(items(0))))
      val species = taxon(StringUtils.stripToNull(items(1)))
      val gene = Individual(XenbaseGenesToOWL.getGeneIRI(fixGeneID(geneText)))
      val phenotypeClass = OntologyUtil.nextClass()
      val quality = Class(OBOUtil.iriForTermID(StringUtils.stripToNull(items(15))))
      val (entity, entityAxioms) = OBOUtil.translatePostCompositionNamed(StringUtils.stripToNull(items(13)))
      val (optionalRelatedEntity, relatedEntityAxioms) = optionSet(Option(StringUtils.stripToNull(items(17))).map(OBOUtil.translatePostCompositionNamed))
      val eqPhenotype = (entity, quality, optionalRelatedEntity) match {
        case (entity, Absent, None) => (LacksAllPartsOfType and (inheres_in some Organism) and (TOWARDS value Individual(entity.getIRI)))
        case (entity, LacksAllPartsOfType, Some(relatedEntity)) => (LacksAllPartsOfType and (inheres_in some entity) and (TOWARDS value Individual(relatedEntity.getIRI)))
        case (entity, quality, Some(relatedEntity)) => quality and (inheres_in some entity) and (TOWARDS some relatedEntity)
        case (entity, quality, None) => quality and (inheres_in some entity)
      }
      Set(
        phenotype Type AnnotatedPhenotype,
        phenotype Fact (associated_with_gene, gene),
        phenotype Fact (associated_with_taxon, species),
        phenotype Fact (dcSource, source),
        phenotype Type phenotypeClass,
        phenotypeClass SubClassOf eqPhenotype,
        factory.getOWLDeclarationAxiom(phenotypeClass),
        factory.getOWLDeclarationAxiom(gene),
        factory.getOWLDeclarationAxiom(species)) ++
        entityAxioms ++
        relatedEntityAxioms
    } else Set()
  }

  def optionSet[T, S](in: Option[(T, Set[S])]): (Option[T], Set[S]) = in match {
    case Some((thing, set)) => (Option(thing), set)
    case None => (None, Set())
  }

  def fixGeneID(id: String): String = "XB-GENEPAGE-" + id.split(":")(1)

  val taxon: Map[String, OWLNamedIndividual] = Map(
    "XBTAXON:0000001" -> tropicalis,
    "XBTAXON:0000002" -> laevis)

}