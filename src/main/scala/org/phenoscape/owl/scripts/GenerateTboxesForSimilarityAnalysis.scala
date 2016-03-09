package org.phenoscape.owl.scripts

import java.io.File
import scala.collection.JavaConversions._
import scala.collection.immutable.Set
import org.phenoscape.owl.MaterializeInferences
import org.phenoscape.owl.PropertyNormalizer
import org.phenoscape.owl.SimilarityTemplates
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.OntologyUtil
import org.phenoscape.scowl._
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.model.OWLClass
import java.io.FileWriter
import java.io.FileOutputStream
import java.io.OutputStreamWriter

object GenerateTboxesForSimilarityAnalysis extends App {

  import GenerateTboxesForSimilarityAnalysisUtil._
  
  val AnatomicalEntity = Class(ANATOMICAL_ENTITY)
  val Quality = Class("http://purl.obolibrary.org/obo/PATO_0000001")
  val manager = OWLManager.createOWLOntologyManager()
  val profiles = manager.loadOntology(IRI.create(new File("profiles.ttl")))
  val profilesTbox = ParseProfileSemantics.tboxWithSemanticsForProfiles(profiles)
  val phenoscapeVocab = loadFromWebWithImports("http://purl.org/phenoscape/vocab.owl")
  val uberon = loadFromWebWithImports("http://purl.obolibrary.org/obo/uberon/ext.owl")
  val pato = loadFromWebWithImports("http://purl.obolibrary.org/obo/pato.owl")
  val bspo = loadFromWebWithImports("http://purl.obolibrary.org/obo/bspo.owl")
  val go = loadFromWebWithImports("http://purl.obolibrary.org/obo/go.owl")
  val zfa = loadFromWebWithImports("http://purl.obolibrary.org/obo/zfa.owl")
  val xao = loadFromWebWithImports("http://purl.obolibrary.org/obo/xao.owl")
  val emapa = loadFromWebWithImports("http://purl.obolibrary.org/obo/emapa.owl")
  val hpo = loadFromWebWithImports("http://purl.obolibrary.org/obo/hp.owl")
  val hpEQ = loadFromWebWithImports("https://phenotype-ontologies.googlecode.com/svn/trunk/src/ontology/hp/hp-equivalence-axioms-subq-ubr.owl")
  val mpEQ = loadFromWebWithImports("https://phenotype-ontologies.googlecode.com/svn/trunk/src/ontology/mp/mp-equivalence-axioms-subq-ubr.owl")
  val caroToUberon = loadFromWebWithImports("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-caro.owl")
  val zfaToUberon = loadFromWebWithImports("http://purl.obolibrary.org/obo/uberon/bridge/uberon-ext-bridge-to-zfa.owl")
  val xaoToUberon = loadFromWebWithImports("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-xao.owl")
  val fmaToUberon = loadFromWebWithImports("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-fma.owl")
  val emapaToUberon = loadFromWebWithImports("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-emapa.owl")

  val uberonPATOReasoner = reasoner(uberon ++ pato)
  val anatomicalEntities = uberonPATOReasoner.getSubClasses(AnatomicalEntity, false).getFlattened.filterNot(_.isOWLNothing).toSet
  val qualities = uberonPATOReasoner.getSubClasses(Quality, false).getFlattened.filterNot(_.isOWLNothing).toSet
  uberonPATOReasoner.dispose()

  val (entityPhenotypes, entityPhenotypeAxioms) = flattenAxioms(anatomicalEntities.map(SimilarityTemplates.entity).unzip[OWLClass, Set[OWLAxiom]])
  val (entityPartsPhenotypes, entityPartsPhenotypeAxioms) = flattenAxioms(anatomicalEntities.map(SimilarityTemplates.partsOfEntity).unzip[OWLClass, Set[OWLAxiom]])
  val (qualityPhenotypes, qualityPhenotypeAxioms) = flattenAxioms(qualities.map(SimilarityTemplates.quality).unzip[OWLClass, Set[OWLAxiom]])

  val mainTbox = OntologyUtil.filterDisjointAxioms(
    profilesTbox ++ phenoscapeVocab ++ uberon ++
      pato ++ bspo ++ go ++ zfa ++ xao ++ emapa ++ hpo ++ hpEQ ++
      mpEQ ++ caroToUberon ++ zfaToUberon ++ xaoToUberon ++ fmaToUberon ++ emapaToUberon)

  val entitiesOnt = manager.createOntology(mainTbox ++ entityPhenotypeAxioms ++ entityPartsPhenotypeAxioms)
  val entitiesAndQualitiesOnt = manager.createOntology(mainTbox ++ entityPhenotypeAxioms ++ entityPartsPhenotypeAxioms ++ qualityPhenotypeAxioms)

  val entitiesReasoner = new ElkReasonerFactory().createReasoner(entitiesOnt)
  MaterializeInferences.materializeInferences(entitiesOnt, entitiesReasoner)
  entitiesReasoner.dispose()

  val entitiesAndQualitiesReasoner = new ElkReasonerFactory().createReasoner(entitiesAndQualitiesOnt)
  MaterializeInferences.materializeInferences(entitiesAndQualitiesOnt, entitiesAndQualitiesReasoner)
  entitiesAndQualitiesReasoner.dispose()

  val reducedEntitiesOnt = OntologyUtil.reduceOntologyToHierarchy(entitiesOnt)
  val reducedEntitiesAndQualitiesOnt = OntologyUtil.reduceOntologyToHierarchy(entitiesAndQualitiesOnt)

  manager.saveOntology(reducedEntitiesOnt, IRI.create(new File("entities.owl")))
  manager.saveOntology(reducedEntitiesAndQualitiesOnt, IRI.create(new File("entitiesAndQualities.owl")))

  val entityWriter = new OutputStreamWriter(new FileOutputStream("entity_phenotypes.txt"), "UTF-8")
  entityPhenotypes.foreach(p => entityWriter.write(s"${p.getIRI.toString}\n"))
  entityWriter.close()

  val entityPartsWriter = new OutputStreamWriter(new FileOutputStream("entity_parts_phenotypes.txt"), "UTF-8")
  entityPartsPhenotypes.foreach(p => entityPartsWriter.write(s"${p.getIRI.toString}\n"))
  entityPartsWriter.close()

  val qualityWriter = new OutputStreamWriter(new FileOutputStream("quality_phenotypes.txt"), "UTF-8")
  qualityPhenotypes.foreach(p => qualityWriter.write(s"${p.getIRI.toString}\n"))
  qualityWriter.close()

}

object GenerateTboxesForSimilarityAnalysisUtil {

  def loadFromWebWithImports(iri: String): Set[OWLAxiom] = {
    val manager = OWLManager.createOWLOntologyManager()
    val ont = manager.loadOntology(IRI.create(iri))
    val importsAxioms = (ont.getImportsClosure - ont).flatMap(_.getAxioms)
    manager.addAxioms(ont, importsAxioms)
    PropertyNormalizer.normalize(ont)
    ont.getAxioms.toSet
  }

  def flattenAxioms[A, B](in: (A, Set[Set[B]])): (A, Set[B]) = (in._1, in._2.flatten)

  def reasoner(axioms: Set[OWLAxiom]): OWLReasoner = new ElkReasonerFactory().createReasoner(OWLManager.createOWLOntologyManager().createOntology(axioms))

}