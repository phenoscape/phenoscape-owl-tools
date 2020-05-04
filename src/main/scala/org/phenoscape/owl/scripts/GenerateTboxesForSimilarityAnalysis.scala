package org.phenoscape.owl.scripts

import java.io.File
import scala.collection.JavaConverters._
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
import java.io.FileOutputStream
import java.io.OutputStreamWriter

object GenerateTboxesForSimilarityAnalysis extends App {

  import GenerateTboxesForSimilarityAnalysisUtil._

  val AnatomicalEntity = Class(ANATOMICAL_ENTITY)
  val Quality          = Class("http://purl.obolibrary.org/obo/PATO_0000001")
  val manager          = OWLManager.createOWLOntologyManager()
  val profiles         = manager.loadOntology(IRI.create(new File("profiles.ttl")))
  val profilesTbox     = ParseProfileSemantics.tboxWithSemanticsForProfiles(profiles)
  val phenoscapeVocab  = loadFromWebWithImports("http://purl.org/phenoscape/vocab.owl")
  val uberon           = loadFromWebWithImports("http://purl.obolibrary.org/obo/uberon/ext.owl")
  val pato             = loadFromWebWithImports("http://purl.obolibrary.org/obo/pato.owl")
  val bspo             = loadFromWebWithImports("http://purl.obolibrary.org/obo/bspo.owl")
  val go               = loadFromWebWithImports("http://purl.obolibrary.org/obo/go.owl")
  val zfa              = loadFromWebWithImports("http://purl.obolibrary.org/obo/zfa.owl")
  val xao              = loadFromWebWithImports("http://purl.obolibrary.org/obo/xao.owl")
  val hp               = loadFromWebWithImports("http://purl.obolibrary.org/obo/hp.owl")
  val mp               = loadFromWebWithImports("http://purl.obolibrary.org/obo/mp.owl")
  val caroToUberon     = loadFromWebWithImports("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-caro.owl")
  val zfaToUberon      = loadFromWebWithImports("http://purl.obolibrary.org/obo/uberon/bridge/uberon-ext-bridge-to-zfa.owl")
  val xaoToUberon      = loadFromWebWithImports("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-xao.owl")

  val uberonPATOReasoner = reasoner(uberon ++ pato)
  val anatomicalEntities =
    uberonPATOReasoner.getSubClasses(AnatomicalEntity, false).getFlattened.asScala.filterNot(_.isOWLNothing).toSet
  val qualities = uberonPATOReasoner.getSubClasses(Quality, false).getFlattened.asScala.filterNot(_.isOWLNothing).toSet
  uberonPATOReasoner.dispose()

  val (entityPhenotypes, entityPhenotypeAxioms) = flattenAxioms(
    anatomicalEntities.map(SimilarityTemplates.entity).unzip[OWLClass, Set[OWLAxiom]]
  )
  val (entityPartsPhenotypes, entityPartsPhenotypeAxioms) = flattenAxioms(
    anatomicalEntities.map(SimilarityTemplates.partsOfEntity).unzip[OWLClass, Set[OWLAxiom]]
  )
  val (qualityPhenotypes, qualityPhenotypeAxioms) = flattenAxioms(
    qualities.map(SimilarityTemplates.quality).unzip[OWLClass, Set[OWLAxiom]]
  )

  val attributes         = loadFromWebWithImports("http://svn.code.sf.net/p/phenoscape/code/trunk/vocab/character_slims.obo")
  val attributeQualities = attributes.flatMap(_.getClassesInSignature.asScala) + HasNumberOf
  val (entityAttributePhenotypes, entityAttributePhenotypeAxioms) = flattenAxioms(
    (for {
      attribute <- attributeQualities
      entity    <- anatomicalEntities
    } yield {
      SimilarityTemplates.entityWithQuality(entity, attribute)
    }).unzip[OWLClass, Set[OWLAxiom]]
  )
  val (entityPartAttributePhenotypes, entityPartAttributePhenotypeAxioms) = flattenAxioms(
    (for {
      attribute <- attributeQualities
      entity    <- anatomicalEntities
    } yield {
      SimilarityTemplates.partsOfEntityWithQuality(entity, attribute)
    }).unzip[OWLClass, Set[OWLAxiom]]
  )

  val mainTbox = OntologyUtil.filterDisjointAxioms(
    profilesTbox ++ phenoscapeVocab ++ uberon ++
    pato ++ bspo ++ go ++ zfa ++ xao ++ hp ++ mp ++
    caroToUberon ++ zfaToUberon ++ xaoToUberon
  )

  val entitiesOnt = manager.createOntology((mainTbox ++ entityPhenotypeAxioms ++ entityPartsPhenotypeAxioms).asJava)
  val entitiesAndQualitiesOnt = manager.createOntology(
    (mainTbox ++ entityPhenotypeAxioms ++ entityPartsPhenotypeAxioms ++ qualityPhenotypeAxioms).asJava
  )
  val entitiesAttributesOnt =
    manager.createOntology((mainTbox ++ entityAttributePhenotypeAxioms ++ entityPartAttributePhenotypeAxioms).asJava)

  val entitiesReasoner = new ElkReasonerFactory().createReasoner(entitiesOnt)
  MaterializeInferences.materializeInferences(entitiesOnt, entitiesReasoner)
  entitiesReasoner.dispose()

  val entitiesAndQualitiesReasoner = new ElkReasonerFactory().createReasoner(entitiesAndQualitiesOnt)
  MaterializeInferences.materializeInferences(entitiesAndQualitiesOnt, entitiesAndQualitiesReasoner)
  entitiesAndQualitiesReasoner.dispose()

  val entitiesAttributesReasoner = new ElkReasonerFactory().createReasoner(entitiesAttributesOnt)
  MaterializeInferences.materializeInferences(entitiesAttributesOnt, entitiesAttributesReasoner)
  entitiesAttributesReasoner.dispose()

  val reducedEntitiesOnt             = OntologyUtil.reduceOntologyToHierarchy(entitiesOnt)
  val reducedEntitiesAndQualitiesOnt = OntologyUtil.reduceOntologyToHierarchy(entitiesAndQualitiesOnt)
  val reducedEntitiesAttributesOnt   = OntologyUtil.reduceOntologyToHierarchy(entitiesAttributesOnt)

  manager.saveOntology(reducedEntitiesOnt, IRI.create(new File("entities.owl")))
  manager.saveOntology(reducedEntitiesAndQualitiesOnt, IRI.create(new File("entitiesAndQualities.owl")))
  manager.saveOntology(reducedEntitiesAttributesOnt, IRI.create(new File("entitiesAndAttributes.owl")))

  val entityWriter = new OutputStreamWriter(new FileOutputStream("entity_phenotypes.txt"), "UTF-8")
  entityPhenotypes.foreach(p => entityWriter.write(s"${p.getIRI.toString}\n"))
  entityWriter.close()

  val entityPartsWriter = new OutputStreamWriter(new FileOutputStream("entity_parts_phenotypes.txt"), "UTF-8")
  entityPartsPhenotypes.foreach(p => entityPartsWriter.write(s"${p.getIRI.toString}\n"))
  entityPartsWriter.close()

  val qualityWriter = new OutputStreamWriter(new FileOutputStream("quality_phenotypes.txt"), "UTF-8")
  qualityPhenotypes.foreach(p => qualityWriter.write(s"${p.getIRI.toString}\n"))
  qualityWriter.close()

  val entitiesAttributesWriter =
    new OutputStreamWriter(new FileOutputStream("entity_attribute_phenotypes.txt"), "UTF-8")
  entityAttributePhenotypes.foreach(p => entitiesAttributesWriter.write(s"${p.getIRI.toString}\n"))
  entitiesAttributesWriter.close()

  val entityPartsAttributesWriter =
    new OutputStreamWriter(new FileOutputStream("entity_part_attribute_phenotypes.txt"), "UTF-8")
  entityPartAttributePhenotypes.foreach(p => entityPartsAttributesWriter.write(s"${p.getIRI.toString}\n"))
  entityPartsAttributesWriter.close()

}

object GenerateTboxesForSimilarityAnalysisUtil {

  def loadFromWebWithImports(iri: String): Set[OWLAxiom] = {
    val manager       = OWLManager.createOWLOntologyManager()
    val ont           = manager.loadOntology(IRI.create(iri))
    val importsAxioms = (ont.getImportsClosure.asScala - ont).flatMap(_.getAxioms().asScala)
    manager.addAxioms(ont, importsAxioms.asJava)
    PropertyNormalizer.normalize(ont)
    ont.getAxioms().asScala.toSet
  }

  def flattenAxioms[A, B](in: (A, Set[Set[B]])): (A, Set[B]) = (in._1, in._2.flatten)

  def reasoner(axioms: Set[OWLAxiom]): OWLReasoner =
    new ElkReasonerFactory().createReasoner(OWLManager.createOWLOntologyManager().createOntology(axioms.asJava))

}
