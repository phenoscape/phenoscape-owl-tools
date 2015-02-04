package org.phenoscape.owl

import scala.annotation.tailrec
import scala.collection.JavaConverters.asScalaSetConverter
import scala.collection.JavaConverters.setAsJavaSetConverter

import org.phenoscape.owl.Vocab.inheres_in
import org.phenoscape.scowl.OWL.Class
import org.phenoscape.scowl.OWL.ScowlClassExpression
import org.phenoscape.scowl.OWL.ScowlObjectProperty
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory

class SubsumerGenerator(ont: OWLOntology, reasonerFactory: OWLReasonerFactory) {

  val reasoner = reasonerFactory.createReasoner(ont)
  val manager = ont.getOWLOntologyManager
  val expressionMakers: Set[EQMaker] = Set(new EQMaker(Class(Vocab.ANATOMICAL_ENTITY), Class(Vocab.QUALITY)))

  case class EQMaker(rootEntity: OWLClass, rootQuality: OWLClass) {

    val namedClass: OWLClass = Class(s"http://example.org/basic_eq?entity=${rootEntity.getIRI.toString}&quality=${rootQuality.getIRI.toString}")

    val axiom: OWLEquivalentClassesAxiom = {
      namedClass EquivalentTo (rootQuality and (inheres_in some rootEntity))
    }

    def next: scala.collection.Set[EQMaker] = for {
      //FIXME not combining roots with subclasses
      //entity <- reasoner.getSubClasses(rootEntity, true).getFlattened.asScala
      entity <- rootEntity.getSubClasses(ont).asScala + rootEntity
      if !entity.isAnonymous
      quality <- rootQuality.getSubClasses(ont).asScala + rootQuality
      if !quality.isAnonymous
      //quality <- reasoner.getSubClasses(rootQuality, true).getFlattened.asScala
      if !(entity == rootEntity && quality == rootQuality)
    } yield new EQMaker(entity.asOWLClass, quality.asOWLClass)

  }

  @tailrec
  private def generateSubsumers(makers: Set[EQMaker], accumulatedAxioms: Set[OWLAxiom] = Set(), tested: Set[EQMaker] = Set()): Set[OWLAxiom] = {
    println(s"Accumulated ${accumulatedAxioms.size}")
    println(s"Working with ${makers.size} makers")
    println("Removing already tested EQs")
    val testMakers = makers diff tested
    val testAxioms = testMakers map (_.axiom)
    println(s"Testing ${testAxioms.size}")
    manager.addAxioms(ont, testAxioms.asJava)
    reasoner.flush()
    println("Querying subclasses")
    val keeps = testMakers filter { maker =>
      val nodeSet = reasoner.getSubClasses(maker.namedClass, false)
      !nodeSet.isBottomSingleton
    }
    println("Removing axioms")
    manager.removeAxioms(ont, testAxioms.asJava)
    println("Unzip stuff")
    println("Collect new axioms")
    val newAxioms = keeps map (_.axiom)
    println("Create next generation")
    val nextGeneration = keeps.par flatMap (_.next)
    println("Concatenate lists")
    val allNewAxioms = newAxioms ++ accumulatedAxioms
    println("Done concatenating")
    if (nextGeneration.isEmpty) allNewAxioms
    else generateSubsumers(nextGeneration.seq, allNewAxioms, tested ++ testMakers)
  }

  def go: Set[OWLAxiom] = {
    generateSubsumers(expressionMakers).toSet
  }

}