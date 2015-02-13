package org.phenoscape.owl

import scala.collection.GenMap
import scala.collection.JavaConversions._

import org.openrdf.model.Statement
import org.openrdf.model.impl.StatementImpl
import org.openrdf.model.impl.URIImpl
import org.openrdf.model.vocabulary.RDF
import org.openrdf.query.BindingSet
import org.openrdf.query.QueryLanguage
import org.openrdf.repository.sail.SailRepositoryConnection
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.ExpressionUtil
import org.phenoscape.owl.util.SesameIterationIterator.iterationToIterator
import org.phenoscape.owlet.SPARQLComposer._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.reasoner.OWLReasoner

import com.hp.hpl.jena.query.Query

object EvolutionaryProfiles {

  val factory = OWLManager.getOWLDataFactory
  val PhenoscapeKB = new URIImpl("http://kb.phenoscape.org/")

  type StateAssociations = GenMap[TaxonNode, GenMap[Character, Set[State]]]

  def computePhenotypeProfiles(rootTaxon: TaxonNode, reasoner: OWLReasoner, db: SailRepositoryConnection): Set[Statement] = {
    val observedAssociations = queryAssociations(db)
    val associationsIndex = index(observedAssociations)
    val (associations, profiles) = postorder(rootTaxon, reasoner, index(observedAssociations), Map.empty)
    profilesToRDF(profiles, db)
  }

  def report(profiles: Map[TaxonNode, Map[Character, Set[State]]], reasoner: OWLReasoner): Unit = {
    for {
      (taxon, profile) <- profiles
      if profile.nonEmpty
    } {
      val taxonLabel = ExpressionUtil.labelFor(taxon, reasoner.getRootOntology.getImportsClosure).getOrElse("unlabeled")
      println(s"$taxonLabel")
      for { (character, states) <- profile } {
        println(s"\t${character.label}: ${states.map(_.label).mkString("\t")}")
      }
      println
    }
  }

  def index(associations: Set[StateAssociation]): Map[TaxonNode, Map[Character, Set[State]]] =
    associations.groupBy(_.taxon).map {
      case (taxon, assocs) =>
        taxon -> assocs.groupBy(_.character).map {
          case (character, assocs) =>
            character -> assocs.map(_.state)
        }
    }

  def profilesToRDF(profiles: StateAssociations, db: SailRepositoryConnection): Set[Statement] = {
    val statePhenotypes: Map[State, Set[Phenotype]] = queryStatePhenotypes(db)
    (for {
      (taxon, profile) <- toSequential(profiles)
      (character, states) <- profile
      state <- states
      phenotype <- statePhenotypes.getOrElse(state, Set.empty)
    } yield {
      val profileURI = new URIImpl(taxonProfileURI(taxon))
      Set(new StatementImpl(profileURI, RDF.TYPE, new URIImpl(phenotype.iri.toString)),
        new StatementImpl(new URIImpl(taxon.iri.toString), new URIImpl(has_phenotypic_profile.toString), profileURI))
    }).flatten.toSet
  }

  implicit def taxonToOWLClass(taxon: TaxonNode): OWLClass = factory.getOWLClass(taxon.iri)

  def taxonProfileURI(taxon: TaxonNode) = s"http://phenoscape.org/profile/${taxon.iri.toString.split("/").last}"

  def toSequential(associations: StateAssociations): Map[TaxonNode, Map[Character, Set[State]]] = associations.map({ case (taxon, states) => taxon -> states.seq.toMap }).seq.toMap

  def postorder(node: TaxonNode, reasoner: OWLReasoner, startingAssociations: StateAssociations, startingProfiles: StateAssociations): (StateAssociations, StateAssociations) = {
    val children = reasoner.getSubClasses(node, true).getFlattened.filterNot(_.isOWLNothing).map(aClass => TaxonNode(aClass.getIRI))
    val nodeStates = startingAssociations.getOrElse(node, Map.empty)
    if (children.isEmpty) {
      (Map(node -> nodeStates), Map.empty)
    } else {
      val (subtreeAssociationsGroups, subtreeProfilesGroups) = children.par.map(postorder(_, reasoner, startingAssociations, startingProfiles)).unzip
      val subtreeAssociations = subtreeAssociationsGroups.flatten.toMap
      val subtreeProfiles = subtreeProfilesGroups.flatten.toMap
      val charactersWithAssociations = subtreeAssociations.values.flatMap(_.keys).toSet ++ nodeStates.keys
      val currentNodeAssociationsAndProfile = for {
        character <- charactersWithAssociations
      } yield {
        val nodeStateSet = nodeStates.getOrElse(character, Set.empty)
        val childrenStateSets = children.map(subtreeAssociations(_).getOrElse(character, Set.empty))
        val allStateSets = childrenStateSets + nodeStateSet
        val nonEmptyStateSets = allStateSets.filter(_.nonEmpty)
        val sharedStates: Set[State] = nonEmptyStateSets.size match {
          case 0 => Set.empty
          case 1 => nonEmptyStateSets.head
          case _ => nonEmptyStateSets.reduce(_ intersect _)
        }
        val (currentStates: Set[State], statesForProfile: Set[State]) = if (sharedStates.nonEmpty) (sharedStates, Set.empty)
        else allStateSets.size match {
          case 0 => (Set.empty, Set.empty)
          case 1 => (allStateSets.head, Set.empty)
          case _ => {
            val unionStates = allStateSets.reduce(_ union _)
            (unionStates, unionStates)
          }
        }
        (character -> currentStates, character -> statesForProfile)
      }
      val (currentNodeAssociations, rawProfile) = currentNodeAssociationsAndProfile.unzip
      val profileWithoutEmptyCharacters = rawProfile.filter { case (character, states) => states.nonEmpty }
      (subtreeAssociations + (node -> currentNodeAssociations.toMap), subtreeProfiles + (node -> profileWithoutEmptyCharacters.toMap))
    }
  }

  def queryAssociations(connection: SailRepositoryConnection): Set[StateAssociation] = {
    val query = connection.prepareTupleQuery(QueryLanguage.SPARQL, associationsQuery.toString)
    query.evaluate().map(StateAssociation(_)).toSet
  }

  val associationsQuery: Query =
    select_distinct('taxon, 'matrix_char, 'matrix_char_label, 'state, 'state_label) from "http://kb.phenoscape.org/" where (
      bgp(
        t('taxon, exhibits_state, 'state),
        t('state, rdfsLabel, 'state_label),
        t('matrix_char, may_have_state_value, 'state),
        t('matrix_char, rdfsLabel, 'matrix_char_label)))

  def queryStatePhenotypes(connection: SailRepositoryConnection): Map[State, Set[Phenotype]] = {
    val query = connection.prepareTupleQuery(QueryLanguage.SPARQL, phenotypesQuery.toString)
    query.evaluate().map { bindings =>
      (State(IRI.create(bindings.getValue("state").stringValue), bindings.getValue("state").stringValue),
        Phenotype(IRI.create(bindings.getValue("phenotype").stringValue)))
    }.toIterable.groupBy {
      case (state, phenotype) => state
    }.map {
      case (state, statesWithPhenotypes) => state -> statesWithPhenotypes.map {
        case (state, phenotype) => phenotype
      }.toSet
    }
  }

  val phenotypesQuery: Query =
    select_distinct('state, 'state_label, 'phenotype) from "http://kb.phenoscape.org/" where (
      bgp(
        t('state, rdfsLabel, 'state_label),
        t('state, describes_phenotype, 'phenotype)))

}

case class TaxonNode(iri: IRI)

case class Character(iri: IRI, label: String)

case class State(iri: IRI, label: String)

case class Phenotype(iri: IRI)

case class StateAssociation(taxon: TaxonNode, character: Character, state: State)

object StateAssociation {

  def apply(result: BindingSet): StateAssociation = StateAssociation(
    TaxonNode(IRI.create(result.getValue("taxon").stringValue)),
    Character(IRI.create(result.getValue("matrix_char").stringValue), result.getValue("matrix_char_label").stringValue),
    State(IRI.create(result.getValue("state").stringValue), result.getValue("state_label").stringValue))

}

