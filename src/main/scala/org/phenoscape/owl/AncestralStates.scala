package org.phenoscape.owl

import scala.collection.JavaConversions._
import org.openrdf.query.QueryLanguage
import org.openrdf.repository.sail.SailRepositoryConnection
import org.phenoscape.owl.Vocab._
import org.phenoscape.owlet.SPARQLComposer._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.phenoscape.owl.util.SesameIterationIterator._
import com.hp.hpl.jena.query.Query
import org.openrdf.query.BindingSet
import scala.collection.GenMap
import scala.collection.GenSet
import org.phenoscape.owl.util.ExpressionUtil

object AncestralStates {

  val factory = OWLManager.getOWLDataFactory
  val Nothing = factory.getOWLNothing

  type StateAssociations = GenMap[TaxonNode, GenMap[Character, Set[State]]]

  def reconstructAncestralStates(rootTaxon: TaxonNode, reasoner: OWLReasoner, observedAssociations: Set[StateAssociation]): Map[TaxonNode, Map[Character, Set[State]]] = {
    val associationsIndex = index(observedAssociations)
    val (associations, profiles) = postorder(rootTaxon, reasoner, index(observedAssociations), Map.empty)
    val result = toSequential(profiles)
    report(result, reasoner)
    result
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

  implicit def taxonToOWLClass(taxon: TaxonNode): OWLClass = factory.getOWLClass(taxon.iri)

  def toSequential(associations: StateAssociations): Map[TaxonNode, Map[Character, Set[State]]] = associations.map({ case (taxon, states) => taxon -> states.seq.toMap }).seq.toMap

  def postorder(node: TaxonNode, reasoner: OWLReasoner, startingAssociations: StateAssociations, startingProfiles: StateAssociations): (StateAssociations, StateAssociations) = {
    val children = reasoner.getSubClasses(node, true).getFlattened.filterNot(_ == Nothing).map(aClass => TaxonNode(aClass.getIRI))
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

}

case class TaxonNode(iri: IRI)

case class Character(iri: IRI, label: String)

case class State(iri: IRI, label: String)

case class StateAssociation(taxon: TaxonNode, character: Character, state: State)

object StateAssociation {

  def apply(result: BindingSet): StateAssociation = StateAssociation(
    TaxonNode(IRI.create(result.getValue("taxon").stringValue)),
    Character(IRI.create(result.getValue("matrix_char").stringValue), result.getValue("matrix_char_label").stringValue),
    State(IRI.create(result.getValue("state").stringValue), result.getValue("state_label").stringValue))

}

