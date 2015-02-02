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

object AncestralStates {

  val factory = OWLManager.getOWLDataFactory
  val Nothing = factory.getOWLNothing

  def reconstructAncestralStates(rootTaxon: TaxonNode, reasoner: OWLReasoner, observedAssociations: Set[StateAssociation]): GenMap[TaxonNode, GenMap[Character, Set[State]]] = {
    println("Found associations: " + observedAssociations.size)
    val associationsIndex = index(observedAssociations)
    //val allCharacters = observedAssociations.map(_.character).toSet
    val result = postorder(rootTaxon, reasoner, index(observedAssociations))
    for {
      (character, states) <- result(rootTaxon)
    } {
      println(s"$character: ${states.size}")
    }
    result
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

  def preorder(node: TaxonNode, reasoner: OWLReasoner): Map[TaxonNode, Map[Character, Set[State]]] = {
    //val res = f(node, initial)
    //val r = reasoner.getSubClasses(node, true).getFlattened.flatMap(preorder(_, f, res))
    ???
  }

  def postorder(node: TaxonNode, reasoner: OWLReasoner, startingAssociations: Map[TaxonNode, Map[Character, Set[State]]]): GenMap[TaxonNode, GenMap[Character, Set[State]]] = {
    val children = reasoner.getSubClasses(node, true).getFlattened.filterNot(_ == Nothing).map(_.getIRI).map(TaxonNode)
    val nodeStates = startingAssociations.getOrElse(node, Map.empty)
    if (children.isEmpty) {
      Map(node -> nodeStates)
    } else {
      val subtreeAssociations = children.par.flatMap(postorder(_, reasoner, startingAssociations)).toMap
      println("Processing " + node)
      val charactersWithAssociations = subtreeAssociations.values.flatMap(_.keys).toSet ++ nodeStates.keys
      val currentNodeAssociations = for {
        character <- charactersWithAssociations
      } yield {
        val nodeStateSet = nodeStates.getOrElse(character, Set.empty)
        val childrenStateSets = children.map(subtreeAssociations(_).getOrElse(character, Set.empty))
        val allStateSets = childrenStateSets + nodeStateSet
        val nonEmptyStateSets = allStateSets.filter(_.nonEmpty)
        val shared: Set[State] = nonEmptyStateSets.size match {
          case 0 => Set.empty
          case 1 => nonEmptyStateSets.head
          case _ => nonEmptyStateSets.reduce(_ intersect _)
        }
        val (currentStates: Set[State], addToProfile) = if (shared.nonEmpty) (shared, false) else allStateSets.size match {
          case 0 => (Set.empty, false)
          case 1 => (allStateSets.head, false)
          case _ => (allStateSets.reduce(_ union _), true) //TODO add character/state to profile for this node!
        }
        (character, currentStates)
      }
      subtreeAssociations + (node -> currentNodeAssociations.toMap)
    }
  }

  def queryAssociations(connection: SailRepositoryConnection): Set[StateAssociation] = {
    val query = connection.prepareTupleQuery(QueryLanguage.SPARQL, associationsQuery.toString)
    query.evaluate().map(StateAssociation(_)).toSet
  }

  val associationsQuery: Query = select_distinct('taxon, 'matrix_char, 'state) from "http://kb.phenoscape.org/" where (
    bgp(
      t('taxon, exhibits_state, 'state),
      t('matrix_char, may_have_state_value, 'state)))

}

case class TaxonNode(iri: IRI)

case class Character(iri: IRI)

case class State(iri: IRI)

case class StateAssociation(taxon: TaxonNode, character: Character, state: State)

object StateAssociation {

  def apply(result: BindingSet): StateAssociation = StateAssociation(
    TaxonNode(IRI.create(result.getValue("taxon").stringValue)),
    Character(IRI.create(result.getValue("matrix_char").stringValue)),
    State(IRI.create(result.getValue("state").stringValue)))

}

