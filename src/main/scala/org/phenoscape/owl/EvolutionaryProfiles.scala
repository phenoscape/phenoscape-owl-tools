package org.phenoscape.owl

import java.io.{File, FileOutputStream}

import org.apache.jena.query.{QueryExecutionFactory, QuerySolution}
import org.apache.jena.rdf.model._
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.ExpressionsUtil
import org.phenoscape.sparql.SPARQLInterpolation._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

import scala.collection.GenMap
import scala.collection.JavaConverters._
import scala.language.implicitConversions

object EvolutionaryProfiles {

  val factory            = OWLManager.getOWLDataFactory
  val OWLNothing         = factory.getOWLNothing.getIRI.toString
  val RdfsLabel          = ResourceFactory.createProperty(rdfsLabel.getIRI.toString)
  val MayHaveStateValue  = ResourceFactory.createProperty(may_have_state_value.getIRI.toString)
  val ExhibitsState      = ResourceFactory.createProperty(exhibits_state.getIRI.toString)
  val DescribesPhenotype = ResourceFactory.createProperty(describes_phenotype.getIRI.toString)

  type StateAssociations = GenMap[TaxonNode, GenMap[Character, Set[State]]]

  def main(args: Array[String]): Unit = {

    val inFile  = args(0)
    val outFile = new File(args(1))

    val rootTaxon = (TaxonNode(CHORDATA))

    val results = computePhenotypeProfiles(rootTaxon, inFile)
    val model   = ModelFactory.createDefaultModel()
    model.add(results.toList.asJava)
    model.write(new FileOutputStream(outFile), "TTL")
  }

  def computePhenotypeProfiles(rootTaxon: TaxonNode, inFile: String): Set[Statement] = {

    val model = ModelFactory.createDefaultModel()
    model.read(inFile)

    val observedAssociations     = queryAssociations(model)
    val (associations, profiles) = postorder(rootTaxon, model, index(observedAssociations), Map.empty)
    profilesToRDF(profiles, model)
  }

  def report(profiles: Map[TaxonNode, Map[Character, Set[State]]], taxonomy: OWLOntology): Unit =
    for {
      (taxon, profile) <- profiles
      if profile.nonEmpty
    } {
      val taxonLabel = ExpressionsUtil.labelFor(taxon, taxonomy).getOrElse("unlabeled")
      println(s"$taxonLabel")
      for { (character, states) <- profile } println(s"\t${character.label}: ${states.map(_.label).mkString("\t")}")
      println
    }

  def index(associations: Set[StateAssociation]): Map[TaxonNode, Map[Character, Set[State]]] =
    associations.groupBy(_.taxon).map {
      case (taxon, assocs) =>
        taxon -> assocs.groupBy(_.character).map {
          case (character, assocs) =>
            character -> assocs.map(_.state)
        }
    }

  def profilesToRDF(profiles: StateAssociations, model: Model): Set[Statement] = {
    val statePhenotypes: Map[State, Set[Phenotype]] = queryStatePhenotypes(model)
    (for {
      (taxon, profile)    <- toSequential(profiles)
      (character, states) <- profile
      state               <- states
      phenotype           <- statePhenotypes.getOrElse(state, Set.empty)
      profileURI           = ResourceFactory.createResource(taxonProfileURI(taxon))
      statement           <- Set(
                     ResourceFactory.createStatement(
                       profileURI,
                       ResourceFactory.createProperty(rdfType.toString),
                       ResourceFactory.createResource(phenotype.iri.toString)
                     ),
                     ResourceFactory.createStatement(
                       ResourceFactory.createResource(taxon.iri.toString),
                       ResourceFactory.createProperty(has_phenotypic_profile.toString),
                       profileURI
                     )
                   )
    } yield statement).toSet
  }

  implicit def taxonToOWLClass(taxon: TaxonNode): OWLClass = factory.getOWLClass(taxon.iri)

  def taxonProfileURI(taxon: TaxonNode) = s"${taxon.iri.toString}#profile"

  def toSequential(associations: StateAssociations): Map[TaxonNode, Map[Character, Set[State]]] =
    associations.map({ case (taxon, states) => taxon -> states.seq.toMap }).seq.toMap

  def postorder(
    node: TaxonNode,
    model: Model,
    startingAssociations: StateAssociations,
    startingProfiles: StateAssociations
  ): (StateAssociations, StateAssociations)                                                     = {
    val children   = (for {
      s   <- model
             .listStatements(null, ResourceFactory.createProperty(rdfsSubClassOf.toString), node.asJenaNode)
             .asScala
             .toList
      term = s.getSubject
      if term.getURI != OWLNothing
      // check if blank node
      if !term.isAnon
      if term.getURI.startsWith("http://purl.obolibrary.org/obo/VTO_")
    } yield TaxonNode(IRI.create(term.getURI))).toSet
    val nodeStates = startingAssociations.getOrElse(node, Map.empty[Character, Set[State]])
    if (children.isEmpty)
      (Map(node -> nodeStates), Map.empty)
    else {
      val (subtreeAssociationsGroups, subtreeProfilesGroups) =
        children.par.map(postorder(_, model, startingAssociations, startingProfiles)).unzip
      val subtreeAssociations                                = subtreeAssociationsGroups.flatten.toMap
      val subtreeProfiles                                    = subtreeProfilesGroups.flatten.toMap
      val charactersWithAssociations                         = subtreeAssociations.values.flatMap(_.keys).toSet ++ nodeStates.keys
      val currentNodeAssociationsAndProfile                  = for {
        character <- charactersWithAssociations
      } yield {
        val nodeStateSet                                              = nodeStates.getOrElse(character, Set.empty)
        val childrenStateSets                                         = children.map(subtreeAssociations(_).getOrElse(character, Set.empty))
        val allStateSets                                              = childrenStateSets + nodeStateSet
        val nonEmptyStateSets                                         = allStateSets.filter(_.nonEmpty)
        val sharedStates: Set[State]                                  = nonEmptyStateSets.size match {
          case 0 => Set.empty
          case 1 => nonEmptyStateSets.head
          case _ => nonEmptyStateSets.reduce(_ intersect _)
        }
        val (currentStates: Set[State], statesForProfile: Set[State]) =
          if (sharedStates.nonEmpty) (sharedStates, Set.empty)
          else
            allStateSets.size match {
              case 0 => (Set.empty, Set.empty)
              case 1 => (allStateSets.head, Set.empty)
              case _ =>
                val unionStates = allStateSets.reduce(_ union _)
                (unionStates, unionStates)
            }
        (character -> currentStates, character -> statesForProfile)
      }
      val (currentNodeAssociations, rawProfile)              = currentNodeAssociationsAndProfile.unzip
      val profileWithoutEmptyCharacters                      = rawProfile.filter { case (character, states) => states.nonEmpty }
      (
        subtreeAssociations + (node -> currentNodeAssociations.toMap),
        subtreeProfiles + (node     -> profileWithoutEmptyCharacters.toMap)
      )
    }
  }

  def queryAssociations(model: Model): Set[StateAssociation] = {
    val qexec   = QueryExecutionFactory.create(associationsQuery.text, model)
    val results = qexec.execSelect()

    results.asScala.map(StateAssociation(_)).toSet
  }

  val associationsQuery: QueryText =
    sparql"""
     SELECT DISTINCT ?taxon ?matrix_char ?matrix_char_label ?state ?state_label
     WHERE {
     ?taxon $ExhibitsState ?state .
     ?state $RdfsLabel ?state_label .
     ?matrix_char $MayHaveStateValue ?state .
     ?matrix_char $RdfsLabel ?matrix_char_label
     }
    """

  def queryStatePhenotypes(model: Model): Map[State, Set[Phenotype]] = {
    val qexec   = QueryExecutionFactory.create(phenotypesQuery.text, model)
    val results = qexec.execSelect()
    results.asScala
      .map { bindings =>
        (
          State(IRI.create(bindings.getResource("state").getURI), bindings.getLiteral("state_label").getLexicalForm),
          Phenotype(IRI.create(bindings.getResource("phenotype").getURI))
        )
      }
      .toIterable
      .groupBy {
        case (state, phenotype) => state
      }
      .map {
        case (state, statesWithPhenotypes) =>
          state -> statesWithPhenotypes.map {
            case (state, phenotype) => phenotype
          }.toSet
      }

  }

  val phenotypesQuery: QueryText =
    sparql"""
    SELECT DISTINCT ?state ?state_label ?phenotype
    WHERE {
    ?state $RdfsLabel ?state_label .
    ?state $DescribesPhenotype ?phenotype
    }
    """

}

case class TaxonNode(iri: IRI) {
  def asJenaNode: RDFNode = ResourceFactory.createResource(iri.toString)
}

case class Character(iri: IRI, label: String)

case class State(iri: IRI, label: String)

case class Phenotype(iri: IRI)

case class StateAssociation(taxon: TaxonNode, character: Character, state: State)

object StateAssociation {

  def apply(result: QuerySolution): StateAssociation =
    StateAssociation(
      TaxonNode(IRI.create(result.getResource("taxon").getURI)),
      Character(
        IRI.create(result.getResource("matrix_char").getURI),
        result.getLiteral("matrix_char_label").getLexicalForm
      ),
      State(IRI.create(result.getResource("state").getURI), result.getLiteral("state_label").getLexicalForm)
    )

}
