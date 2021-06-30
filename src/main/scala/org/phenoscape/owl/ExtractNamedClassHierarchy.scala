package org.phenoscape.owl

import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{AxiomType, IRI, OWLAxiom, OWLOntology}

import java.util.UUID
import scala.jdk.CollectionConverters._

object ExtractNamedClassHierarchy {

  def extractHierarchy(ontology: OWLOntology): OWLOntology = {
    val manager = ontology.getOWLOntologyManager
    val namedClassAxioms = for {
      axiom <- ontology.getAxioms(AxiomType.SUBCLASS_OF, Imports.INCLUDED).asScala
      if !axiom.getSubClass.isAnonymous
      if !axiom.getSuperClass.isAnonymous
    } yield axiom
    manager.createOntology(
      namedClassAxioms.toSet[OWLAxiom].asJava,
      IRI.create("http://example.org/" + UUID.randomUUID.toString)
    )
  }

}
