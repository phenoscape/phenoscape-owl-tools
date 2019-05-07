package org.phenoscape.owl

import java.util.UUID
import scala.collection.JavaConverters._
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLAxiom

object ExtractNamedClassHierarchy extends OWLTask {

  def extractHierarchy(ontology: OWLOntology): OWLOntology = {
    val manager = ontology.getOWLOntologyManager
    val namedClassAxioms = for {
      axiom <- ontology.getAxioms(AxiomType.SUBCLASS_OF, true).asScala
      if !axiom.getSubClass.isAnonymous
      if !axiom.getSuperClass.isAnonymous
    } yield axiom
    manager.createOntology(namedClassAxioms.toSet[OWLAxiom].asJava, IRI.create("http://example.org/" + UUID.randomUUID.toString))
  }

}