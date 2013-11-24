package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConversions._
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.util.OWLEntityRenamer
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.io.RDFXMLOntologyFormat
import org.semanticweb.owlapi.apibinding.OWLManager

object PropertyNormalizer extends OWLTask {

  val properties = Map(
    IRI.create("http://purl.obolibrary.org/obo/OBO_REL_part_of") -> IRI.create("http://purl.obolibrary.org/obo/BFO_0000050"),
    IRI.create("http://purl.obolibrary.org/obo/TODO_part_of") -> IRI.create("http://purl.obolibrary.org/obo/BFO_0000050"),
    IRI.create("http://purl.obolibrary.org/obo/fma#part_of") -> IRI.create("http://purl.obolibrary.org/obo/BFO_0000050"),
    IRI.create("http://purl.obolibrary.org/obo/OBO_REL#_part_of") -> IRI.create("http://purl.obolibrary.org/obo/BFO_0000050"),
    IRI.create("http://purl.obolibrary.org/obo/OBO_REL_has_part") -> IRI.create("http://purl.obolibrary.org/obo/BFO_0000051"),
    IRI.create("http://purl.obolibrary.org/obo/TODO_has_part") -> IRI.create("http://purl.obolibrary.org/obo/BFO_0000051"),
    IRI.create("http://purl.obolibrary.org/obo/TODO_develops_from") -> IRI.create("http://purl.obolibrary.org/obo/RO_0002202"),
    IRI.create("http://purl.obolibrary.org/obo/tao#develops_from") -> IRI.create("http://purl.obolibrary.org/obo/RO_0002202"),
    IRI.create("http://purl.obolibrary.org/obo/OBO_REL_bearer_of") -> IRI.create("http://purl.obolibrary.org/obo/BFO_0000053"),
    IRI.create("http://purl.obolibrary.org/obo/OBO_REL#_has_quality") -> IRI.create("http://purl.obolibrary.org/obo/BFO_0000053"),
    IRI.create("http://purl.obolibrary.org/obo/tao#has_quality") -> IRI.create("http://purl.obolibrary.org/obo/BFO_0000053"),
    IRI.create("http://purl.obolibrary.org/obo/pato#_inheres_in") -> IRI.create("http://purl.obolibrary.org/obo/BFO_0000052"),
    IRI.create("http://purl.obolibrary.org/obo/OBO_REL_inheres_in") -> IRI.create("http://purl.obolibrary.org/obo/BFO_0000052"),
    IRI.create("http://purl.obolibrary.org/obo/TODO_inheres_in") -> IRI.create("http://purl.obolibrary.org/obo/BFO_0000052"),
    IRI.create("http://purl.obolibrary.org/obo/RO_overlaps") -> IRI.create("http://purl.obolibrary.org/obo/RO_0002131"),
    IRI.create("http://purl.obolibrary.org/obo/towards") -> IRI.create("http://purl.obolibrary.org/obo/pato#towards"),
    IRI.create("http://purl.obolibrary.org/obo/OBO_REL_towards") -> IRI.create("http://purl.obolibrary.org/obo/pato#towards"),
    IRI.create("http://purl.obolibrary.org/obo/TODO_towards") -> IRI.create("http://purl.obolibrary.org/obo/pato#towards"),
    IRI.create("http://purl.obolibrary.org/obo/hp/hp-logical-definitions#involves") -> Vocab.INVOLVES.getIRI)

  def normalize(ontology: OWLOntology): OWLOntology = {
    val manager = ontology.getOWLOntologyManager
    val renamer = new OWLEntityRenamer(manager, Set(ontology))
    for ((key, value) <- properties) {
      manager.applyChanges(renamer.changeIRI(key, value))
    }
    return ontology
  }

}