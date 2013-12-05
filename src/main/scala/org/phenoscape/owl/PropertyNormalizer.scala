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
    IRI.create("http://purl.obolibrary.org/obo/OBO_REL_part_of") -> Vocab.part_of.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/BFO_00000050") -> Vocab.part_of.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/TODO_part_of") -> Vocab.part_of.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/fma#part_of") -> Vocab.part_of.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/OBO_REL#_part_of") -> Vocab.part_of.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/mp#part_of") -> Vocab.part_of.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/xao#part_of") -> Vocab.part_of.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/zfa#part_of") -> Vocab.part_of.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/emapa#part_of") -> Vocab.part_of.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/OBO_REL_has_part") -> Vocab.has_part.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/TODO_has_part") -> Vocab.has_part.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/has_part") -> Vocab.has_part.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/TODO_develops_from") -> Vocab.DEVELOPS_FROM.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/tao#develops_from") -> Vocab.DEVELOPS_FROM.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/xao#develops_from") -> Vocab.DEVELOPS_FROM.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/emapa#develops_from") -> Vocab.DEVELOPS_FROM.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/fma#develops_from") -> Vocab.DEVELOPS_FROM.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/OBO_REL_bearer_of") -> Vocab.bearer_of.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/RO_0000053") -> Vocab.bearer_of.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/OBO_REL#_has_quality") -> Vocab.has_part.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/tao#has_quality") -> Vocab.has_part.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/inheres_in") -> Vocab.inheres_in.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/pato#_inheres_in") -> Vocab.inheres_in.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/pato#inheres_in") -> Vocab.inheres_in.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/OBO_REL_inheres_in") -> Vocab.inheres_in.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/TODO_inheres_in") -> Vocab.inheres_in.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/RO_0000052") -> Vocab.inheres_in.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/overlaps") -> IRI.create("http://purl.obolibrary.org/obo/RO_0002131"),
    IRI.create("http://purl.obolibrary.org/obo/RO_overlaps") -> IRI.create("http://purl.obolibrary.org/obo/RO_0002131"),
    IRI.create("http://purl.obolibrary.org/obo/towards") -> Vocab.TOWARDS.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/OBO_REL_towards") -> Vocab.TOWARDS.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/TODO_towards") -> Vocab.TOWARDS.getIRI,
    IRI.create("http://purl.obolibrary.org/obo/hp/hp-logical-definitions#involves") -> Vocab.INVOLVES.getIRI)

  def normalize(ontology: OWLOntology): OWLOntology = {
    val manager = ontology.getOWLOntologyManager
    val renamer = new OWLEntityRenamer(manager, Set(ontology))
    for ((key, value) <- properties) {
      if (key != value) manager.applyChanges(renamer.changeIRI(key, value))
    }
    return ontology
  }

}