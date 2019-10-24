package org.phenoscape.owl

import scala.collection.JavaConversions._

import org.phenoscape.owl.Vocab._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology

object AbsenceClassGenerator extends OWLTask {

  val absenceOf = factory.getOWLAnnotationProperty(Vocab.ABSENCE_OF)
  val manager = OWLManager.createOWLOntologyManager

  def generateAbsenceClasses(ontology: OWLOntology): OWLOntology = {
    val manager = ontology.getOWLOntologyManager()
    val newIRI = getAbsenceOntologyIRI(ontology)
    val newAxioms = for {
      ontClass <- ontology.getClassesInSignature(false)
      axiom <- createAbsenceClass(ontClass)
    } yield axiom
    manager.createOntology(newAxioms, newIRI)
  }

  def createAbsenceClass(ontClass: OWLClass): Set[OWLAxiom] = {
    val classIRI = ontClass.getIRI
    val notImpliesPresenceOfClass = Class(NegationClassGenerator.getNegationIRI(NamedRestrictionGenerator.getRestrictionIRI(IMPLIES_PRESENCE_OF.getIRI, classIRI)))
    Set(
      notImpliesPresenceOfClass EquivalentTo (has_part some (LacksAllPartsOfType and (towards value Individual(classIRI)))),
      notImpliesPresenceOfClass Annotation(absenceOf, ontClass))
  }

  def getAbsenceOntologyIRI(ontology: OWLOntology): IRI = {
    return IRI.create("http://phenoscape.org/not_has_part/" + ontology.getOntologyID.getOntologyIRI.toString)
  }

  def generateAllAbsenceAxiomsForEntity(ontClass: OWLClass): Set[OWLAxiom] = {
    val presenceAxioms = NamedRestrictionGenerator.createRestriction(IMPLIES_PRESENCE_OF, ontClass)
    val namedPresenceClass = Class(NamedRestrictionGenerator.getRestrictionIRI(IMPLIES_PRESENCE_OF.getIRI, ontClass.getIRI))
    presenceAxioms ++
      createAbsenceClass(ontClass) ++
      NegationClassGenerator.createNegationClassAxioms(namedPresenceClass)
  }

}