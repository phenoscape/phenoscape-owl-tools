package org.phenoscape.owl

import scala.collection.JavaConversions._

import org.phenoscape.owl.Vocab._
import org.phenoscape.scowl.OWL.Class
import org.phenoscape.scowl.OWL.Individual
import org.phenoscape.scowl.OWL.ScowlClassExpression
import org.phenoscape.scowl.OWL.ScowlNamedObject
import org.phenoscape.scowl.OWL.ScowlObjectProperty
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
    val absenceClass = Class(getAbsenceIRI(classIRI))
    val notHasPartClass = Class(NegationClassGenerator.getNegationIRI(NamedRestrictionGenerator.getRestrictionIRI(has_part.getIRI, classIRI)))
    Set(
      factory.getOWLDeclarationAxiom(absenceClass),
      absenceClass EquivalentTo (has_part some (LacksAllPartsOfType and (towards value Individual(classIRI)))),
      absenceClass EquivalentTo (has_part some (inheres_in some notHasPartClass)),
      absenceClass Annotation (absenceOf, ontClass.getIRI))
  }

  def getAbsenceIRI(classIRI: IRI): IRI = {
    return IRI.create("http://purl.org/phenoscape/lacks/" + classIRI.toString)
  }

  def getAbsenceOntologyIRI(ontology: OWLOntology): IRI = {
    return IRI.create("http://phenoscape.org/not_has_part/" + ontology.getOntologyID.getOntologyIRI.toString)
  }

  def generateAllAbsenceAxiomsForEntity(ontClass: OWLClass): Set[OWLAxiom] = {
    val hasPartAxioms = NamedRestrictionGenerator.createRestriction(has_part, ontClass)
    val namedHasPartClass = Class(NamedRestrictionGenerator.getRestrictionIRI(has_part.getIRI, ontClass.getIRI()))
    hasPartAxioms ++
      createAbsenceClass(ontClass) ++
      NegationClassGenerator.createNegationClassAxioms(namedHasPartClass)
  }

}