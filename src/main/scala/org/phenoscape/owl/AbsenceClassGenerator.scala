package org.phenoscape.owl

import org.phenoscape.owl.Vocab._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLAxiom, OWLClass}

object AbsenceClassGenerator {

  val factory = OWLManager.getOWLDataFactory
  val absenceOf = factory.getOWLAnnotationProperty(Vocab.ABSENCE_OF)

  def createAbsenceClass(ontClass: OWLClass): Set[OWLAxiom] = {
    val classIRI = ontClass.getIRI
    val notImpliesPresenceOfClass = Class(
      NegationClassGenerator
        .getNegationIRI(NamedRestrictionGenerator.getRestrictionIRI(IMPLIES_PRESENCE_OF.getIRI, classIRI))
    )
    Set(
      notImpliesPresenceOfClass EquivalentTo (has_part some (LacksAllPartsOfType and (towards value Individual(
        classIRI
      )))),
      notImpliesPresenceOfClass Annotation (absenceOf, ontClass)
    )
  }

  def generateAllAbsenceAxiomsForEntity(ontClass: OWLClass): Set[OWLAxiom] = {
    val presenceAxioms = NamedRestrictionGenerator.createRestriction(IMPLIES_PRESENCE_OF, ontClass)
    val namedPresenceClass = Class(
      NamedRestrictionGenerator.getRestrictionIRI(IMPLIES_PRESENCE_OF.getIRI, ontClass.getIRI)
    )
    presenceAxioms ++
      createAbsenceClass(ontClass) ++
      NegationClassGenerator.createNegationClassAxioms(namedPresenceClass)
  }

}
