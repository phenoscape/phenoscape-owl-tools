package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConversions._
import org.phenoscape.scowl.OWL.Class
import org.phenoscape.scowl.OWL.Individual
import org.phenoscape.scowl.OWL.OWLClassExpressionToClassExpression
import org.phenoscape.scowl.OWL.OWLNamedObjectToScowlNamedObject
import org.phenoscape.scowl.OWL.OWLObjectPropertyToProperty
import org.phenoscape.scowl.OWL.ObjectProperty
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLClassExpression

object AbsenceClassGenerator extends OWLTask {

  val lacksAllPartsOfType = Class(Vocab.LACKS_ALL_PARTS_OF_TYPE)
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
    val notHasPartClass = Class(NegationClassGenerator.getNegationIRI(NamedRestrictionGenerator.getRestrictionIRI(Vocab.HAS_PART.getIRI, classIRI)))
    Set(
      factory.getOWLDeclarationAxiom(absenceClass),
      absenceClass EquivalentTo (lacksAllPartsOfType and (Vocab.TOWARDS value Individual(classIRI))),
      absenceClass EquivalentTo notHasPartClass,
      absenceClass Annotation (absenceOf, ontClass.getIRI))
    //absenceClass SubClassOf (involves some ontClass) //this is dangerous
  }

  def getAbsenceIRI(classIRI: IRI): IRI = {
    return IRI.create("http://purl.org/phenoscape/lacks/" + classIRI.toString)
  }

  def getAbsenceOntologyIRI(ontology: OWLOntology): IRI = {
    return IRI.create("http://phenoscape.org/not_has_part/" + ontology.getOntologyID.getOntologyIRI.toString)
  }

  def generateAllAbsenceAxiomsForEntity(ontClass: OWLClass): Set[OWLAxiom] = {
    val hasPartAxioms = NamedRestrictionGenerator.createRestriction(Vocab.HAS_PART, ontClass)
    val tempOntology = manager.createOntology(hasPartAxioms)
    val namedHasPartClass = Class(NamedRestrictionGenerator.getRestrictionIRI(Vocab.HAS_PART.getIRI, ontClass.getIRI()))
    hasPartAxioms ++
      createAbsenceClass(ontClass) ++
      NegationClassGenerator.createNegationClassAxioms(namedHasPartClass, tempOntology)
  }

}