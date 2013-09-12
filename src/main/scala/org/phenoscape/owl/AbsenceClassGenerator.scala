package org.phenoscape.owl

import scala.collection.JavaConversions._
import scala.collection.Set
import scala.collection.mutable
import java.io.File
import org.nescent.strix.OWL._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLAxiom

object AbsenceClassGenerator extends OWLTask {

    val hasPart = OWLManager.getOWLDataFactory().getOWLObjectProperty(Vocab.HAS_PART);
    val lacksAllPartsOfType = Class(Vocab.LACKS_ALL_PARTS_OF_TYPE);
    val towards = ObjectProperty(Vocab.TOWARDS);
    val absenceOf = factory.getOWLAnnotationProperty(Vocab.ABSENCE_OF);
    val involves = ObjectProperty(Vocab.INVOLVES);
    val manager = this.getOWLOntologyManager();

    def main(args: Array[String]): Unit = {
            val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)));
            val absenceOntology = generateAbsenceClasses(ontology);
            manager.saveOntology(absenceOntology, IRI.create(new File(args(1))));
    }

    def generateAbsenceClasses(ontology: OWLOntology): OWLOntology = {
            val manager = ontology.getOWLOntologyManager();
            val factory = manager.getOWLDataFactory();
            val newIRI = getAbsenceOntologyIRI(ontology);
            val absenceOntology = manager.createOntology(newIRI);
            ontology.getClassesInSignature(false).map(createAbsenceClass(_)).foreach(manager.addAxioms(absenceOntology, _));
            return absenceOntology;
    }

    def createAbsenceClass(ontClass: OWLClass): Set[OWLAxiom] = {
            val axioms: mutable.Set[OWLAxiom] = mutable.Set();;
            val classIRI = ontClass.getIRI();
            val absenceClass = Class(getAbsenceIRI(classIRI));
            axioms.add(factory.getOWLDeclarationAxiom(absenceClass));
            axioms.add(absenceClass EquivalentTo (lacksAllPartsOfType and (towards value Individual(classIRI))));
            val notHasPartClass = Class(NegationClassGenerator.getNegationIRI(NamedRestrictionGenerator.getRestrictionIRI(hasPart.getIRI(), classIRI)));
            axioms.add(absenceClass EquivalentTo notHasPartClass);
            axioms.add(absenceClass Annotation (absenceOf, ontClass.getIRI()));
            //axioms.add(absenceClass SubClassOf (involves some ontClass)); //this is dangerous
            return axioms;
    }

    def getAbsenceIRI(classIRI: IRI): IRI = {
            return IRI.create("http://purl.org/phenoscape/lacks/" + classIRI.toString());
    }

    def getAbsenceOntologyIRI(ontology: OWLOntology): IRI = {
            return IRI.create("http://phenoscape.org/not_has_part/" + ontology.getOntologyID().getOntologyIRI().toString());
    }

    def generateAllAbsenceAxiomsForEntity(ontClass: OWLClass): Set[OWLAxiom] = {
            val axioms: mutable.Set[OWLAxiom] = mutable.Set();;
            val hasPartAxioms = NamedRestrictionGenerator.createRestriction(ObjectProperty(Vocab.HAS_PART), ontClass);
            val tempOntology = manager.createOntology(hasPartAxioms);
            axioms.addAll(hasPartAxioms);
            axioms.addAll(createAbsenceClass(ontClass));
            val namedHasPartClass = Class(NamedRestrictionGenerator.getRestrictionIRI(Vocab.HAS_PART, ontClass.getIRI()));
            axioms.addAll(NegationClassGenerator.createNegationClassAxioms(namedHasPartClass, tempOntology));
            return axioms;
    }

}