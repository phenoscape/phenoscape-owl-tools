package org.phenoscape.owl.build

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileReader
import java.io.StringReader
import java.util.Properties

import scala.Option.option2Iterable
import scala.collection.JavaConversions._

import org.openrdf.model.impl.URIImpl
import org.openrdf.rio.RDFFormat
import org.phenoscape.owl.AbsenceClassGenerator
import org.phenoscape.owl.KnowledgeBaseBuilder
import org.phenoscape.owl.MaterializeInferences
import org.phenoscape.owl.NamedRestrictionGenerator
import org.phenoscape.owl.NegationClassGenerator
import org.phenoscape.owl.NegationHierarchyAsserter
import org.phenoscape.owl.PhenexToOWL
import org.phenoscape.owl.PropertyNormalizer
import org.phenoscape.owl.ReverseDevelopsFromRuleGenerator
import org.phenoscape.owl.TaxonomyConverter
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.Vocab.has_part
import org.phenoscape.owl.util.OBOUtil
import org.phenoscape.owl.util.OntologyUtil
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.RDFXMLOntologyFormat
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntology

import com.bigdata.journal.Options
import com.bigdata.rdf.sail.BigdataSail
import com.bigdata.rdf.sail.BigdataSailRepository

object SingleMatrixKB extends KnowledgeBaseBuilder {

  def buildKB(inputMatrix: File, inputBigdataProperties: File, outputBigdataJournal: File, outputTboxFile: File): Unit = {
    val owlManager = OWLManager.createOWLOntologyManager()
    step("Loading ontologies")
    val phenoscapeVocab = processOntology(owlManager.loadOntology(IRI.create("http://purl.org/phenoscape/vocab.owl")))
    val ro = processOntology(owlManager.loadOntology(IRI.create("http://purl.obolibrary.org/obo/ro.owl")))
    val attributes = processOntology(owlManager.loadOntology(IRI.create("http://svn.code.sf.net/p/phenoscape/code/trunk/vocab/character_slims.obo")))
    val uberon = processOntology(owlManager.loadOntology(IRI.create("http://purl.obolibrary.org/obo/uberon/ext.owl")))
    val pato = processOntology(owlManager.loadOntology(IRI.create("http://purl.obolibrary.org/obo/pato.owl")))
    val bspo = processOntology(owlManager.loadOntology(IRI.create("http://purl.obolibrary.org/obo/bspo.owl")))
    val go = processOntology(owlManager.loadOntology(IRI.create("http://purl.obolibrary.org/obo/go.owl")))
    val taxrank = processOntology(owlManager.loadOntology(IRI.create("http://purl.obolibrary.org/obo/taxrank.owl")))
    val vto = processOntology(owlManager.loadOntology(IRI.create("http://purl.obolibrary.org/obo/vto.owl")))

    step("Querying anatomical entities")
    val coreReasoner = reasoner(uberon, pato, bspo, go, ro, phenoscapeVocab)
    val anatomicalEntities = coreReasoner.getSubClasses(Class(Vocab.ANATOMICAL_ENTITY), false).getFlattened().filterNot(_.isOWLNothing())
    coreReasoner.dispose()

    step("Creating VTO instances")
    val vtoIndividuals = TaxonomyConverter.createInstanceOntology(vto)

    step("Converting NeXML to OWL")

    val vocabForNeXML = combine(uberon, pato, bspo, go, ro, phenoscapeVocab)
    val converter = new PhenexToOWL()
    val nexOntology = PropertyNormalizer.normalize(converter.convert(inputMatrix, vocabForNeXML))
    val nexmlTBoxAxioms = owlManager.createOntology(nexOntology.getTBoxAxioms(false))

    val hasParts = owlManager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(has_part, _)))
    val presences = owlManager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(Vocab.IMPLIES_PRESENCE_OF, _)))
    val inherers = owlManager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(Vocab.inheres_in, _)))
    val inherersInPartOf = owlManager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(Vocab.inheres_in_part_of, _)))
    val towards = owlManager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(Vocab.TOWARDS, _)))
    val absences = owlManager.createOntology(anatomicalEntities.flatMap(AbsenceClassGenerator.createAbsenceClass(_)))
    val namedHasPartClasses = anatomicalEntities.map(_.getIRI()).map(NamedRestrictionGenerator.getRestrictionIRI(has_part.getIRI, _)).map(Class(_))
    val absenceNegationEquivalences = owlManager.createOntology(namedHasPartClasses.flatMap(NegationClassGenerator.createNegationClassAxioms(_, hasParts)))
    val developsFromRulesForAbsence = owlManager.createOntology(anatomicalEntities.flatMap(ReverseDevelopsFromRuleGenerator.createRules(_)).toSet[OWLAxiom])

    val allTBox = combine(uberon, pato, bspo, go, vto, ro, phenoscapeVocab,
      hasParts, inherers, inherersInPartOf, towards, presences, absences, absenceNegationEquivalences, developsFromRulesForAbsence,
      nexmlTBoxAxioms)
    println("tbox class count: " + allTBox.getClassesInSignature().size())
    println("tbox logical axiom count: " + allTBox.getLogicalAxiomCount())
    val tBoxWithoutDisjoints = OntologyUtil.ontologyWithoutDisjointAxioms(allTBox)

    step("Materializing tbox classification")
    val tboxReasoner = reasoner(tBoxWithoutDisjoints)
    val inferredAxioms = owlManager.createOntology()
    MaterializeInferences.materializeInferences(inferredAxioms, tboxReasoner)
    tboxReasoner.dispose()

    step("Asserting reverse negation hierarchy")
    val hierarchyAxioms = NegationHierarchyAsserter.assertNegationHierarchy(tBoxWithoutDisjoints, inferredAxioms)
    owlManager.addAxioms(inferredAxioms, hierarchyAxioms)
    val negationReasoner = reasoner(tBoxWithoutDisjoints, inferredAxioms)
    MaterializeInferences.materializeInferences(inferredAxioms, negationReasoner)

    if (negationReasoner.getUnsatisfiableClasses().getEntitiesMinusBottom().isEmpty()) {
      println("SUCCESS: all classes are satisfiable.")
    } else {
      println("WARNING: some classes are unsatisfiable.")
      println(negationReasoner.getUnsatisfiableClasses())
    }
    negationReasoner.dispose()

    step("Writing tbox axioms for ELK")
    write(combine(tBoxWithoutDisjoints, inferredAxioms), outputTboxFile.getAbsolutePath)
    tboxReasoner.dispose()

    val everything = combine(uberon, pato, bspo, go, vto, vtoIndividuals, taxrank, ro, phenoscapeVocab, attributes,
      hasParts, inherers, inherersInPartOf, towards, presences, absences, absenceNegationEquivalences, developsFromRulesForAbsence,
      nexOntology, inferredAxioms)
    val bigdataProperties = new Properties()
    bigdataProperties.load(new FileReader(inputBigdataProperties))
    bigdataProperties.setProperty(Options.FILE, outputBigdataJournal.getAbsolutePath)
    val sail = new BigdataSail(bigdataProperties)
    val repository = new BigdataSailRepository(sail);
    repository.initialize()
    val connection = repository.getConnection;
    connection.setAutoCommit(false);
    val baseURI = ""
    val graphURI = new URIImpl("http://kb.phenoscape.org/")

    val rdfOutput = new ByteArrayOutputStream()
    owlManager.saveOntology(everything, new RDFXMLOntologyFormat(), rdfOutput)
    rdfOutput.close()
    val rdfReader = new StringReader(rdfOutput.toString("utf-8"))
    connection.add(rdfReader, baseURI, RDFFormat.RDFXML, graphURI)
    connection.commit();
    connection.close();
    rdfReader.close()

    step("Done")

  }

  def processOntology(ont: OWLOntology): OWLOntology = {
    val definedByAxioms = ont.getClassesInSignature map OBOUtil.createDefinedByAnnotation flatMap (_.toSet)
    ont.getOWLOntologyManager.addAxioms(ont, definedByAxioms)
    PropertyNormalizer.normalize(ont)
  }

}