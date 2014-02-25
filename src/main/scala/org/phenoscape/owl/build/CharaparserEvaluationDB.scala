package org.phenoscape.owl.build

import java.io.BufferedReader
import java.io.File
import java.io.StringReader
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.io.Source
import org.apache.commons.io.FileUtils
import org.apache.log4j.BasicConfigurator
import org.apache.log4j.Level
import org.apache.log4j.Logger
import org.phenoscape.scowl.OWL._
import org.obolibrary.obo2owl.Obo2Owl
import org.obolibrary.oboformat.parser.OBOFormatParser
import org.phenoscape.owl.AbsenceClassGenerator
import org.phenoscape.owl.EQCharactersGenerator
import org.phenoscape.owl.KnowledgeBaseBuilder
import org.phenoscape.owl.MaterializeInferences
import org.phenoscape.owl.MaterializeSubClassOfClosure
import org.phenoscape.owl.MaterializeSubClassOfClosureToNTriples
import org.phenoscape.owl.NamedRestrictionGenerator
import org.phenoscape.owl.NegationClassGenerator
import org.phenoscape.owl.NegationHierarchyAsserter
import org.phenoscape.owl.PhenexToOWL
import org.phenoscape.owl.PropertyNormalizer
import org.phenoscape.owl.ReverseDevelopsFromRuleGenerator
import org.phenoscape.owl.TaxonomyConverter
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.mod.human.HumanPhenotypesToOWL
import org.phenoscape.owl.mod.mgi.MGIExpressionToOWL
import org.phenoscape.owl.mod.mgi.MGIGeneticMarkersToOWL
import org.phenoscape.owl.mod.mgi.MGIPhenotypesToOWL
import org.phenoscape.owl.mod.xenbase.XenbaseExpressionToOWL
import org.phenoscape.owl.mod.xenbase.XenbaseGenesToOWL
import org.phenoscape.owl.mod.zfin.ZFINExpressionToOWL
import org.phenoscape.owl.mod.zfin.ZFINGeneticMarkersToOWL
import org.phenoscape.owl.mod.zfin.ZFINPhenotypesToOWL
import org.phenoscape.owl.mod.zfin.ZFINPreviousGeneNamesToOWL
import org.semanticweb.owlapi.model.OWLAxiom
import org.phenoscape.owl.Vocab._
import java.io.FileReader
import org.phenoscape.owl.util.OntologyUtil
import com.bigdata.rdf.sail.BigdataSail
import com.bigdata.rdf.sail.BigdataSailRepository
import org.openrdf.model.impl.URIImpl
import java.util.Properties
import org.openrdf.rio.RDFFormat
import com.bigdata.journal.Options

object CharaparserEvaluationDB extends KnowledgeBaseBuilder {

  BasicConfigurator.configure()
  Logger.getRootLogger().setLevel(Level.ERROR)

  val cwd = System.getProperty("user.dir")
  val STAGING = new File("staging")
  val KB = new File("staging/kb")
  val NEXML = new File("staging/nexml")
  val BIGDATA_PROPERTIES = new File("bigdata.properties")
  val BIGDATA_JOURNAL = new File("staging/bigdata.jnl")
  STAGING.mkdir()
  KB.mkdir()
  cd(KB)

  step("Loading ontologies")
  val roAnnotations = load(new File(cwd + "/staging/sources/ro-annotations.owl"))
  val bfoMinimal = load(new File(cwd + "/staging/sources/bfo-classes-minimal.owl"))
  val roCore = load(new File(cwd + "/staging/sources/ro-core.owl"))
  val temporalIntervals = load(new File(cwd + "/staging/sources/temporal-intervals.owl"))
  val roRelease = load(new File(cwd + "/staging/sources/ro.owl"))
  val ro = combine(roRelease, temporalIntervals, roCore, bfoMinimal, roAnnotations)
  write(ro, cwd + "/staging/kb/ro.owl")
  val phenoscapeVocab = load(new File(cwd + "/staging/sources/phenoscape-vocab.owl"))
  write(phenoscapeVocab, cwd + "/staging/kb/phenoscape-vocab.owl")
  val attributes = load(new File(cwd + "/staging/sources/character_slims.obo"))
  write(attributes, cwd + "/staging/kb/attributes.owl")
  val ext = load(new File(cwd + "/staging/sources/phenoscape-ext-simple.owl"))
  val termRequests = load(new File(cwd + "/staging/sources/term_requests_ALL.owl"))
  val uberon = combine(ext, termRequests)
  write(uberon, cwd + "/staging/kb/uberon.owl")
  val pato = load(new File(cwd + "/staging/sources/pato.owl"))
  write(pato, cwd + "/staging/kb/pato.owl")
  val bspo = load(new File(cwd + "/staging/sources/bspo.owl"))
  write(bspo, cwd + "/staging/kb/bspo.owl")
  val taxrank = load(new File(cwd + "/staging/sources/taxrank.owl"))
  write(taxrank, cwd + "/staging/kb/taxrank.owl")
  val vto = load(new File(cwd + "/staging/sources/vto.owl"))
  write(vto, cwd + "/staging/kb/vto.owl")

  step("Querying entities and qualities")
  val coreReasoner = reasoner(uberon, pato, bspo, ro, phenoscapeVocab)
  //FIXME may need more than just anatomical entities
  val anatomicalEntities = coreReasoner.getSubClasses(Class(Vocab.ANATOMICAL_ENTITY), false).getFlattened().filterNot(_.isOWLNothing())
  //val qualities = coreReasoner.getSubClasses(Class(Vocab.QUALITY), false).getFlattened().filterNot(_.isOWLNothing())
  coreReasoner.dispose()

  step("Creating VTO instances")
  val vtoIndividuals = TaxonomyConverter.createInstanceOntology(vto)
  write(vtoIndividuals, cwd + "/staging/kb/vto-individuals.owl")

  step("Converting NeXML to OWL")
  cd(NEXML)
  val filesToConvert = FileUtils.listFiles(new File(cwd + "/staging/nexml"), Array("xml"), true).filterNot(_.getName() == "catalog-v001.xml")
  cd(KB)
  val nexmlTBoxAxioms: mutable.Set[OWLAxiom] = mutable.Set()
  for (file <- filesToConvert) {
    val converter = new PhenexToOWL()
    val nexOntology = PropertyNormalizer.normalize(converter.convert(file))
    nexmlTBoxAxioms.addAll(nexOntology.getTBoxAxioms(false))
    write(nexOntology, cwd + "/staging/kb/" + file.getName().replaceAll(".xml$", ".owl"))
  }

  step("Generating tbox")
  val tboxFromData = manager.createOntology(nexmlTBoxAxioms)
  val hasParts = manager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(has_part, _)))
  val inherers = manager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(inheres_in, _)))
  val inherersInPartOf = manager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(inheres_in_part_of, _)))
  val towardses = manager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(TOWARDS, _)))
  val absences = manager.createOntology(anatomicalEntities.flatMap(AbsenceClassGenerator.createAbsenceClass(_)))
  val namedHasPartClasses = anatomicalEntities.map(_.getIRI()).map(NamedRestrictionGenerator.getRestrictionIRI(has_part.getIRI, _)).map(Class(_))
  val absenceNegationEquivalences = manager.createOntology(namedHasPartClasses.flatMap(NegationClassGenerator.createNegationClassAxioms(_, hasParts)))
  val developsFromRulesForAbsence = manager.createOntology(anatomicalEntities.flatMap(ReverseDevelopsFromRuleGenerator.createRules(_)).toSet[OWLAxiom])

  val allTBox = combine(uberon, pato, bspo, vto,
    hasParts, inherers, inherersInPartOf, towardses, absences, absenceNegationEquivalences, developsFromRulesForAbsence, tboxFromData, ro, phenoscapeVocab)
  println("tbox class count: " + allTBox.getClassesInSignature().size())
  println("tbox logical axiom count: " + allTBox.getLogicalAxiomCount())

  step("Materializing tbox classification")
  val tboxReasoner = reasoner(allTBox)
  val inferredAxioms = manager.createOntology()
  MaterializeInferences.materializeInferences(inferredAxioms, tboxReasoner)
  tboxReasoner.dispose()

  step("Asserting reverse negation hierarchy")
  val hierarchyAxioms = NegationHierarchyAsserter.assertNegationHierarchy(allTBox, inferredAxioms)
  manager.addAxioms(inferredAxioms, hierarchyAxioms)
  val negationReasoner = reasoner(allTBox, inferredAxioms)
  MaterializeInferences.materializeInferences(inferredAxioms, negationReasoner)

  if (negationReasoner.getUnsatisfiableClasses().getEntitiesMinusBottom().isEmpty()) {
    println("SUCCESS: all classes are satisfiable.")
  } else {
    println("WARNING: some classes are unsatisfiable.")
    println(negationReasoner.getUnsatisfiableClasses())
  }

  step("Writing generated and inferred tbox axioms")
  write(combine(hasParts, inherers, inherersInPartOf, towardses, absences, absenceNegationEquivalences, developsFromRulesForAbsence, inferredAxioms), cwd + "/staging/kb/generated.owl")

  step("Writing tbox axioms for ELK")
  write(combine(allTBox, inferredAxioms), cwd + "/staging/kb/tbox.owl")

  step("Materializing subclass closure")
  MaterializeSubClassOfClosureToNTriples.writeClosureToFile(negationReasoner, new File(cwd + "/staging/kb/hierarchy_closure.nt"))
  negationReasoner.dispose()

  step("Creating Bigdata triplestore")
  val bigdataProperties = new Properties()
  bigdataProperties.load(new FileReader(BIGDATA_PROPERTIES))
  bigdataProperties.setProperty(Options.FILE, BIGDATA_JOURNAL.getAbsolutePath)
  val sail = new BigdataSail(bigdataProperties)
  val repository = new BigdataSailRepository(sail);
  repository.initialize()
  val connection = repository.getConnection;
  connection.setAutoCommit(false);

  val baseURI = ""
  val mainGraphURI = new URIImpl("http://kb.phenoscape.org/")
  val closureURI = new URIImpl("http://kb.phenoscape.org/closure")
  for (rdfFile <- FileUtils.listFiles(KB, Array("owl"), true)) {
    connection.add(rdfFile, baseURI, RDFFormat.RDFXML, mainGraphURI)
  }
  connection.add(new File(cwd + "/staging/kb/hierarchy_closure.nt"), baseURI, RDFFormat.NTRIPLES, closureURI)
  connection.commit();
  connection.close();

  step("Done")

}