package org.phenoscape.owl.build

import java.io.BufferedReader
import java.io.File
import java.io.FileReader
import java.io.StringReader
import java.util.Properties
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.io.Source
import org.apache.commons.io.FileUtils
import org.apache.log4j.BasicConfigurator
import org.apache.log4j.Level
import org.apache.log4j.Logger
import org.obolibrary.oboformat.parser.OBOFormatParser
import org.openrdf.rio.RDFFormat
import org.phenoscape.owl.AbsenceClassGenerator
import org.phenoscape.owl.EQCharactersGenerator
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
import org.phenoscape.owl.util.OntologyUtil
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.model.OWLAxiom
import com.bigdata.journal.Options
import com.bigdata.rdf.sail.BigdataSail
import com.bigdata.rdf.sail.BigdataSailRepository
import org.openrdf.model.impl.URIImpl
import org.semanticweb.owlapi.io.OWLOntologyDocumentSource
import org.semanticweb.owlapi.io.ReaderDocumentSource
import org.openrdf.query.QueryLanguage
import java.io.FileOutputStream
import java.io.BufferedOutputStream
import org.openrdf.rio.turtle.TurtleWriter
import org.openrdf.query.algebra.evaluation.TripleSource
import org.phenoscape.owl.mod.xenbase.XenbasePhenotypesToOWL
import org.semanticweb.owlapi.apibinding.OWLManager

object PhenoscapeKB extends KnowledgeBaseBuilder {

  BasicConfigurator.configure()
  Logger.getRootLogger().setLevel(Level.ERROR)

  val manager = getManager

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
  val uberonReferences = load(new File(cwd + "/staging/sources/references.owl"))
  val uberonChebi = load(new File(cwd + "/staging/sources/chebi_import.owl"))
  val uberonCL = load(new File(cwd + "/staging/sources/cl_import.owl"))
  val uberonGO = load(new File(cwd + "/staging/sources/go_import.owl"))
  val uberonTaxon = load(new File(cwd + "/staging/sources/ncbitaxon_import.owl"))
  val uberonPATO = load(new File(cwd + "/staging/sources/pato_import.owl"))
  val uberonPR = load(new File(cwd + "/staging/sources/pr_import.owl"))
  val uberonENVO = load(new File(cwd + "/staging/sources/envo_import.owl"))
  val uberonNBO = load(new File(cwd + "/staging/sources/nbo_import.owl"))
  val uberonRO = load(new File(cwd + "/staging/sources/ro_import.owl"))
  val ext = load(new File(cwd + "/staging/sources/ext.owl"))
  val uberon = combine(ext, uberonReferences, uberonChebi, uberonGO, uberonTaxon, uberonPATO, uberonPR, uberonENVO, uberonNBO, uberonRO)
  write(uberon, cwd + "/staging/kb/uberon.owl")
  val homology = load(new File(cwd + "/staging/sources/homology.owl"))
  write(homology, cwd + "/staging/kb/homology.owl")
  val pato = load(new File(cwd + "/staging/sources/pato.owl"))
  write(pato, cwd + "/staging/kb/pato.owl")
  val bspo = load(new File(cwd + "/staging/sources/bspo.owl"))
  write(bspo, cwd + "/staging/kb/bspo.owl")
  val go = load(new File(cwd + "/staging/sources/go.owl"))
  write(go, cwd + "/staging/kb/go.owl")
  val taxrank = load(new File(cwd + "/staging/sources/taxrank.owl"))
  write(taxrank, cwd + "/staging/kb/taxrank.owl")
  val vto = load(new File(cwd + "/staging/sources/vto.owl"))
  write(vto, cwd + "/staging/kb/vto.owl")
  val collections = OWLManager.createOWLOntologyManager.loadOntologyFromOntologyDocument(new File(cwd + "/staging/sources/fish_collection_abbreviation.obo"))
  write(collections, cwd + "/staging/kb/fish_collection_abbreviation.owl")
  val zfa = load(new File(cwd + "/staging/sources/zfa.owl"))
  write(zfa, cwd + "/staging/kb/zfa.owl")
  val xao = load(new File(cwd + "/staging/sources/xao.owl"))
  write(xao, cwd + "/staging/kb/xao.owl")
  val hp = load(new File(cwd + "/staging/sources/hp.owl"))
  write(hp, cwd + "/staging/kb/hp.owl")
  val mp = load(new File(cwd + "/staging/sources/mp.owl"))
  write(mp, cwd + "/staging/kb/mp.owl")

  val hpEQ = load(new File(cwd + "/staging/sources/hp-equivalence-axioms-subq-ubr.owl"))
  write(hpEQ, cwd + "/staging/kb/hp-logical-definitions.owl")
  val mpEQ = load(new File(cwd + "/staging/sources/mp-equivalence-axioms-subq-ubr.owl"))
  write(mpEQ, cwd + "/staging/kb/mp-logical-definitions.owl")

  val zfaToUberon = load(new File(cwd + "/staging/sources/uberon-ext-bridge-to-zfa.owl"))
  write(zfaToUberon, cwd + "/staging/kb/uberon-ext-bridge-to-zfa.owl")
  val xaoToUberon = load(new File(cwd + "/staging/sources/uberon-bridge-to-xao.owl"))
  write(xaoToUberon, cwd + "/staging/kb/uberon-bridge-to-xao.owl")
  val fmaToUberon = load(new File(cwd + "/staging/sources/uberon-bridge-to-fma.owl"))
  write(fmaToUberon, cwd + "/staging/kb/uberon-bridge-to-fma.owl")
  val mgiToEMAPA = load(new File(cwd + "/staging/sources/mgi_anatomy.owl"))
  write(mgiToEMAPA, cwd + "/staging/kb/mgi_anatomy.owl")
  val emapa = load(new File(cwd + "/staging/sources/emapa.owl"))
  write(emapa, cwd + "/staging/kb/emapa.owl")
  val emapaToUberon = load(new File(cwd + "/staging/sources/uberon-bridge-to-emapa.owl"))
  write(mgiToEMAPA, cwd + "/staging/kb/uberon-bridge-to-emapa.owl.owl")

  step("Querying entities and qualities")
  val coreReasoner = reasoner(uberon, pato, bspo, go, ro, phenoscapeVocab)
  val anatomicalEntities = coreReasoner.getSubClasses(Class(Vocab.ANATOMICAL_ENTITY), false).getFlattened().filterNot(_.isOWLNothing())
  //val qualities = coreReasoner.getSubClasses(Class(Vocab.QUALITY), false).getFlattened().filterNot(_.isOWLNothing())
  coreReasoner.dispose()

  //  step("Generating EQ characters for analyses")
  //  val attributeQualities = attributes.getClassesInSignature() + Class(Vocab.HAS_NUMBER_OF)
  //  val eqCharacters = manager.createOntology(EQCharactersGenerator.generateEQCharacters(anatomicalEntities, attributeQualities))

  step("Creating VTO instances")
  val vtoIndividuals = TaxonomyConverter.createInstanceOntology(vto)
  write(vtoIndividuals, cwd + "/staging/kb/vto-individuals.owl")
  //step("Materializing VTO closure")
  //val materializedVTOClasses = MaterializeSubClassOfClosure.materialize(vto)
  //val materializedVTOIndividuals = TaxonomyConverter.createInstanceOntology(materializedVTOClasses)
  //step("Writing VTO closure")
  //write(materializedVTOIndividuals, cwd + "/staging/kb/vto-individuals-closure.owl")

  step("Converting NeXML to OWL")
  val vocabForNeXML = combine(uberon, pato, bspo, go, ro, phenoscapeVocab)
  cd(NEXML)
  val filesToConvert = (FileUtils.listFiles(new File(cwd + "/staging/nexml/completed-phenex-files"), Array("xml"), true) ++
    FileUtils.listFiles(new File(cwd + "/staging/nexml/fin_limb-incomplete-files"), Array("xml"), true)).filterNot(_.getName() == "catalog-v001.xml")
  cd(KB)
  val nexmlTBoxAxioms: mutable.Set[OWLAxiom] = mutable.Set()
  for (file <- filesToConvert) {
    val nexOntology = PropertyNormalizer.normalize(PhenexToOWL.convert(file, vocabForNeXML))
    nexmlTBoxAxioms.addAll(nexOntology.getTBoxAxioms(false))
    write(nexOntology, cwd + "/staging/kb/" + file.getName().replaceAll(".xml$", ".owl"))
  }

  step("Converting ZFIN data")
  val zfinGenes = PropertyNormalizer.normalize(ZFINGeneticMarkersToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/zfin_genetic_markers.txt"), "ISO-8859-1")))
  write(zfinGenes, cwd + "/staging/kb/zfin-genes.owl")
  val zfinPreviousGeneNames = PropertyNormalizer.normalize(ZFINPreviousGeneNamesToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/zfin_aliases.txt"), "ISO-8859-1")))
  write(zfinPreviousGeneNames, cwd + "/staging/kb/zfin-previous-gene-names.owl")
  val zfinExpressionData = PropertyNormalizer.normalize(ZFINExpressionToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/zfin_wildtype_expression.txt"), "ISO-8859-1")))
  write(zfinExpressionData, cwd + "/staging/kb/zfin-expression-data.owl")
  val zfinPhenotypeData = PropertyNormalizer.normalize(ZFINPhenotypesToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/zfin_phenotypes.txt"), "ISO-8859-1")))
  write(zfinPhenotypeData, cwd + "/staging/kb/zfin-phenotype-data.owl")

  step("Converting MGI data")
  val mgiGenes = PropertyNormalizer.normalize(MGIGeneticMarkersToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/mgi_genes.txt"), "utf-8")))
  write(mgiGenes, cwd + "/staging/kb/mgi-genes.owl")
  val mgiExpressionData = PropertyNormalizer.normalize(MGIExpressionToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/mgi_expression_data.txt"), "utf-8")))
  write(mgiExpressionData, cwd + "/staging/kb/mgi-expression-data.owl")
  val mgiPhenotypeData = PropertyNormalizer.normalize(MGIPhenotypesToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/mgi_phenotypes.txt"), "utf-8")))
  write(mgiPhenotypeData, cwd + "/staging/kb/mgi-phenotype-data.owl")

  step("Converting Xenbase data")
  val xenbaseGenes = PropertyNormalizer.normalize(XenbaseGenesToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/xenbase_genes.txt"), "utf-8")))
  write(xenbaseGenes, cwd + "/staging/kb/xenbase-genes.owl")
  val xenbaseExpressionData = PropertyNormalizer.normalize(XenbaseExpressionToOWL.convert(
    Source.fromFile(new File(cwd + "/staging/sources/xenbase_genepage_mappings.txt"), "utf-8"),
    Source.fromFile(new File(cwd + "/staging/sources/GeneExpression_laevis.txt"), "utf-8"),
    Source.fromFile(new File(cwd + "/staging/sources/GeneExpression_tropicalis.txt"), "utf-8")))
  write(xenbaseExpressionData, cwd + "/staging/kb/xenbase-expression-data.owl")
  val xenbasePhenotypeFiles = FileUtils.listFiles(new File(cwd + "/staging/sources/xenbase-phenotypes"), Array("txt"), true)
  val xenbasePhenotypeData = PropertyNormalizer.normalize(manager.createOntology(xenbasePhenotypeFiles.flatMap(f =>
    XenbasePhenotypesToOWL.convertToAxioms(Source.fromFile(f))).toSet))
  write(xenbasePhenotypeData, cwd + "/staging/kb/xenbase-phenotype-data.owl")

  step("Converting human data")
  val humanPhenotypeData = PropertyNormalizer.normalize(HumanPhenotypesToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/hp_phenotypes.txt"), "utf-8")))
  write(humanPhenotypeData, cwd + "/staging/kb/human-phenotypes.owl")

  step("Generating tbox")
  val tboxFromData = manager.createOntology(
    zfinGenes.getTBoxAxioms(false) ++
      zfinPreviousGeneNames.getTBoxAxioms(false) ++
      zfinExpressionData.getTBoxAxioms(false) ++
      zfinPhenotypeData.getTBoxAxioms(false) ++
      mgiGenes.getTBoxAxioms(false) ++
      mgiExpressionData.getTBoxAxioms(false) ++
      mgiPhenotypeData.getTBoxAxioms(false) ++
      xenbaseGenes.getTBoxAxioms(false) ++
      xenbaseExpressionData.getTBoxAxioms(false) ++
      xenbasePhenotypeData.getTBoxAxioms(false) ++
      humanPhenotypeData.getTBoxAxioms(false) ++
      nexmlTBoxAxioms)

  //val parts = manager.createOntology(anatomicalEntities.map(NamedRestrictionGenerator.createRestriction(ObjectProperty(Vocab.PART_OF), _)).flatten)
  val hasParts = manager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(has_part, _)))
  val presences = manager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(Vocab.IMPLIES_PRESENCE_OF, _)))
  val inherers = manager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(Vocab.inheres_in, _)))
  val inherersInPartOf = manager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(Vocab.inheres_in_part_of, _)))
  val towards = manager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(Vocab.towards, _)))
  //val involvers = manager.createOntology((anatomicalEntities ++ qualities).map(NamedRestrictionGenerator.createRestriction(ObjectProperty(Vocab.INVOLVES), _)).flatten)
  //val homologies = manager.createOntology(anatomicalEntities.map(NamedRestrictionGenerator.createRestriction(ObjectProperty(Vocab.PHP), _)).flatten)
  val absences = manager.createOntology(anatomicalEntities.flatMap(AbsenceClassGenerator.createAbsenceClass))
  val namedHasPartClasses = anatomicalEntities.map(_.getIRI()).map(NamedRestrictionGenerator.getRestrictionIRI(has_part.getIRI, _)).map(Class(_))
  val absenceNegationEquivalences = manager.createOntology(namedHasPartClasses.flatMap(NegationClassGenerator.createNegationClassAxioms(_, hasParts)))
  val developsFromRulesForAbsence = manager.createOntology(anatomicalEntities.flatMap(ReverseDevelopsFromRuleGenerator.createRules).toSet[OWLAxiom])

  val allTBox = combine(uberon, homology, pato, bspo, go, vto, zfa, xao, hp, //mp,
    hpEQ, mpEQ, zfaToUberon, xaoToUberon, fmaToUberon, mgiToEMAPA, emapa, emapaToUberon,
    hasParts, inherers, inherersInPartOf, towards, presences, absences, absenceNegationEquivalences, developsFromRulesForAbsence, tboxFromData, ro, phenoscapeVocab) // , eqCharacters
  println("tbox class count: " + allTBox.getClassesInSignature().size())
  println("tbox logical axiom count: " + allTBox.getLogicalAxiomCount())
  val tBoxWithoutDisjoints = OntologyUtil.ontologyWithoutDisjointAxioms(allTBox)
  //write(allTBox, cwd + "/staging/kb/asserted_tbox.owl")
  //write(tBoxWithoutDisjoints, cwd + "/staging/kb/asserted_tbox_no_disjoints.owl")

  step("Materializing tbox classification")
  val tboxReasoner = reasoner(tBoxWithoutDisjoints)
  val inferredAxioms = manager.createOntology()
  MaterializeInferences.materializeInferences(inferredAxioms, tboxReasoner)
  tboxReasoner.dispose()

  step("Asserting reverse negation hierarchy")
  val hierarchyAxioms = NegationHierarchyAsserter.assertNegationHierarchy(tBoxWithoutDisjoints, inferredAxioms)
  manager.addAxioms(inferredAxioms, hierarchyAxioms)
  val negationReasoner = reasoner(tBoxWithoutDisjoints, inferredAxioms)
  MaterializeInferences.materializeInferences(inferredAxioms, negationReasoner)

  if (negationReasoner.getUnsatisfiableClasses().getEntitiesMinusBottom().isEmpty()) {
    println("SUCCESS: all classes are satisfiable.")
  } else {
    println("WARNING: some classes are unsatisfiable.")
    println(negationReasoner.getUnsatisfiableClasses())
  }
  negationReasoner.dispose()

  step("Writing generated and inferred tbox axioms")
  write(combine(hasParts, inherers, inherersInPartOf, towards, presences, absences, absenceNegationEquivalences, developsFromRulesForAbsence, inferredAxioms), cwd + "/staging/kb/generated.owl") //, eqCharacters

  step("Writing tbox axioms for ELK")
  write(combine(tBoxWithoutDisjoints, inferredAxioms), cwd + "/staging/kb/tbox.owl")
  //step("Materializing subclass closure")
  //MaterializeSubClassOfClosureToNTriples.writeClosureToFile(tboxReasoner, new File(cwd + "/staging/kb/hierarchy_closure.nt"))
  tboxReasoner.dispose()

  step("Loading Bigdata")
  val bigdataProperties = new Properties()
  bigdataProperties.load(new FileReader(BIGDATA_PROPERTIES))
  bigdataProperties.setProperty(Options.FILE, BIGDATA_JOURNAL.getAbsolutePath)
  val sail = new BigdataSail(bigdataProperties)
  val repository = new BigdataSailRepository(sail)
  repository.initialize()
  val connection = repository.getUnisolatedConnection()
  connection.setAutoCommit(false);

  val baseURI = ""
  val graphURI = new URIImpl("http://kb.phenoscape.org/")
  for (rdfFile <- FileUtils.listFiles(KB, Array("owl"), true)) {
    connection.add(rdfFile, baseURI, RDFFormat.RDFXML, graphURI)
  }

  connection.commit()
  // close the repository connection
  connection.close()

  step("Computing absence assertions")
  val absenceConnection = repository.getUnisolatedConnection()
  absenceConnection.setAutoCommit(false);
  val absencesQuery = absenceConnection.prepareUpdate(QueryLanguage.SPARQL, """
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ps: <http://purl.org/phenoscape/vocab.owl#>
PREFIX Entity: <http://purl.obolibrary.org/obo/UBERON_0001062>
PREFIX Taxon: <http://purl.obolibrary.org/obo/VTO_0000001>

WITH <http://kb.phenoscape.org/>
INSERT {
    ?taxon ps:hasAbsenceOf ?entity .
}
WHERE {
    ?taxon ps:exhibits_state/ps:describes_phenotype/rdfs:subClassOf*/ps:absence_of ?entity .
    ?entity rdfs:subClassOf* Entity: .
    ?entity rdfs:isDefinedBy <http://purl.obolibrary.org/obo/uberon.owl> .
    ?taxon rdfs:subClassOf* Taxon: .
}
""")
  absenceConnection.commit()
  absenceConnection.close()

  step("Computing presence assertions")
  val presenceConnection = repository.getUnisolatedConnection()
  presenceConnection.setAutoCommit(false);
  val presencesQuery = presenceConnection.prepareUpdate(QueryLanguage.SPARQL, """
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ps: <http://purl.org/phenoscape/vocab.owl#>
PREFIX Entity: <http://purl.obolibrary.org/obo/UBERON_0001062>
PREFIX Taxon: <http://purl.obolibrary.org/obo/VTO_0000001>

WITH <http://kb.phenoscape.org/>
INSERT {
    ?taxon ps:hasPresenceOf ?entity .
}
WHERE {
    ?taxon ps:exhibits_state/ps:describes_phenotype/rdfs:subClassOf*/ps:implies_presence_of_some ?entity .
    ?entity rdfs:subClassOf* Entity: .
    ?entity rdfs:isDefinedBy <http://purl.obolibrary.org/obo/uberon.owl> .
    ?taxon rdfs:subClassOf* Taxon: .
}
""")
  presenceConnection.commit()
  presenceConnection.close()

  step("Exporting all triples to turtle file")

  val queryConnection = repository.getReadOnlyConnection
  queryConnection.setAutoCommit(false)
  val triplesQuery = queryConnection.prepareGraphQuery(QueryLanguage.SPARQL, """
CONSTRUCT {
 ?s ?p ?o .
}
FROM <http://kb.phenoscape.org/>
WHERE {
 ?s ?p ?o .
}
""")
  val triplesOutput = new BufferedOutputStream(new FileOutputStream(new File(cwd + "/staging/kb/kb.ttl")))
  triplesQuery.evaluate(new TurtleWriter(triplesOutput))
  triplesOutput.close()
  queryConnection.commit()
  queryConnection.close()

  step("Done")

}