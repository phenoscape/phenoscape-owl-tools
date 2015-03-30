package org.phenoscape.owl.build

import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.FileReader
import java.util.Properties

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.io.Source
import scala.language.postfixOps

import org.apache.commons.io.FileUtils
import org.apache.log4j.BasicConfigurator
import org.apache.log4j.Level
import org.apache.log4j.Logger
import org.openrdf.model.impl.URIImpl
import org.openrdf.query.QueryLanguage
import org.openrdf.rio.RDFFormat
import org.openrdf.rio.turtle.TurtleWriter
import org.phenoscape.owl.AbsenceClassGenerator
import org.phenoscape.owl.EvolutionaryProfiles
import org.phenoscape.owl.GeneProfiles
import org.phenoscape.owl.KnowledgeBaseBuilder
import org.phenoscape.owl.MaterializeInferences
import org.phenoscape.owl.NamedRestrictionGenerator
import org.phenoscape.owl.NegationClassGenerator
import org.phenoscape.owl.NegationHierarchyAsserter
import org.phenoscape.owl.PhenexToOWL
import org.phenoscape.owl.PropertyNormalizer
import org.phenoscape.owl.ReverseDevelopsFromRuleGenerator
import org.phenoscape.owl.SimilarityTemplates
import org.phenoscape.owl.TaxonNode
import org.phenoscape.owl.TaxonomyConverter
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.mod.human.HumanPhenotypesToOWL
import org.phenoscape.owl.mod.mgi.MGIExpressionToOWL
import org.phenoscape.owl.mod.mgi.MGIGeneticMarkersToOWL
import org.phenoscape.owl.mod.mgi.MGIPhenotypesToOWL
import org.phenoscape.owl.mod.xenbase.XenbaseExpressionToOWL
import org.phenoscape.owl.mod.xenbase.XenbaseGenesToOWL
import org.phenoscape.owl.mod.xenbase.XenbasePhenotypesToOWL
import org.phenoscape.owl.mod.zfin.ZFINExpressionToOWL
import org.phenoscape.owl.mod.zfin.ZFINGeneticMarkersToOWL
import org.phenoscape.owl.mod.zfin.ZFINPhenotypesToOWL
import org.phenoscape.owl.mod.zfin.ZFINPreviousGeneNamesToOWL
import org.phenoscape.owl.util.OntologyUtil
import org.phenoscape.owlet.SPARQLComposer._
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

import com.bigdata.journal.Options
import com.bigdata.rdf.sail.BigdataSail
import com.bigdata.rdf.sail.BigdataSailRepository

object PhenoscapeKB extends KnowledgeBaseBuilder {

  BasicConfigurator.configure()
  Logger.getRootLogger().setLevel(Level.ERROR)

  val manager = getManager
  val rdfsSubClassOf = ObjectProperty(OWLRDFVocabulary.RDFS_SUBCLASS_OF.getIRI)
  val implies_presence_of_some = NamedRestrictionGenerator.getClassRelationIRI(Vocab.IMPLIES_PRESENCE_OF.getIRI)

  val cwd = System.getProperty("user.dir")
  val STAGING = new File("staging")
  val KB = new File("staging/kb")
  val NEXML = new File("staging/nexml")
  val BIGDATA_PROPERTIES = new File("bigdata.properties")
  val BIGDATA_JOURNAL = new File("staging/bigdata.jnl")
  STAGING.mkdir()
  KB.mkdir()
  cd(KB)

  def loadOntologiesAndCreateReasoner(): OWLReasoner = {
    step("Loading ontologies")
    val roAnnotations = loadNormalized(new File(cwd + "/staging/sources/ro-annotations.owl"))
    val bfoMinimal = loadNormalized(new File(cwd + "/staging/sources/bfo-classes-minimal.owl"))
    val roCore = loadNormalized(new File(cwd + "/staging/sources/ro-core.owl"))
    val temporalIntervals = loadNormalized(new File(cwd + "/staging/sources/temporal-intervals.owl"))
    val roRelease = loadNormalized(new File(cwd + "/staging/sources/ro.owl"))
    val ro = combine(roRelease, temporalIntervals, roCore, bfoMinimal, roAnnotations)
    write(ro, cwd + "/staging/kb/ro.owl")
    val phenoscapeVocab = loadNormalized(new File(cwd + "/staging/sources/phenoscape-vocab.owl"))
    write(phenoscapeVocab, cwd + "/staging/kb/phenoscape-vocab.owl")
    val attributes = loadNormalized(new File(cwd + "/staging/sources/character_slims.obo"))
    write(attributes, cwd + "/staging/kb/attributes.owl")
    val uberonReferences = loadNormalized(new File(cwd + "/staging/sources/references.owl"))
    val uberonChebi = loadNormalized(new File(cwd + "/staging/sources/chebi_import.owl"))
    val uberonCL = loadNormalized(new File(cwd + "/staging/sources/cl_import.owl"))
    val uberonGO = loadNormalized(new File(cwd + "/staging/sources/go_import.owl"))
    val uberonTaxon = loadNormalized(new File(cwd + "/staging/sources/ncbitaxon_import.owl"))
    val uberonPATO = loadNormalized(new File(cwd + "/staging/sources/pato_import.owl"))
    val uberonPR = loadNormalized(new File(cwd + "/staging/sources/pr_import.owl"))
    val uberonENVO = loadNormalized(new File(cwd + "/staging/sources/envo_import.owl"))
    val uberonNBO = loadNormalized(new File(cwd + "/staging/sources/nbo_import.owl"))
    val uberonRO = loadNormalized(new File(cwd + "/staging/sources/ro_import.owl"))
    val ext = loadNormalized(new File(cwd + "/staging/sources/ext.owl"))
    val uberon = combine(ext, uberonReferences, uberonChebi, uberonGO, uberonTaxon, uberonPATO, uberonPR, uberonENVO, uberonNBO, uberonRO)
    write(uberon, cwd + "/staging/kb/uberon.owl")
    val homology = loadNormalized(new File(cwd + "/staging/sources/homology.owl"))
    write(homology, cwd + "/staging/kb/homology.owl")
    val pato = loadNormalized(new File(cwd + "/staging/sources/pato.owl"))
    write(pato, cwd + "/staging/kb/pato.owl")
    val bspo = loadNormalized(new File(cwd + "/staging/sources/bspo.owl"))
    write(bspo, cwd + "/staging/kb/bspo.owl")
    val go = loadNormalized(new File(cwd + "/staging/sources/go.owl"))
    write(go, cwd + "/staging/kb/go.owl")
    val taxrank = loadNormalized(new File(cwd + "/staging/sources/taxrank.owl"))
    write(taxrank, cwd + "/staging/kb/taxrank.owl")
    val vto = loadNormalized(new File(cwd + "/staging/sources/vto.owl"))
    write(vto, cwd + "/staging/kb/vto.owl")
    val collections = OWLManager.createOWLOntologyManager.loadOntologyFromOntologyDocument(new File(cwd + "/staging/sources/fish_collection_abbreviation.obo"))
    write(collections, cwd + "/staging/kb/fish_collection_abbreviation.owl")
    val zfa = loadNormalized(new File(cwd + "/staging/sources/zfa.owl"))
    write(zfa, cwd + "/staging/kb/zfa.owl")
    val xao = loadNormalized(new File(cwd + "/staging/sources/xao.owl"))
    write(xao, cwd + "/staging/kb/xao.owl")
    val hp = loadNormalized(new File(cwd + "/staging/sources/hp.owl"))
    write(hp, cwd + "/staging/kb/hp.owl")
    val mp = loadNormalized(new File(cwd + "/staging/sources/mp.owl"))
    write(mp, cwd + "/staging/kb/mp.owl")

    val hpEQ = loadNormalized(new File(cwd + "/staging/sources/hp-equivalence-axioms-subq-ubr.owl"))
    write(hpEQ, cwd + "/staging/kb/hp-logical-definitions.owl")
    val mpEQ = loadNormalized(new File(cwd + "/staging/sources/mp-equivalence-axioms-subq-ubr.owl"))
    write(mpEQ, cwd + "/staging/kb/mp-logical-definitions.owl")

    val caroToUberon = loadNormalized(new File(cwd + "/staging/sources/uberon-bridge-to-caro.owl"))
    write(caroToUberon, cwd + "/staging/kb/uberon-bridge-to-caro.owl")
    val zfaToUberon = loadNormalized(new File(cwd + "/staging/sources/uberon-ext-bridge-to-zfa.owl"))
    write(zfaToUberon, cwd + "/staging/kb/uberon-ext-bridge-to-zfa.owl")
    val xaoToUberon = loadNormalized(new File(cwd + "/staging/sources/uberon-bridge-to-xao.owl"))
    write(xaoToUberon, cwd + "/staging/kb/uberon-bridge-to-xao.owl")
    val fmaToUberon = loadNormalized(new File(cwd + "/staging/sources/uberon-bridge-to-fma.owl"))
    write(fmaToUberon, cwd + "/staging/kb/uberon-bridge-to-fma.owl")
    val mgiToEMAPA = loadNormalized(new File(cwd + "/staging/sources/mgi_anatomy.owl"))
    write(mgiToEMAPA, cwd + "/staging/kb/mgi_anatomy.owl")
    val emapa = loadNormalized(new File(cwd + "/staging/sources/emapa.owl"))
    write(emapa, cwd + "/staging/kb/emapa.owl")
    val emapaToUberon = loadNormalized(new File(cwd + "/staging/sources/uberon-bridge-to-emapa.owl"))
    write(mgiToEMAPA, cwd + "/staging/kb/uberon-bridge-to-emapa.owl.owl")

    step("Querying entities and qualities")
    val coreReasoner = reasoner(uberon, pato, bspo, go, ro, phenoscapeVocab)
    val anatomicalEntities = coreReasoner.getSubClasses(Class(Vocab.ANATOMICAL_ENTITY), false).getFlattened.filterNot(_.isOWLNothing)
    //val qualities = coreReasoner.getSubClasses(Class(Vocab.QUALITY), false).getFlattened().filterNot(_.isOWLNothing())
    coreReasoner.dispose()

    //  step("Generating EQ characters for analyses")
    val attributeQualities = attributes.getClassesInSignature + HasNumberOf
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
      FileUtils.listFiles(new File(cwd + "/staging/nexml/fin_limb-incomplete-files"), Array("xml"), true) ++
      FileUtils.listFiles(new File(cwd + "/staging/nexml/matrix-vs-monograph"), Array("xml"), true)).filterNot(_.getName == "catalog-v001.xml")
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
        nexmlTBoxAxioms +
        (has_part_inhering_in SubPropertyChain (has_part o inheres_in))) //TODO add this to Phenoscape vocab ontology

    //val parts = manager.createOntology(anatomicalEntities.map(NamedRestrictionGenerator.createRestriction(ObjectProperty(Vocab.PART_OF), _)).flatten)
    val hasParts = manager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(has_part, _)))
    val presences = manager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(Vocab.IMPLIES_PRESENCE_OF, _)))
    val hasPartsInheringIns = manager.createOntology(anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(has_part_inhering_in, _)))
    val absences = manager.createOntology(anatomicalEntities.flatMap(AbsenceClassGenerator.createAbsenceClass))
    val namedHasPartClasses = anatomicalEntities.map(_.getIRI()).map(NamedRestrictionGenerator.getRestrictionIRI(has_part.getIRI, _)).map(Class(_))
    val absenceNegationEquivalences = manager.createOntology(namedHasPartClasses.flatMap(NegationClassGenerator.createNegationClassAxioms(_, hasParts)))
    val developsFromRulesForAbsence = manager.createOntology(anatomicalEntities.flatMap(ReverseDevelopsFromRuleGenerator.createRules).toSet[OWLAxiom])

    step("Generating semantic similarity subsumers")
    val entitySubsumerAxioms = for {
      entity <- anatomicalEntities
      (term, entityAxioms) = SimilarityTemplates.entity(entity)
      (partsTerm, entityPartsAxioms) = SimilarityTemplates.entityAndParts(entity)
      axiom <- (entityAxioms ++ entityPartsAxioms)
    } yield axiom
    val entityQualitySubsumerAxioms = for {
      entity <- anatomicalEntities
      attribute <- attributeQualities
      (term, entityAxioms) = SimilarityTemplates.entityWithQuality(entity, attribute)
      (partsTerm, entityPartsAxioms) = SimilarityTemplates.entityAndPartsWithQuality(entity, attribute)
      axiom <- (entityAxioms ++ entityPartsAxioms)
    } yield axiom
    val subsumers = manager.createOntology(entitySubsumerAxioms ++ entityQualitySubsumerAxioms)

    val allTBox = combine(uberon, homology, pato, bspo, go, vto, zfa, xao, hp, //mp,
      hpEQ, mpEQ, caroToUberon, zfaToUberon, xaoToUberon, fmaToUberon, mgiToEMAPA, emapa, emapaToUberon,
      hasParts, hasPartsInheringIns, presences, absences, absenceNegationEquivalences, developsFromRulesForAbsence, subsumers, tboxFromData, ro, phenoscapeVocab) // , eqCharacters
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
    implicit val negationReasoner = reasoner(tBoxWithoutDisjoints, inferredAxioms)
    MaterializeInferences.materializeInferences(inferredAxioms, negationReasoner)

    if (negationReasoner.getUnsatisfiableClasses().getEntitiesMinusBottom().isEmpty()) {
      println("SUCCESS: all classes are satisfiable.")
    } else {
      println("WARNING: some classes are unsatisfiable.")
      println(negationReasoner.getUnsatisfiableClasses())
    }

    step("Writing generated and inferred tbox axioms")
    write(combine(hasParts, hasPartsInheringIns, presences, absences, absenceNegationEquivalences, developsFromRulesForAbsence, inferredAxioms), cwd + "/staging/kb/generated.owl") //, eqCharacters

    step("Writing tbox axioms for ELK")
    val tboxOut = combine(tBoxWithoutDisjoints, inferredAxioms)
    write(tboxOut, cwd + "/staging/kb/tbox.owl")

    step("Reducing tbox for OWLsim")
    val reducedTbox = OntologyUtil.reduceOntologyToHierarchy(tboxOut)
    write(reducedTbox, cwd + "/staging/kb/tbox-hierarchy-only.owl")

    negationReasoner

  }

  implicit val fullReasoner = loadOntologiesAndCreateReasoner()

  val presencesQuery = construct(t('taxon, Vocab.has_presence_of, 'entity)) from "http://kb.phenoscape.org/" where (
    bgp(
      t('taxon, Vocab.exhibits_state / Vocab.describes_phenotype / (rdfsSubClassOf*) / implies_presence_of_some, 'entity),
      t('entity, OWLRDFVocabulary.RDFS_IS_DEFINED_BY.getIRI, IRI.create("http://purl.obolibrary.org/obo/uberon.owl"))),
      subClassOf('taxon, Class(Vocab.CHORDATA)),
      subClassOf('entity, Class(Vocab.ANATOMICAL_ENTITY)))

  val absencesQuery = construct(t('taxon, Vocab.has_absence_of, 'entity)) from "http://kb.phenoscape.org/" where (
    bgp(
      t('taxon, Vocab.exhibits_state / Vocab.describes_phenotype / (rdfsSubClassOf*) / ABSENCE_OF, 'entity),
      t('entity, OWLRDFVocabulary.RDFS_IS_DEFINED_BY.getIRI, IRI.create("http://purl.obolibrary.org/obo/uberon.owl"))),
      subClassOf('taxon, Class(Vocab.CHORDATA)),
      subClassOf('entity, Class(Vocab.ANATOMICAL_ENTITY)))

  step("Loading Bigdata")
  val bigdataProperties = new Properties()
  bigdataProperties.load(new FileReader(BIGDATA_PROPERTIES))
  bigdataProperties.setProperty(Options.FILE, BIGDATA_JOURNAL.getAbsolutePath)
  val sail = new BigdataSail(bigdataProperties)
  val repository = new BigdataSailRepository(sail)
  repository.initialize()
  val bigdata = repository.getUnisolatedConnection()
  bigdata.setAutoCommit(false)

  val baseURI = ""
  val graphURI = new URIImpl("http://kb.phenoscape.org/")
  for (rdfFile <- FileUtils.listFiles(KB, Array("owl"), true)) {
    bigdata.add(rdfFile, baseURI, RDFFormat.RDFXML, graphURI)
  }

  bigdata.commit()

  val profileEntityFilterExpression = (part_of some AppendageGirdleComplex)
  val validAbsencePhenotypes = fullReasoner.getSubClasses(profileEntityFilterExpression, false).getFlattened.map(_.getIRI).map(AbsenceClassGenerator.getAbsenceIRI).map(Class(_))
  val profilePhenotypeFilter = (fullReasoner.getSubClasses(has_part some (phenotype_of some profileEntityFilterExpression), false).getFlattened).toSet ++ validAbsencePhenotypes

  step("Building evolutionary profiles using ancestral states reconstruction")
  bigdata.add(EvolutionaryProfiles.computePhenotypeProfiles(TaxonNode(CHORDATA), profilePhenotypeFilter, fullReasoner, bigdata), graphURI)
  bigdata.commit()

  step("Building gene profiles")
  bigdata.add(GeneProfiles.generateGeneProfiles(bigdata, profilePhenotypeFilter), graphURI)
  bigdata.commit()

  fullReasoner.dispose()
  System.gc()

  step("Exporting presence assertions")
  val presencesFile = new File(cwd + "/staging/kb/presences.ttl")
  val presencesOutput = new BufferedOutputStream(new FileOutputStream(presencesFile))
  bigdata.prepareGraphQuery(QueryLanguage.SPARQL, presencesQuery.toString).evaluate(new TurtleWriter(presencesOutput))
  presencesOutput.close()

  step("Exporting absence assertions")
  val absencesFile = new File(cwd + "/staging/kb/absences.ttl")
  val absencesOutput = new BufferedOutputStream(new FileOutputStream(absencesFile))
  bigdata.prepareGraphQuery(QueryLanguage.SPARQL, absencesQuery.toString).evaluate(new TurtleWriter(absencesOutput))
  absencesOutput.close()

  step("Load presence/absence data")
  bigdata.add(presencesFile, baseURI, RDFFormat.TURTLE, graphURI)
  bigdata.add(absencesFile, baseURI, RDFFormat.TURTLE, graphURI)
  bigdata.commit()

  step("Exporting all triples to turtle file")
  val triplesQuery = bigdata.prepareGraphQuery(QueryLanguage.SPARQL, """
  CONSTRUCT {
   ?s ?p ?o .
  }
  FROM <http://kb.phenoscape.org/>
  WHERE {
   ?s ?p ?o .
  }
  """)
  //bigdata.begin()
  val triplesOutput = new BufferedOutputStream(new FileOutputStream(new File(cwd + "/staging/kb/kb.ttl")))
  triplesQuery.evaluate(new TurtleWriter(triplesOutput))
  triplesOutput.close()

  step("Exporting phenotypic profiles for semantic similarity")
  val profilesQuery = bigdata.prepareGraphQuery(QueryLanguage.SPARQL, """
PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl:  <http://www.w3.org/2002/07/owl#>
PREFIX ps:   <http://purl.org/phenoscape/vocab.owl#>
CONSTRUCT {
 ?profile rdf:type ?phenotype
}
FROM <http://kb.phenoscape.org/>
WHERE {
  ?source ps:has_phenotypic_profile ?profile .
  ?profile rdf:type ?phenotype .
}
    """)
  val profilesOutput = new BufferedOutputStream(new FileOutputStream(new File(cwd + "/staging/kb/profiles.ttl")))
  profilesQuery.evaluate(new TurtleWriter(profilesOutput))
  profilesOutput.close()

  bigdata.close()

  step("Done")

}