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
import org.phenoscape.kb.ingest.human.HumanPhenotypesToOWL
import org.phenoscape.kb.ingest.mgi.MGIExpressionToOWL
import org.phenoscape.kb.ingest.mgi.MGIGeneticMarkersToOWL
import org.phenoscape.kb.ingest.mgi.MGIPhenotypesToOWL
import org.phenoscape.kb.ingest.xenbase.XenbaseExpressionToOWL
import org.phenoscape.kb.ingest.xenbase.XenbaseGenesToOWL
import org.phenoscape.kb.ingest.xenbase.XenbasePhenotypesToOWL
import org.phenoscape.kb.ingest.zfin.ZFINExpressionToOWL
import org.phenoscape.kb.ingest.zfin.ZFINGeneticMarkersToOWL
import org.phenoscape.kb.ingest.zfin.ZFINPhenotypesToOWL
import org.phenoscape.kb.ingest.zfin.ZFINPreviousGeneNamesToOWL
import org.phenoscape.owl.util.OntologyUtil
import org.phenoscape.owlet.SPARQLComposer._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary
import com.bigdata.journal.Options
import com.bigdata.rdf.sail.BigdataSail
import com.bigdata.rdf.sail.BigdataSailRepository
import org.semanticweb.owlapi.model.AxiomType
import org.phenoscape.owl.SourcedAxioms
import org.semanticweb.owlapi.io.RDFXMLOntologyFormat
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream

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

  step("Loading Bigdata")
  val bigdataProperties = new Properties()
  bigdataProperties.load(new FileReader(BIGDATA_PROPERTIES))
  bigdataProperties.setProperty(Options.FILE, BIGDATA_JOURNAL.getAbsolutePath)
  val sail = new BigdataSail(bigdataProperties)
  val repository = new BigdataSailRepository(sail)
  repository.initialize()
  val bigdata = repository.getUnisolatedConnection()

  val baseURI = ""
  val graphURI = new URIImpl("http://kb.phenoscape.org/")

  def loadOntologiesAndCreateReasoner(): OWLReasoner = {
    bigdata.begin()
    step("Loading ontologies")
    val phenoscapeVocab = loadFromWebWithImports(IRI.create("http://purl.org/phenoscape/vocab.owl"))
    addTriples(phenoscapeVocab, bigdata, graphURI)
    val attributes = loadFromWebWithImports(IRI.create("http://svn.code.sf.net/p/phenoscape/code/trunk/vocab/character_slims.obo"))
    addTriples(attributes, bigdata, graphURI)
    val uberon = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/uberon/ext.owl"))
    addTriples(uberon, bigdata, graphURI)
    val homology = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/uberon/homology.owl"))
    addTriples(homology, bigdata, graphURI)
    val pato = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/pato.owl"))
    addTriples(pato, bigdata, graphURI)
    val bspo = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/bspo.owl"))
    addTriples(bspo, bigdata, graphURI)
    val go = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/go.owl"))
    addTriples(go, bigdata, graphURI)
    val taxrank = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/taxrank.owl"))
    addTriples(taxrank, bigdata, graphURI)
    val vto = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/vto.owl"))
    addTriples(vto, bigdata, graphURI)
    val collections = loadFromWebWithImports(IRI.create("http://svn.code.sf.net/p/phenoscape/code/trunk/vocab/fish_collection_abbreviation.obo"))
    addTriples(collections, bigdata, graphURI)
    val zfa = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/zfa.owl"))
    addTriples(zfa, bigdata, graphURI)
    val xao = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/xao.owl"))
    addTriples(xao, bigdata, graphURI)
    val hp = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/hp.owl"))
    addTriples(hp, bigdata, graphURI)
    val mp = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/mp.owl"))
    addTriples(mp, bigdata, graphURI)

    val caroToUberon = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-caro.owl"))
    addTriples(caroToUberon, bigdata, graphURI)
    val zfaToUberon = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/uberon/bridge/uberon-ext-bridge-to-zfa.owl"))
    addTriples(zfaToUberon, bigdata, graphURI)
    val xaoToUberon = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-xao.owl"))
    addTriples(xaoToUberon, bigdata, graphURI)
    val fmaToUberon = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-fma.owl"))
    addTriples(fmaToUberon, bigdata, graphURI)
    val mgiToEMAPA = SourcedAxioms(loadNormalized(new File(cwd + "/staging/sources/mgi_anatomy.owl")))
    addTriples(mgiToEMAPA, bigdata, graphURI)
    val emapa = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/emapa.owl"))
    addTriples(emapa, bigdata, graphURI)
    val emapaToUberon = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-emapa.owl"))
    addTriples(emapaToUberon, bigdata, graphURI)

    step("Querying entities and qualities")
    val coreReasoner = reasoner(Set(uberon, pato, bspo, go, phenoscapeVocab).flatMap(_.axioms))
    val anatomicalEntities = coreReasoner.getSubClasses(Class(Vocab.ANATOMICAL_ENTITY), false).getFlattened.filterNot(_.isOWLNothing) + Class(Vocab.ANATOMICAL_ENTITY)
    val qualities = coreReasoner.getSubClasses(Class(Vocab.QUALITY), false).getFlattened.filterNot(_.isOWLNothing) + Class(Vocab.QUALITY)
    coreReasoner.dispose()

    step("Converting NeXML to OWL")
    val vocabForNeXML = combine(uberon, pato, bspo, go, phenoscapeVocab)
    cd(NEXML)
    val filesToConvert = (FileUtils.listFiles(new File(cwd + "/staging/nexml/completed-phenex-files"), Array("xml"), true) ++
      FileUtils.listFiles(new File(cwd + "/staging/nexml/fin_limb-incomplete-files"), Array("xml"), true) ++
      FileUtils.listFiles(new File(cwd + "/staging/nexml/Jackson Dissertation Files"), Array("xml"), true) ++
      FileUtils.listFiles(new File(cwd + "/staging/nexml/matrix-vs-monograph"), Array("xml"), true)).filterNot(_.getName == "catalog-v001.xml")
    cd(KB)
    val nexmlTBoxAxioms: mutable.Set[OWLAxiom] = mutable.Set()
    for (file <- filesToConvert) {
      val nexOntology = PropertyNormalizer.normalize(PhenexToOWL.convert(file, vocabForNeXML))
      nexmlTBoxAxioms.addAll(nexOntology.getTBoxAxioms(false))
      addTriples(nexOntology, bigdata, graphURI)
    }

    step("Converting ZFIN data")
    val zfinGenes = PropertyNormalizer.normalize(ZFINGeneticMarkersToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/zfin_genetic_markers.txt"), "ISO-8859-1")))
    addTriples(zfinGenes, bigdata, graphURI)
    val zfinPreviousGeneNames = PropertyNormalizer.normalize(ZFINPreviousGeneNamesToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/zfin_aliases.txt"), "ISO-8859-1")))
    addTriples(zfinPreviousGeneNames, bigdata, graphURI)
    val zfinExpressionData = PropertyNormalizer.normalize(ZFINExpressionToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/zfin_wildtype_expression.txt"), "ISO-8859-1")))
    addTriples(zfinExpressionData, bigdata, graphURI)
    val zfinPhenotypeData = PropertyNormalizer.normalize(ZFINPhenotypesToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/zfin_phenotypes.txt"), "ISO-8859-1")))
    addTriples(zfinPhenotypeData, bigdata, graphURI)

    step("Converting MGI data")
    val mgiGenes = PropertyNormalizer.normalize(MGIGeneticMarkersToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/mgi_genes.txt"), "utf-8")))
    addTriples(mgiGenes, bigdata, graphURI)
    val mgiExpressionData = PropertyNormalizer.normalize(MGIExpressionToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/mgi_expression_data.txt"), "utf-8")))
    addTriples(mgiExpressionData, bigdata, graphURI)
    val mgiPhenotypeData = PropertyNormalizer.normalize(MGIPhenotypesToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/mgi_phenotypes.txt"), "utf-8")))
    addTriples(mgiPhenotypeData, bigdata, graphURI)

    step("Converting Xenbase data")
    val xenbaseGenes = PropertyNormalizer.normalize(XenbaseGenesToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/xenbase_genes.txt"), "utf-8")))
    addTriples(xenbaseGenes, bigdata, graphURI)
    val xenbaseExpressionData = PropertyNormalizer.normalize(XenbaseExpressionToOWL.convert(
      Source.fromFile(new File(cwd + "/staging/sources/xenbase_genepage_mappings.txt"), "utf-8"),
      Source.fromFile(new File(cwd + "/staging/sources/GeneExpression_laevis.txt"), "utf-8"),
      Source.fromFile(new File(cwd + "/staging/sources/GeneExpression_tropicalis.txt"), "utf-8")))
    addTriples(xenbaseExpressionData, bigdata, graphURI)
    val xenbasePhenotypeFiles = FileUtils.listFiles(new File(cwd + "/staging/sources/xenbase-phenotypes"), Array("txt"), true)
    val xenbasePhenotypeData = PropertyNormalizer.normalize(xenbasePhenotypeFiles.flatMap(f => XenbasePhenotypesToOWL.convertToAxioms(Source.fromFile(f))).toSet)
    addTriples(xenbasePhenotypeData, bigdata, graphURI)

    step("Converting human data")
    val humanPhenotypeData = PropertyNormalizer.normalize(HumanPhenotypesToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/hp_phenotypes.txt"), "utf-8")))
    addTriples(humanPhenotypeData, bigdata, graphURI)

    step("Generating tbox")
    val tboxFromData =
      zfinGenes.filter(isTboxAxiom) ++
        zfinPreviousGeneNames.filter(isTboxAxiom) ++
        zfinExpressionData.filter(isTboxAxiom) ++
        zfinPhenotypeData.filter(isTboxAxiom) ++
        mgiGenes.filter(isTboxAxiom) ++
        mgiExpressionData.filter(isTboxAxiom) ++
        mgiPhenotypeData.filter(isTboxAxiom) ++
        xenbaseGenes.filter(isTboxAxiom) ++
        xenbaseExpressionData.filter(isTboxAxiom) ++
        xenbasePhenotypeData.filter(isTboxAxiom) ++
        humanPhenotypeData.filter(isTboxAxiom) ++
        nexmlTBoxAxioms

    val parts = anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(part_of, _))
    addTriples(parts, bigdata, graphURI)
    val hasParts = (anatomicalEntities ++ qualities).flatMap(NamedRestrictionGenerator.createRestriction(has_part, _))
    addTriples(hasParts, bigdata, graphURI)
    val presences = anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(Vocab.IMPLIES_PRESENCE_OF, _))
    addTriples(presences, bigdata, graphURI)
    val hasPartsInheringIns = anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(has_part_inhering_in, _))
    addTriples(hasPartsInheringIns, bigdata, graphURI)
    val phenotypeOfs = anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(phenotype_of, _))
    val absences = anatomicalEntities.flatMap(AbsenceClassGenerator.createAbsenceClass)
    addTriples(absences, bigdata, graphURI)
    val namedHasPartClasses = anatomicalEntities.map(_.getIRI).map(NamedRestrictionGenerator.getRestrictionIRI(has_part.getIRI, _)).map(Class(_))
    val absenceNegationEquivalences = namedHasPartClasses.flatMap(NegationClassGenerator.createNegationClassAxioms)
    addTriples(absenceNegationEquivalences, bigdata, graphURI)
    val developsFromRulesForAbsence = anatomicalEntities.flatMap(ReverseDevelopsFromRuleGenerator.createRules).toSet[OWLAxiom]
    addTriples(developsFromRulesForAbsence, bigdata, graphURI)

    step("Generating additional semantic similarity subsumers")
    val attributeQualities = attributes.axioms.flatMap(_.getClassesInSignature) + HasNumberOf
    val subsumers = for {
      entity <- anatomicalEntities
      (partsTerm, entityPartsAxioms) = SimilarityTemplates.partsOfEntity(entity)
      (developsFromTerm, developsFromAxioms) = SimilarityTemplates.developsFromEntity(entity)
      axiom <- (entityPartsAxioms ++ developsFromAxioms)
    } yield axiom
    addTriples(subsumers, bigdata, graphURI)

    val allTBox = uberon.axioms ++ homology.axioms ++ pato.axioms ++ bspo.axioms ++ go.axioms ++ vto.axioms ++ zfa.axioms ++ xao.axioms ++ hp.axioms ++ mp.axioms ++
      caroToUberon.axioms ++ zfaToUberon.axioms ++ xaoToUberon.axioms ++ fmaToUberon.axioms ++ mgiToEMAPA.axioms ++ emapa.axioms ++ emapaToUberon.axioms ++
      parts ++ hasParts ++ hasPartsInheringIns ++ phenotypeOfs ++ presences ++ absences ++ absenceNegationEquivalences ++ developsFromRulesForAbsence ++ subsumers ++ tboxFromData ++ phenoscapeVocab.axioms

    val coreTBox = uberon.axioms ++ homology.axioms ++ pato.axioms ++ bspo.axioms ++ go.axioms ++ vto.axioms ++ zfa.axioms ++ xao.axioms ++ hp.axioms ++ mp.axioms ++
      caroToUberon.axioms ++ zfaToUberon.axioms ++ xaoToUberon.axioms ++ fmaToUberon.axioms ++ mgiToEMAPA.axioms ++ emapa.axioms ++ emapaToUberon.axioms ++
      developsFromRulesForAbsence ++ tboxFromData ++ phenoscapeVocab.axioms
    println("tbox class count: " + allTBox.flatMap(_.getClassesInSignature).size)
    println("tbox logical axiom count: " + allTBox.filter(_.isLogicalAxiom).size)
    val tBoxWithoutDisjoints = OntologyUtil.filterDisjointAxioms(allTBox)
    val coreTBoxWithoutDisjoints = OntologyUtil.filterDisjointAxioms(coreTBox)

    step("Materializing tbox classification")
    val tboxReasoner = reasoner(tBoxWithoutDisjoints)
    val inferredAxioms = manager.createOntology()
    MaterializeInferences.materializeInferences(inferredAxioms, tboxReasoner)
    tboxReasoner.dispose()

    val coreTboxOntology = manager.createOntology(coreTBoxWithoutDisjoints)
    val coreTboxReasoner = reasoner(coreTboxOntology)
    MaterializeInferences.materializeInferences(coreTboxOntology, coreTboxReasoner)
    coreTboxReasoner.dispose()

    step("Asserting reverse negation hierarchy")
    val hierarchyAxioms = NegationHierarchyAsserter.assertNegationHierarchy(tBoxWithoutDisjoints ++ inferredAxioms.getAxioms)
    manager.addAxioms(inferredAxioms, hierarchyAxioms)
    implicit val negationReasoner = reasoner(tBoxWithoutDisjoints ++ inferredAxioms.getAxioms)
    MaterializeInferences.materializeInferences(inferredAxioms, negationReasoner)

    if (negationReasoner.getUnsatisfiableClasses().getEntitiesMinusBottom().isEmpty()) {
      println("SUCCESS: all classes are satisfiable.")
    } else {
      println("WARNING: some classes are unsatisfiable.")
      println(negationReasoner.getUnsatisfiableClasses())
    }

    //step("Writing inferred tbox axioms")
    //addTriples(inferredAxioms, bigdata, graphURI)

    step("Writing tbox axioms for ELK")
    val tboxOut = OWLManager.createOWLOntologyManager().createOntology((tBoxWithoutDisjoints ++ inferredAxioms.getAxioms))
    write(tboxOut, cwd + "/staging/kb/tbox.owl")
    bigdata.add(new File(cwd + "/staging/kb/tbox.owl"), "", RDFFormat.RDFXML, graphURI)

    write(coreTboxOntology, cwd + "/staging/kb/core-tbox.owl")

    step("Reducing tbox for OWLsim")
    val reducedTbox = OntologyUtil.reduceOntologyToHierarchy(tboxOut)
    write(reducedTbox, cwd + "/staging/kb/tbox-hierarchy-only.owl")

    bigdata.commit()

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

  step("Building evolutionary profiles using ancestral states reconstruction")
  bigdata.begin()
  bigdata.add(EvolutionaryProfiles.computePhenotypeProfiles(TaxonNode(CHORDATA), fullReasoner, bigdata), graphURI)
  bigdata.commit()

  step("Building gene profiles")
  bigdata.begin()
  bigdata.add(GeneProfiles.generateGeneProfiles(bigdata), graphURI)
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
  bigdata.begin()
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