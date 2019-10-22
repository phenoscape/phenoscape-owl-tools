package org.phenoscape.owl.build

import java.io.BufferedOutputStream
import java.io.{ File => JFile }
import java.io.FileOutputStream
import java.io.FileReader
import java.util.Properties

import scala.collection.JavaConverters._
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
import org.phenoscape.owl.SourcedAxioms
import org.phenoscape.owl.TaxonNode
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.Conversions.BetterFileOps
import org.phenoscape.owl.util.OntologyUtil
import org.phenoscape.owlet.SPARQLComposer._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

import com.bigdata.journal.Options
import com.bigdata.rdf.sail.BigdataSail
import com.bigdata.rdf.sail.BigdataSailRepository

import better.files._
import java.util.Date
import org.openrdf.model.vocabulary.DCTERMS

object PhenoscapeKB extends KnowledgeBaseBuilder {

  val targetDir = File(args(0))
  val BIGDATA_PROPERTIES = new JFile(args(1))

  BasicConfigurator.configure()
  Logger.getRootLogger().setLevel(Level.ERROR)

  val manager = getManager
  val rdfsSubClassOf = ObjectProperty(OWLRDFVocabulary.RDFS_SUBCLASS_OF.getIRI)
  val implies_presence_of_some = NamedRestrictionGenerator.getClassRelationIRI(Vocab.IMPLIES_PRESENCE_OF.getIRI)

  val SOURCES = targetDir / "sources"
  val NEXML = SOURCES / "nexml"
  val KB = targetDir / "kb"
  val BIGDATA_JOURNAL = targetDir / "blazegraph.jnl"
  KB.createIfNotExists(true, true)

  step("Loading Bigdata")
  val bigdataProperties = new Properties()
  bigdataProperties.load(new FileReader(BIGDATA_PROPERTIES))
  bigdataProperties.setProperty(Options.FILE, BIGDATA_JOURNAL.pathAsString)
  val sail = new BigdataSail(bigdataProperties)
  val repository = new BigdataSailRepository(sail)
  repository.initialize()
  val baseURI = ""
  val graphURI = new URIImpl("http://kb.phenoscape.org/")
  val bigdata = repository.getUnisolatedConnection()
  val valueFactory = bigdata.getValueFactory
  bigdata.begin()
  bigdata.add(graphURI, DCTERMS.CREATED, valueFactory.createLiteral(new Date()), graphURI)
  bigdata.commit()

  def loadOntologiesAndCreateReasoner(): OWLReasoner = {
    bigdata.begin()
    step("Loading ontologies")
    val phenoscapeVocab = loadFromWeb(IRI.create("https://raw.githubusercontent.com/phenoscape/phenoscape-ontologies/master/phenoscape-kb-vocab/vocab.owl"), false)
    addTriples(phenoscapeVocab, bigdata, graphURI)
    val attributes = loadFromWeb(IRI.create("http://svn.code.sf.net/p/phenoscape/code/trunk/vocab/character_slims.obo"), false)
    addTriples(attributes, bigdata, graphURI)
    val uberon = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/uberon/ext.owl"), true)
    addTriples(uberon, bigdata, graphURI)
    val cl = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/cl.owl"), true)
    addTriples(cl, bigdata, graphURI)
    val homology = loadFromWeb(IRI.create("http://purl.org/phenoscape/demo/phenoscape_homology.owl"), true)
    addTriples(homology, bigdata, graphURI)
    val vtoToNCBI = loadFromWeb(IRI.create("http://purl.org/phenoscape/demo/vto_ncbi.owl"), true)
    addTriples(vtoToNCBI, bigdata, graphURI)
    val pato = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/pato.owl"), true)
    addTriples(pato, bigdata, graphURI)
    val bspo = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/bspo.owl"), true)
    addTriples(bspo, bigdata, graphURI)
    val go = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/go.owl"), true)
    addTriples(go, bigdata, graphURI)
    val taxrank = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/taxrank.owl"), true)
    addTriples(taxrank, bigdata, graphURI)
    val vto = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/vto.owl"), true)
    addTriples(vto, bigdata, graphURI)
    val collections = loadFromWeb(IRI.create("http://svn.code.sf.net/p/phenoscape/code/trunk/vocab/fish_collection_abbreviation.obo"), true)
    addTriples(collections, bigdata, graphURI)
    val zfa = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/upheno/imports/zfa_import.owl"), true)
    addTriples(zfa, bigdata, graphURI)
    val xao = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/xao.owl"), true)
    addTriples(xao, bigdata, graphURI)
    val hp = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/hp.owl"), true)
    addTriples(hp, bigdata, graphURI)
    val mp = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/mp.owl"), true)
    addTriples(mp, bigdata, graphURI)
    val ro = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/ro.owl"), false) //.axioms.filter(_.isAnnotationAxiom)
    addTriples(ro, bigdata, graphURI)
    val eco = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/eco/eco-base.owl"), false)
    addTriples(eco, bigdata, graphURI)

    val caroToUberon = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-caro.owl"), true)
    addTriples(caroToUberon, bigdata, graphURI)
    val zfaToUberon = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-zfa.owl"), true)
    addTriples(zfaToUberon, bigdata, graphURI)
    val xaoToUberon = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-xao.owl"), true)
    addTriples(xaoToUberon, bigdata, graphURI)
    val mgiToEMAPA = SourcedAxioms(loadNormalized((SOURCES / "mgi_anatomy.owl").toJava))
    addTriples(mgiToEMAPA, bigdata, graphURI)
    //val emapa = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/emapa.owl"))
    //addTriples(emapa, bigdata, graphURI)
    val emapaToUberon = loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-emapa.owl"), true)
    addTriples(emapaToUberon, bigdata, graphURI)

    step("Querying entities and qualities")
    val coreReasoner = reasoner(Set(uberon, cl, pato, bspo, phenoscapeVocab).flatMap(_.axioms))
    val anatomicalEntities = coreReasoner.getSubClasses(Class(Vocab.ANATOMICAL_ENTITY), false).getFlattened.asScala.filterNot(_.isOWLNothing) + Class(Vocab.ANATOMICAL_ENTITY)
    val qualities = coreReasoner.getSubClasses(Class(Vocab.QUALITY), false).getFlattened.asScala.filterNot(_.isOWLNothing) + Class(Vocab.QUALITY)
    coreReasoner.dispose()

    step("Converting NeXML to OWL")
    val vocabForNeXML = combine(uberon, pato, bspo, phenoscapeVocab)
    val filesToConvert = (FileUtils.listFiles((NEXML / "completed-phenex-files").toJava, Array("xml"), true).asScala ++
      FileUtils.listFiles((NEXML / "fin_limb-incomplete-files").toJava, Array("xml"), true).asScala ++
      FileUtils.listFiles((NEXML / "Jackson_Dissertation_Files").toJava, Array("xml"), true).asScala ++
      FileUtils.listFiles((NEXML / "teleost-incomplete-files" / "Miniature_Monographs").toJava, Array("xml"), true).asScala ++
      FileUtils.listFiles((NEXML / "teleost-incomplete-files" / "Miniatures_Matrix_Files").toJava, Array("xml"), true).asScala ++
      FileUtils.listFiles((NEXML / "teleost-incomplete-files" / "Dillman_Supermatrix_Files").toJava, Array("xml"), true).asScala ++
      FileUtils.listFiles((NEXML / "matrix-vs-monograph").toJava, Array("xml"), true).asScala).filterNot(_.getName == "catalog-v001.xml")
    val nexmlTBoxAxioms: mutable.Set[OWLAxiom] = mutable.Set()
    for (file <- filesToConvert) {
      println(s"Adding NeXML file: $file")
      val nexOntology = PropertyNormalizer.normalize(PhenexToOWL.convert(file, vocabForNeXML))
      nexmlTBoxAxioms ++= nexOntology.getTBoxAxioms(Imports.EXCLUDED).asScala
      addTriples(nexOntology, bigdata, graphURI)
    }

    step("Converting ZFIN data")
    val zfinGenes = PropertyNormalizer.normalize(ZFINGeneticMarkersToOWL.convert((SOURCES / "zfin_genetic_markers.txt").toSource("ISO-8859-1")))
    addTriples(zfinGenes, bigdata, graphURI)
    val zfinPreviousGeneNames = PropertyNormalizer.normalize(ZFINPreviousGeneNamesToOWL.convert((SOURCES / "zfin_aliases.txt").toSource("ISO-8859-1")))
    addTriples(zfinPreviousGeneNames, bigdata, graphURI)
    val zfinExpressionData = PropertyNormalizer.normalize(ZFINExpressionToOWL.convert((SOURCES / "zfin_wildtype_expression.txt").toSource("ISO-8859-1")))
    addTriples(zfinExpressionData, bigdata, graphURI)
    val zfinPhenotypeData = PropertyNormalizer.normalize(ZFINPhenotypesToOWL.convert((SOURCES / "zfin_phenotypes.txt").toSource("ISO-8859-1")))
    addTriples(zfinPhenotypeData, bigdata, graphURI)

    step("Converting MGI data")
    val mgiGenes = PropertyNormalizer.normalize(MGIGeneticMarkersToOWL.convert((SOURCES / "mgi_genes.txt").toSource("utf-8")))
    addTriples(mgiGenes, bigdata, graphURI)
    val mgiExpressionData = PropertyNormalizer.normalize(MGIExpressionToOWL.convert((SOURCES / "mgi_expression_data.txt").toSource("utf-8")))
    addTriples(mgiExpressionData, bigdata, graphURI)
    val mgiPhenotypeData = PropertyNormalizer.normalize(MGIPhenotypesToOWL.convert((SOURCES / "mgi_phenotypes.txt").toSource("utf-8")))
    addTriples(mgiPhenotypeData, bigdata, graphURI)

    step("Converting Xenbase data")
    val xenbaseGenes = PropertyNormalizer.normalize(XenbaseGenesToOWL.convert((SOURCES / "xenbase_genes.txt").toSource("utf-8")))
    addTriples(xenbaseGenes, bigdata, graphURI)
    val xenbaseExpressionData = PropertyNormalizer.normalize(XenbaseExpressionToOWL.convert(
      (SOURCES / "xenbase_genepage_mappings.txt").toSource("utf-8"),
      (SOURCES / "GeneExpression_laevis.txt").toSource("utf-8"),
      (SOURCES / "GeneExpression_tropicalis.txt").toSource("utf-8")))
    addTriples(xenbaseExpressionData, bigdata, graphURI)
    val xenbasePhenotypeFiles = FileUtils.listFiles((SOURCES / "xenbase-phenotypes").toJava, Array("txt"), true)
    val xenbasePhenotypeData = PropertyNormalizer.normalize(xenbasePhenotypeFiles.asScala.flatMap(f => XenbasePhenotypesToOWL.convertToAxioms(Source.fromFile(f))).toSet)
    addTriples(xenbasePhenotypeData, bigdata, graphURI)

    step("Converting human data")
    val humanPhenotypeData = PropertyNormalizer.normalize(HumanPhenotypesToOWL.convert((SOURCES / "hp_phenotypes.txt").toSource(f"utf-8")))
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
    val attributeQualities = attributes.axioms.flatMap(_.getClassesInSignature.asScala) + HasNumberOf
    val subsumers = for {
      entity <- anatomicalEntities
      (partsTerm, entityPartsAxioms) = SimilarityTemplates.partsOfEntity(entity)
      (developsFromTerm, developsFromAxioms) = SimilarityTemplates.developsFromEntity(entity)
      axiom <- (entityPartsAxioms ++ developsFromAxioms)
    } yield axiom
    addTriples(subsumers, bigdata, graphURI)

    val allTBox = ro.axioms ++ uberon.axioms ++ homology.axioms ++ pato.axioms ++ bspo.axioms ++ vto.axioms ++ vtoToNCBI.axioms ++ zfa.axioms ++ xao.axioms ++ hp.axioms ++ mp.axioms ++
      caroToUberon.axioms ++ zfaToUberon.axioms ++ xaoToUberon.axioms ++ mgiToEMAPA.axioms ++ emapaToUberon.axioms ++ eco.axioms ++
      parts ++ hasParts ++ hasPartsInheringIns ++ phenotypeOfs ++ presences ++ absences ++ absenceNegationEquivalences ++ developsFromRulesForAbsence ++ subsumers ++ tboxFromData ++ phenoscapeVocab.axioms

    val coreTBox = ro.axioms ++ uberon.axioms ++ homology.axioms ++ pato.axioms ++ bspo.axioms ++ vto.axioms ++ vtoToNCBI.axioms ++ zfa.axioms ++ xao.axioms ++ hp.axioms ++ mp.axioms ++
      caroToUberon.axioms ++ zfaToUberon.axioms ++ xaoToUberon.axioms ++ mgiToEMAPA.axioms ++ emapaToUberon.axioms ++ eco.axioms ++
      developsFromRulesForAbsence ++ tboxFromData ++ phenoscapeVocab.axioms
    println("tbox class count: " + allTBox.flatMap(_.getClassesInSignature.asScala).size)
    println("tbox logical axiom count: " + allTBox.filter(_.isLogicalAxiom).size)
    val tBoxWithoutDisjoints = OntologyUtil.filterDisjointAxioms(allTBox)
    val coreTBoxWithoutDisjoints = OntologyUtil.filterDisjointAxioms(coreTBox)

    step("Check satisfiability with disjoints")
    val disjointReasoner = reasoner(coreTBox)
    if (disjointReasoner.getUnsatisfiableClasses().getEntitiesMinusBottom().isEmpty()) {
      println("SUCCESS: all classes are satisfiable with disjoints.")
    } else {
      println("WARNING: some classes are unsatisfiable with disjoints.")
      println(disjointReasoner.getUnsatisfiableClasses())
    }
    disjointReasoner.dispose()

    step("Materializing tbox classification")
    val tboxReasoner = reasoner(tBoxWithoutDisjoints)
    val inferredAxioms = manager.createOntology()
    MaterializeInferences.materializeInferences(inferredAxioms, tboxReasoner)
    tboxReasoner.dispose()

    val coreTboxOntology = manager.createOntology(coreTBoxWithoutDisjoints.asJava)
    val coreTboxReasoner = reasoner(coreTboxOntology)
    MaterializeInferences.materializeInferences(coreTboxOntology, coreTboxReasoner)
    coreTboxReasoner.dispose()

    step("Asserting reverse negation hierarchy")
    val hierarchyAxioms = NegationHierarchyAsserter.assertNegationHierarchy(tBoxWithoutDisjoints ++ inferredAxioms.getAxioms().asScala)
    step("Adding negation hierarchy axioms")
    manager.addAxioms(inferredAxioms, hierarchyAxioms.asJava)
    step("Creating negation reasoner")
    implicit val negationReasoner = reasoner(tBoxWithoutDisjoints ++ inferredAxioms.getAxioms().asScala)
    step("Materializing negation reasoner inferences")
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
    val tboxOut = OWLManager.createOWLOntologyManager().createOntology((tBoxWithoutDisjoints ++ inferredAxioms.getAxioms().asScala).asJava)

    write(tboxOut, (KB / "tbox.owl").toJava)
    bigdata.add((KB / "tbox.owl").toJava, "", RDFFormat.RDFXML, graphURI)

    write(coreTboxOntology, (KB / "core-tbox.owl").toJava)

    step("Reducing tbox for OWLsim")
    val reducedTbox = OntologyUtil.reduceOntologyToHierarchy(tboxOut)
    write(reducedTbox, (KB / "tbox-hierarchy-only.owl").toJava)

    step("Building evolutionary profiles using ancestral states reconstruction")
    val vtoOnt = OWLManager.createOWLOntologyManager().createOntology(vto.axioms.asJava)
    bigdata.add(EvolutionaryProfiles.computePhenotypeProfiles(TaxonNode(CHORDATA), vtoOnt, bigdata).asJava, graphURI)

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

  step("Building gene profiles")
  bigdata.begin()
  bigdata.add(GeneProfiles.generateGeneProfiles(bigdata).asJava, graphURI)
  bigdata.commit()

  fullReasoner.dispose()
  System.gc()

  step("Exporting presence assertions")
  val presencesFile = (KB / "presences.ttl").toJava
  val presencesOutput = new BufferedOutputStream(new FileOutputStream(presencesFile))
  bigdata.prepareGraphQuery(QueryLanguage.SPARQL, presencesQuery.toString).evaluate(new TurtleWriter(presencesOutput))
  presencesOutput.close()

  step("Exporting absence assertions")
  val absencesFile = (KB / "absences.ttl").toJava
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
  val triplesOutput = new BufferedOutputStream(new FileOutputStream((KB / "kb.ttl").toJava))
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
  val profilesOutput = new BufferedOutputStream(new FileOutputStream((KB / "profiles.ttl").toJava))
  profilesQuery.evaluate(new TurtleWriter(profilesOutput))
  profilesOutput.close()

  bigdata.close()

  step("Done")

}