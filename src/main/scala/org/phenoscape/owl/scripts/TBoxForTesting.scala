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

object TBoxForTesting extends KnowledgeBaseBuilder {

  val cwd = "/hpchome/nescent/jpb15/Phenoscape-KB"
  val STAGING = new File(cwd + "/staging")
  val KB = new File(cwd + "/staging/kb")
  val NEXML = new File(cwd + "/staging/nexml")

  val manager = getManager
  val rdfsSubClassOf = ObjectProperty(OWLRDFVocabulary.RDFS_SUBCLASS_OF.getIRI)
  val implies_presence_of_some = NamedRestrictionGenerator.getClassRelationIRI(Vocab.IMPLIES_PRESENCE_OF.getIRI)

  step("Loading ontologies")
  val hp = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/hp.owl"))
  val phenoscapeVocab = loadFromWebWithImports(IRI.create("http://purl.org/phenoscape/vocab.owl"))
  val attributes = loadFromWebWithImports(IRI.create("http://svn.code.sf.net/p/phenoscape/code/trunk/vocab/character_slims.obo"))
  val uberon = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/uberon/ext.owl"))
  val homology = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/uberon/homology.owl"))
  val pato = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/pato.owl"))
  val bspo = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/bspo.owl"))
  val go = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/go.owl"))
  val taxrank = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/taxrank.owl"))
  val vto = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/vto.owl"))
  val collections = loadFromWebWithImports(IRI.create("http://svn.code.sf.net/p/phenoscape/code/trunk/vocab/fish_collection_abbreviation.obo"))
  val zfa = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/zfa.owl"))
  val xao = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/xao.owl"))
  val mp = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/mp.owl"))

  val caroToUberon = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-caro.owl"))
  val zfaToUberon = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/uberon/bridge/uberon-ext-bridge-to-zfa.owl"))
  val xaoToUberon = loadFromWebWithImports(IRI.create("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-xao.owl"))

  step("Querying entities and qualities")
  val coreReasoner = reasoner(Set(uberon, pato, bspo, go, phenoscapeVocab).flatMap(_.axioms))
  val anatomicalEntities = coreReasoner.getSubClasses(Class(Vocab.ANATOMICAL_ENTITY), false).getFlattened.filterNot(_.isOWLNothing)
  val qualities = coreReasoner.getSubClasses(Class(Vocab.QUALITY), false).getFlattened.filterNot(_.isOWLNothing)
  coreReasoner.dispose()

  step("Converting NeXML to OWL")
  val vocabForNeXML = combine(uberon, pato, bspo, go, phenoscapeVocab)
  val filesToConvert = (FileUtils.listFiles(new File(cwd + "/staging/nexml/completed-phenex-files"), Array("xml"), true) ++
    FileUtils.listFiles(new File(cwd + "/staging/nexml/fin_limb-incomplete-files"), Array("xml"), true) ++
    FileUtils.listFiles(new File(cwd + "/staging/nexml/Jackson Dissertation Files"), Array("xml"), true) ++
    FileUtils.listFiles(new File(cwd + "/staging/nexml/matrix-vs-monograph"), Array("xml"), true)).filterNot(_.getName == "catalog-v001.xml")
  val nexmlTBoxAxioms: mutable.Set[OWLAxiom] = mutable.Set()
  for (file <- filesToConvert) {
    val nexOntology = PropertyNormalizer.normalize(PhenexToOWL.convert(file, vocabForNeXML))
    nexmlTBoxAxioms.addAll(nexOntology.getTBoxAxioms(false))
  }

  step("Converting ZFIN data")
  val zfinExpressionData = PropertyNormalizer.normalize(ZFINExpressionToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/zfin_wildtype_expression.txt"), "ISO-8859-1")))
  val zfinPhenotypeData = PropertyNormalizer.normalize(ZFINPhenotypesToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/zfin_phenotypes.txt"), "ISO-8859-1")))

  step("Converting MGI data")
  val mgiPhenotypeData = PropertyNormalizer.normalize(MGIPhenotypesToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/mgi_phenotypes.txt"), "utf-8")))

  step("Converting Xenbase data")
  val xenbaseExpressionData = PropertyNormalizer.normalize(XenbaseExpressionToOWL.convert(
    Source.fromFile(new File(cwd + "/staging/sources/xenbase_genepage_mappings.txt"), "utf-8"),
    Source.fromFile(new File(cwd + "/staging/sources/GeneExpression_laevis.txt"), "utf-8"),
    Source.fromFile(new File(cwd + "/staging/sources/GeneExpression_tropicalis.txt"), "utf-8")))
  val xenbasePhenotypeFiles = FileUtils.listFiles(new File(cwd + "/staging/sources/xenbase-phenotypes"), Array("txt"), true)
  val xenbasePhenotypeData = PropertyNormalizer.normalize(xenbasePhenotypeFiles.flatMap(f => XenbasePhenotypesToOWL.convertToAxioms(Source.fromFile(f))).toSet)

  step("Converting human data")
  val humanPhenotypeData = PropertyNormalizer.normalize(HumanPhenotypesToOWL.convert(Source.fromFile(new File(cwd + "/staging/sources/hp_phenotypes.txt"), "utf-8")))

  step("Generating tbox")
  val tboxFromData =
    zfinExpressionData.filter(isTboxAxiom) ++
      zfinPhenotypeData.filter(isTboxAxiom) ++
      mgiPhenotypeData.filter(isTboxAxiom) ++
      xenbaseExpressionData.filter(isTboxAxiom) ++
      xenbasePhenotypeData.filter(isTboxAxiom) ++
      humanPhenotypeData.filter(isTboxAxiom) ++
      nexmlTBoxAxioms

  val hasParts = (anatomicalEntities ++ qualities).flatMap(NamedRestrictionGenerator.createRestriction(has_part, _))
  val presences = anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(Vocab.IMPLIES_PRESENCE_OF, _))
  val hasPartsInheringIns = anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(has_part_inhering_in, _))
  val phenotypeOfs = anatomicalEntities.flatMap(NamedRestrictionGenerator.createRestriction(phenotype_of, _))
  val absences = anatomicalEntities.flatMap(AbsenceClassGenerator.createAbsenceClass)
  val namedHasPartClasses = anatomicalEntities.map(_.getIRI).map(NamedRestrictionGenerator.getRestrictionIRI(has_part.getIRI, _)).map(Class(_))
  val absenceNegationEquivalences = namedHasPartClasses.flatMap(NegationClassGenerator.createNegationClassAxioms)
  val developsFromRulesForAbsence = anatomicalEntities.flatMap(ReverseDevelopsFromRuleGenerator.createRules).toSet[OWLAxiom]

  val allTBox = uberon.axioms ++ homology.axioms ++ pato.axioms ++ bspo.axioms ++ go.axioms ++ vto.axioms ++ zfa.axioms ++ xao.axioms ++ hp.axioms ++ mp.axioms ++
    caroToUberon.axioms ++ zfaToUberon.axioms ++ xaoToUberon.axioms ++ hasParts ++ hasPartsInheringIns ++ phenotypeOfs ++ presences ++ absences ++ absenceNegationEquivalences ++ developsFromRulesForAbsence ++ tboxFromData ++ phenoscapeVocab.axioms

  println("tbox class count: " + allTBox.flatMap(_.getClassesInSignature).size)
  println("tbox logical axiom count: " + allTBox.filter(_.isLogicalAxiom).size)
  val tBoxWithoutDisjoints = OntologyUtil.filterDisjointAxioms(allTBox)

  //step("Writing inferred tbox axioms")
  //addTriples(inferredAxioms, bigdata, graphURI)

  step("Writing tbox axioms for ELK")
  val tboxOut = OWLManager.createOWLOntologyManager().createOntology(tBoxWithoutDisjoints)
  write(tboxOut, cwd + "/staging/kb/tbox-for-pascal.owl")

  step("Done")

}