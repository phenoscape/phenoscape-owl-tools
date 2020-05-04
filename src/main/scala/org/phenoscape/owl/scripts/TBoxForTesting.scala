package org.phenoscape.owl.build

import java.io.File

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.io.Source
import scala.language.postfixOps

import org.apache.commons.io.FileUtils
import org.phenoscape.kb.ingest.human.HumanPhenotypesToOWL
import org.phenoscape.kb.ingest.mgi.MGIPhenotypesToOWL
import org.phenoscape.kb.ingest.xenbase.XenbaseExpressionToOWL
import org.phenoscape.kb.ingest.xenbase.XenbasePhenotypesToOWL
import org.phenoscape.kb.ingest.zfin.ZFINExpressionToOWL
import org.phenoscape.kb.ingest.zfin.ZFINPhenotypesToOWL
import org.phenoscape.owl.AbsenceClassGenerator
import org.phenoscape.owl.KnowledgeBaseBuilder
import org.phenoscape.owl.NamedRestrictionGenerator
import org.phenoscape.owl.NegationClassGenerator
import org.phenoscape.owl.PhenexToOWL
import org.phenoscape.owl.PropertyNormalizer
import org.phenoscape.owl.ReverseDevelopsFromRuleGenerator
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.OntologyUtil
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

object TBoxForTesting extends KnowledgeBaseBuilder {

  val cwd = "/hpchome/nescent/jpb15/Phenoscape-KB"
  val STAGING = new File(cwd + "/staging")
  val KB = new File(cwd + "/staging/kb")
  val NEXML = new File(cwd + "/staging/nexml")

  val manager = getManager
  val rdfsSubClassOf = ObjectProperty(OWLRDFVocabulary.RDFS_SUBCLASS_OF.getIRI)
  val implies_presence_of_some = NamedRestrictionGenerator.getClassRelationIRI(
    Vocab.IMPLIES_PRESENCE_OF.getIRI
  )

  step("Loading ontologies")
  val hp =
    loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/hp.owl"), false)
  val phenoscapeVocab =
    loadFromWeb(IRI.create("http://purl.org/phenoscape/vocab.owl"), false)
  val attributes = loadFromWeb(
    IRI.create(
      "http://svn.code.sf.net/p/phenoscape/code/trunk/vocab/character_slims.obo"
    ),
    false
  )
  val uberon = loadFromWeb(
    IRI.create("http://purl.obolibrary.org/obo/uberon/ext.owl"),
    false
  )
  val homology = loadFromWeb(
    IRI.create("http://purl.obolibrary.org/obo/uberon/homology.owl"),
    false
  )
  val pato =
    loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/pato.owl"), false)
  val bspo =
    loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/bspo.owl"), false)
  val go =
    loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/go.owl"), false)
  val taxrank =
    loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/taxrank.owl"), false)
  val vto =
    loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/vto.owl"), false)
  val collections = loadFromWeb(
    IRI.create(
      "http://svn.code.sf.net/p/phenoscape/code/trunk/vocab/fish_collection_abbreviation.obo"
    ),
    false
  )
  val zfa =
    loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/zfa.owl"), false)
  val xao =
    loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/xao.owl"), false)
  val mp =
    loadFromWeb(IRI.create("http://purl.obolibrary.org/obo/mp.owl"), false)

  val caroToUberon = loadFromWeb(
    IRI.create(
      "http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-caro.owl"
    ),
    false
  )
  val zfaToUberon = loadFromWeb(
    IRI.create(
      "http://purl.obolibrary.org/obo/uberon/bridge/uberon-ext-bridge-to-zfa.owl"
    ),
    false
  )
  val xaoToUberon = loadFromWeb(
    IRI.create(
      "http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-xao.owl"
    ),
    false
  )

  step("Querying entities and qualities")
  val coreReasoner = reasoner(
    Set(uberon, pato, bspo, go, phenoscapeVocab).flatMap(_.axioms)
  )
  val anatomicalEntities = coreReasoner
    .getSubClasses(Class(Vocab.ANATOMICAL_ENTITY), false)
    .getFlattened
    .asScala
    .filterNot(_.isOWLNothing)
  val qualities = coreReasoner
    .getSubClasses(Class(Vocab.QUALITY), false)
    .getFlattened
    .asScala
    .filterNot(_.isOWLNothing)
  coreReasoner.dispose()

  step("Converting NeXML to OWL")
  val vocabForNeXML = combine(uberon, pato, bspo, go, phenoscapeVocab)
  val filesToConvert = (FileUtils
    .listFiles(
      new File(cwd + "/staging/nexml/completed-phenex-files"),
      Array("xml"),
      true
    )
    .asScala ++
    FileUtils
      .listFiles(
        new File(cwd + "/staging/nexml/fin_limb-incomplete-files"),
        Array("xml"),
        true
      )
      .asScala ++
    FileUtils
      .listFiles(
        new File(cwd + "/staging/nexml/Jackson Dissertation Files"),
        Array("xml"),
        true
      )
      .asScala ++
    FileUtils
      .listFiles(
        new File(cwd + "/staging/nexml/matrix-vs-monograph"),
        Array("xml"),
        true
      )
      .asScala).filterNot(_.getName == "catalog-v001.xml")
  val nexmlTBoxAxioms: mutable.Set[OWLAxiom] = mutable.Set()
  for (file <- filesToConvert) {
    val nexOntology =
      PropertyNormalizer.normalize(PhenexToOWL.convert(file, vocabForNeXML))
    nexmlTBoxAxioms.asJava.addAll(nexOntology.getTBoxAxioms(Imports.EXCLUDED))
  }

  step("Converting ZFIN data")
  val zfinExpressionData = PropertyNormalizer.normalize(
    ZFINExpressionToOWL.convert(
      Source.fromFile(
        new File(cwd + "/staging/sources/zfin_wildtype_expression.txt"),
        "ISO-8859-1"
      )
    )
  )
  val zfinPhenotypeData = PropertyNormalizer.normalize(
    ZFINPhenotypesToOWL.convert(
      Source.fromFile(
        new File(cwd + "/staging/sources/zfin_phenotypes.txt"),
        "ISO-8859-1"
      )
    )
  )

  step("Converting MGI data")
  val mgiPhenotypeData = PropertyNormalizer.normalize(
    MGIPhenotypesToOWL.convert(
      Source.fromFile(
        new File(cwd + "/staging/sources/mgi_phenotypes.txt"),
        "utf-8"
      )
    )
  )

  step("Converting Xenbase data")
  val xenbaseExpressionData = PropertyNormalizer.normalize(
    XenbaseExpressionToOWL.convert(
      Source.fromFile(
        new File(cwd + "/staging/sources/xenbase_genepage_mappings.txt"),
        "utf-8"
      ),
      Source.fromFile(
        new File(cwd + "/staging/sources/GeneExpression_laevis.txt"),
        "utf-8"
      ),
      Source.fromFile(
        new File(cwd + "/staging/sources/GeneExpression_tropicalis.txt"),
        "utf-8"
      )
    )
  )
  val xenbasePhenotypeFiles = FileUtils.listFiles(
    new File(cwd + "/staging/sources/xenbase-phenotypes"),
    Array("txt"),
    true
  )
  val xenbasePhenotypeData = PropertyNormalizer.normalize(
    xenbasePhenotypeFiles.asScala
      .flatMap(f => XenbasePhenotypesToOWL.convertToAxioms(Source.fromFile(f)))
      .toSet
  )

  step("Converting human data")
  val humanPhenotypeData = PropertyNormalizer.normalize(
    HumanPhenotypesToOWL.convert(
      Source
        .fromFile(new File(cwd + "/staging/sources/hp_phenotypes.txt"), "utf-8")
    )
  )

  step("Generating tbox")
  val tboxFromData =
    zfinExpressionData.filter(isTboxAxiom) ++
      zfinPhenotypeData.filter(isTboxAxiom) ++
      mgiPhenotypeData.filter(isTboxAxiom) ++
      xenbaseExpressionData.filter(isTboxAxiom) ++
      xenbasePhenotypeData.filter(isTboxAxiom) ++
      humanPhenotypeData.filter(isTboxAxiom) ++
      nexmlTBoxAxioms

  val hasParts = (anatomicalEntities ++ qualities).flatMap(
    NamedRestrictionGenerator.createRestriction(has_part, _)
  )
  val presences = anatomicalEntities.flatMap(
    NamedRestrictionGenerator.createRestriction(Vocab.IMPLIES_PRESENCE_OF, _)
  )
  val hasPartsInheringIns = anatomicalEntities.flatMap(
    NamedRestrictionGenerator.createRestriction(has_part_inhering_in, _)
  )
  val phenotypeOfs = anatomicalEntities.flatMap(
    NamedRestrictionGenerator.createRestriction(phenotype_of, _)
  )
  val absences =
    anatomicalEntities.flatMap(AbsenceClassGenerator.createAbsenceClass)
  val namedHasPartClasses = anatomicalEntities
    .map(_.getIRI)
    .map(NamedRestrictionGenerator.getRestrictionIRI(has_part.getIRI, _))
    .map(Class(_))
  val absenceNegationEquivalences = namedHasPartClasses.flatMap(
    NegationClassGenerator.createNegationClassAxioms
  )
  val developsFromRulesForAbsence = anatomicalEntities
    .flatMap(ReverseDevelopsFromRuleGenerator.createRules)
    .toSet[OWLAxiom]

  val allTBox = uberon.axioms ++ homology.axioms ++ pato.axioms ++ bspo.axioms ++ go.axioms ++ vto.axioms ++ zfa.axioms ++ xao.axioms ++ hp.axioms ++ mp.axioms ++
    caroToUberon.axioms ++ zfaToUberon.axioms ++ xaoToUberon.axioms ++ hasParts ++ hasPartsInheringIns ++ phenotypeOfs ++ presences ++ absences ++ absenceNegationEquivalences ++ developsFromRulesForAbsence ++ tboxFromData ++ phenoscapeVocab.axioms

  println(
    "tbox class count: " + allTBox.flatMap(_.getClassesInSignature.asScala).size
  )
  println("tbox logical axiom count: " + allTBox.filter(_.isLogicalAxiom).size)
  val tBoxWithoutDisjoints = OntologyUtil.filterDisjointAxioms(allTBox)

  //step("Writing inferred tbox axioms")
  //addTriples(inferredAxioms, bigdata, graphURI)

  step("Writing tbox axioms for ELK")
  val tboxOut = OWLManager
    .createOWLOntologyManager()
    .createOntology(tBoxWithoutDisjoints.asJava)
  write(tboxOut, new File(cwd + "/staging/kb/tbox-for-pascal.owl"))

  step("Done")

}
