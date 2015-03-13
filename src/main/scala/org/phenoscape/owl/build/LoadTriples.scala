package org.phenoscape.owl.build

import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.FileReader
import java.util.Properties

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.io.Source

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

object LoadTriples extends App {

  val bigdataPropertiesFiles = new File(args(0))
  val bigdataJournalFile = new File(args(1))
  val inputFile = new File(args(2))
  val graphURI = new URIImpl(args(3))
  val bigdataProperties = new Properties()
  bigdataProperties.load(new FileReader(bigdataPropertiesFiles))
  bigdataProperties.setProperty(Options.FILE, bigdataJournalFile.getAbsolutePath)
  val sail = new BigdataSail(bigdataProperties)
  val repository = new BigdataSailRepository(sail)
  repository.initialize()
  val bigdata = repository.getUnisolatedConnection()
  bigdata.setAutoCommit(false);
  val baseURI = ""
  bigdata.add(inputFile, baseURI, RDFFormat.TURTLE, graphURI)
  bigdata.commit()
  bigdata.close()

}