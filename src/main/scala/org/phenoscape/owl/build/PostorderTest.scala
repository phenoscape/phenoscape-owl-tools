package org.phenoscape.owl.build

import java.io.File
import java.io.FileReader
import java.util.Properties
import org.apache.log4j.BasicConfigurator
import org.apache.log4j.Level
import org.apache.log4j.Logger
import org.phenoscape.owl.KnowledgeBaseBuilder
import org.phenoscape.owl.NamedRestrictionGenerator
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.Vocab._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary
import com.bigdata.journal.Options
import com.bigdata.rdf.sail.BigdataSail
import com.bigdata.rdf.sail.BigdataSailRepository
import org.phenoscape.owl.EvolutionaryProfiles
import org.phenoscape.owl.TaxonNode

object PostorderTest extends KnowledgeBaseBuilder {

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


  step("Loading ontologies")
  val tbox = manager.loadOntologyFromOntologyDocument(new File(cwd + "/staging/kb/tbox.owl"))

  step("Reasoning with tbox")
  val tboxReasoner = reasoner(tbox)

  step("Loading Bigdata")
  val bigdataProperties = new Properties()
  bigdataProperties.load(new FileReader(BIGDATA_PROPERTIES))
  bigdataProperties.setProperty(Options.FILE, BIGDATA_JOURNAL.getAbsolutePath)
  val sail = new BigdataSail(bigdataProperties)
  val repository = new BigdataSailRepository(sail)
  repository.initialize()
  val connection = repository.getUnisolatedConnection()
  connection.begin()

  step("Testing postorder part of ancestral states reconstruction")
  println("Triples: " + connection.getTripleStore.getStatementCount)
  val result = EvolutionaryProfiles.computePhenotypeProfiles(TaxonNode(CHORDATA), tbox, connection)
  println("Size of profile data: " + result.size)
  result.take(100).foreach(println)

  connection.commit()
  // close the repository connection
  connection.close()

  step("Done")

}