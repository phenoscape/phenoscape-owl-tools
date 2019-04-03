package org.phenoscape.owl

import java.io.File

import org.apache.log4j.{BasicConfigurator, Level, Logger}
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.{Vocab => _}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary


object PostorderTest extends KnowledgeBaseBuilder {

  val inFile = args(0)
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

//  step("Loading Bigdata")
//  val bigdataProperties = new Properties()
//  bigdataProperties.load(new FileReader(BIGDATA_PROPERTIES))
//  bigdataProperties.setProperty(Options.FILE, BIGDATA_JOURNAL.getAbsolutePath)
//  val sail = new BigdataSail(bigdataProperties)
//  val repository = new BigdataSailRepository(sail)
//  repository.initialize()
//  val connection = repository.getUnisolatedConnection()
//  connection.begin()

//  step("Exporting all triples to turtle file")
//  val triplesQuery = bigdata.prepareGraphQuery(QueryLanguage.SPARQL, """
//  CONSTRUCT {
//   ?s ?p ?o .
//  }
//  FROM <http://kb.phenoscape.org/>
//  WHERE {
//   ?s ?p ?o .
//  }
//  """)
//  val triplesOutput = new BufferedOutputStream(new FileOutputStream((KB / "kb.ttl").toJava))
//  triplesQuery.evaluate(new TurtleWriter(triplesOutput))
//  triplesOutput.close()

  step("Testing postorder part of ancestral states reconstruction")
//  println("Triples: " + connection.getTripleStore.getStatementCount)
  val result = EvolutionaryProfiles.computePhenotypeProfiles(TaxonNode(CHORDATA), tbox, inFile)
  println("Size of profile data: " + result.size)
  result.take(100).foreach(println)

//  connection.commit()
  // close the repository connection
//  connection.close()

  step("Done")

}