package org.phenoscape.owl.build

import java.io.File
import java.io.FileReader
import java.util.Properties
import org.openrdf.model.impl.URIImpl
import org.openrdf.rio.RDFFormat
import com.bigdata.journal.Options
import com.bigdata.rdf.sail.BigdataSail
import com.bigdata.rdf.sail.BigdataSailRepository
import org.apache.commons.io.FileUtils
//import scala.collection.JavaConversions._
import scala.collection.JavaConversions._
import java.util.Date
import com.bigdata.rdf.store.DataLoader

object LoadTriples extends App {

  val bigdataPropertiesFile = new File(args(0))
  val bigdataJournalFile = new File(args(1))
  val inputFolder = new File(args(2))
  val graphURI = args(3)
  val bigdataProperties = new Properties()
  bigdataProperties.load(new FileReader(bigdataPropertiesFile))
  bigdataProperties.setProperty(Options.FILE, bigdataJournalFile.getAbsolutePath)
  val sail = new BigdataSail(bigdataProperties)
  //val repository = new BigdataSailRepository(sail)
  val tripleStore = sail.getDatabase
  //repository.initialize()
  //val bigdata = repository.getUnisolatedConnection()
  val loader = new DataLoader(tripleStore)
  val baseURI = ""
  val stats = loader.loadFiles(inputFolder, baseURI, RDFFormat.TURTLE, graphURI, null)
  println(stats)
  loader.endSource()
  tripleStore.commit()
  //  for (triplesFile <- FileUtils.listFiles(inputFolder, Array("ttl"), true)) {
  //    val start = new Date()
  //    println(s"Loading $triplesFile")
  //    val stats = loader.loadFiles(triplesFile, baseURI, RDFFormat.TURTLE, graphURI, null)
  //    println(stats)
  //    //bigdata.begin()
  //    //bigdata.add(triplesFile, baseURI, RDFFormat.TURTLE, graphURI)
  //    //bigdata.commit()
  //    val end = new Date()
  //    val seconds = (end.getTime - start.getTime).toDouble / 1000
  //    println(s"Done loading $triplesFile; $seconds seconds")
  //  }
  //bigdata.close()

}