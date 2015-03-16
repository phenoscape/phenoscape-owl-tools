package org.phenoscape.owl.build

import java.io.File
import java.io.FileReader
import java.util.Properties

import org.openrdf.model.impl.URIImpl
import org.openrdf.rio.RDFFormat

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
  val baseURI = ""
  bigdata.begin()
  bigdata.add(inputFile, baseURI, RDFFormat.TURTLE, graphURI)
  bigdata.commit()
  bigdata.close()

}