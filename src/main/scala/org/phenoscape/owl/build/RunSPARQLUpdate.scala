package org.phenoscape.owl.build

import java.io.{File, FileReader}
import java.util.Properties

import com.bigdata.journal.Options
import com.bigdata.rdf.sail.{BigdataSail, BigdataSailRepository}
import org.openrdf.query.QueryLanguage

import scala.io.Source

object RunSPARQLUpdate extends App {

  val BlazegraphProperties = new File(args(0))
  val BlazegraphJournal = new File(args(1))
  val queryFile = new File(args(2))

  val blazegraphProperties = new Properties()
  blazegraphProperties.load(new FileReader(BlazegraphProperties))
  blazegraphProperties.setProperty(Options.FILE, BlazegraphJournal.getAbsolutePath)
  val sail = new BigdataSail(blazegraphProperties)
  val repository = new BigdataSailRepository(sail)
  repository.initialize()
  val blazegraph = repository.getUnisolatedConnection()

  val query = blazegraph.prepareUpdate(QueryLanguage.SPARQL, Source.fromFile(queryFile, "utf-8").mkString)
  query.execute()
  blazegraph.close()

}
