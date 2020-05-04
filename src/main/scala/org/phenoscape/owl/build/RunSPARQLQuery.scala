package org.phenoscape.owl.build

import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.FileReader
import java.util.Properties

import scala.io.Source

import org.openrdf.query.QueryLanguage
import org.openrdf.query.resultio.text.tsv.SPARQLResultsTSVWriter

import com.bigdata.journal.Options

import com.bigdata.rdf.sail.BigdataSail
import com.bigdata.rdf.sail.BigdataSailRepository

object RunSPARQLQuery extends App {

  val BlazegraphProperties = new File(args(0))
  val BlazegraphJournal    = new File(args(1))
  val queryFile            = new File(args(2))
  val outFile              = new File(args(3))

  val blazegraphProperties = new Properties()
  blazegraphProperties.load(new FileReader(BlazegraphProperties))
  blazegraphProperties.setProperty(Options.FILE, BlazegraphJournal.getAbsolutePath)
  val sail       = new BigdataSail(blazegraphProperties)
  val repository = new BigdataSailRepository(sail)
  repository.initialize()
  val blazegraph = repository.getUnisolatedConnection()

  val query       = blazegraph.prepareTupleQuery(QueryLanguage.SPARQL, Source.fromFile(queryFile, "utf-8").mkString)
  val queryOutput = new BufferedOutputStream(new FileOutputStream(outFile))
  query.evaluate(new SPARQLResultsTSVWriter(queryOutput))
  queryOutput.close()
  blazegraph.close()

}
