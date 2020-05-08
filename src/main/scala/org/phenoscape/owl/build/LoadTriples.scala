package org.phenoscape.owl.build

import java.io.File
import java.io.FileReader
import java.util.Properties
import org.openrdf.rio.RDFFormat
import com.bigdata.journal.Options
import com.bigdata.rdf.sail.BigdataSail
import com.bigdata.rdf.store.DataLoader

object LoadTriples extends App {

  val bigdataPropertiesFile = new File(args(0))
  val bigdataJournalFile    = new File(args(1))
  val inputFolder           = new File(args(2))
  val graphURI              = args(3)
  val bigdataProperties     = new Properties()
  bigdataProperties.load(new FileReader(bigdataPropertiesFile))
  bigdataProperties.setProperty(Options.FILE, bigdataJournalFile.getAbsolutePath)
  val sail                  = new BigdataSail(bigdataProperties)
  val tripleStore           = sail.getDatabase
  val loader                = new DataLoader(tripleStore)
  val baseURI               = ""
  val stats                 = loader.loadFiles(inputFolder, baseURI, RDFFormat.TURTLE, graphURI, null)
  println(stats)
  loader.endSource()
  tripleStore.commit()

}
