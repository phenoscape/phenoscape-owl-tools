package org.phenoscape.owl.build

import java.io.{BufferedOutputStream, File, FileOutputStream}

import org.openrdf.model.Statement
import org.openrdf.model.impl.{NumericLiteralImpl, StatementImpl, URIImpl}
import org.openrdf.rio.{RDFFormat, Rio}
import org.phenoscape.owl.Vocab

import scala.io.Source

object ConvertScoresToTriples extends App {

  import ConvertScoresToTriplesUtil._

  val scoresFilePath = args(0)
  val outfile = new File(args(1))
  val scoresFile = Source.fromFile(scoresFilePath, "utf-8")
  val triples = scoresFile.getLines.drop(1).map(lineToTriple)
  val triplesOutput = new BufferedOutputStream(new FileOutputStream(outfile))
  val writer = Rio.createWriter(RDFFormat.TURTLE, triplesOutput)
  writer.startRDF()
  triples.foreach(writer.handleStatement)
  writer.endRDF()
  triplesOutput.close()

}

object ConvertScoresToTriplesUtil {

  private val has_expect_score = new URIImpl(Vocab.has_expect_score.getIRI.toString)

  def lineToTriple(line: String): Statement = {
    val items = line.split("\t", -1)
    val comparison = new URIImpl(items(0).trim)
    val expectScore = new NumericLiteralImpl(items(3).trim.toDouble)
    new StatementImpl(comparison, has_expect_score, expectScore)
  }

}
