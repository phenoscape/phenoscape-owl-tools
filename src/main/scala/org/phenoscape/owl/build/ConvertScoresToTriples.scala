package org.phenoscape.owl.build

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.parallel._
import scala.collection.optimizer._
import scala.collection.par.Scheduler.Implicits.global
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.reasoner.{ Node => ReasonerNode }
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.openrdf.model.Statement
import org.phenoscape.owl.util.OntologyUtil
import org.openrdf.model.impl.URIImpl
import org.openrdf.model.impl.StatementImpl
import org.phenoscape.owl.Vocab
import org.openrdf.model.impl.NumericLiteralImpl
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.util.Date
import scala.collection.JavaConverters._
import org.openrdf.rio.RDFFormat
import org.openrdf.rio.Rio
import org.phenoscape.owl.sim.OWLsim
import org.phenoscape.owl.util.OntologyUtil
import org.semanticweb.owlapi.apibinding.OWLManager
import scala.io.Source

object ConvertScoresToTriples extends App {

  import ConvertScoresToTriplesUtil._

  val scoresFilePath = args(0)
  val scoresFile = Source.fromFile(scoresFilePath, "utf-8")
  val triples = scoresFile.getLines.drop(1).map(lineToTriple)
  val triplesOutput = new BufferedOutputStream(new FileOutputStream(new File("expect_scores.ttl")))
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