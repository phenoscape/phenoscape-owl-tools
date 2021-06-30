package org.phenoscape.owl.build

import org.phenoscape.kb.ingest.util.OBOUtil
import org.semanticweb.owlapi.apibinding.OWLManager

import java.io.File
import scala.jdk.CollectionConverters._

object AddDefinedBy extends App {

  val ontFile = new File(args(0))
  val manager = OWLManager.createOWLOntologyManager()
  val ont = manager.loadOntologyFromOntologyDocument(ontFile)
  val definedByAxioms = ont.getClassesInSignature().asScala.flatMap(OBOUtil.createDefinedByAnnotation)
  manager.addAxioms(ont, definedByAxioms.asJava)
  manager.saveOntology(ont)

}
