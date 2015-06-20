package org.phenoscape.owl.build

import org.semanticweb.owlapi.apibinding.OWLManager
import java.io.File
import scala.collection.JavaConversions._
import org.phenoscape.owl.util.OBOUtil

object AddDefinedBy extends App {

  val ontFile = new File(args(0))
  val manager = OWLManager.createOWLOntologyManager()
  val ont = manager.loadOntologyFromOntologyDocument(ontFile)
  val definedByAxioms = ont.getClassesInSignature.flatMap(OBOUtil.createDefinedByAnnotation)
  manager.addAxioms(ont, definedByAxioms)
  manager.saveOntology(ont)

}