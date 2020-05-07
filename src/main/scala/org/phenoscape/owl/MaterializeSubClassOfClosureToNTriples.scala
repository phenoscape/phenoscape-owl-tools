package org.phenoscape.owl

import java.io.File
import scala.collection.JavaConverters._
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import java.io.BufferedWriter
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import org.semanticweb.owlapi.apibinding.OWLManager

object MaterializeSubClassOfClosureToNTriples extends OWLTask {

  val manager = OWLManager.createOWLOntologyManager;

  def main(args: Array[String]): Unit = {
    val targetFile = new File(System.getProperty("org.phenoscape.owl.MaterializeSubClassOfClosure.target"));
    if (!targetFile.exists())
      targetFile.createNewFile();
    val source = manager.loadOntologyFromOntologyDocument(new File(args(0)));
    println(manager.getOntologies().asScala.map(_.getAxiomCount()).reduce(_ + _));
    val reasoner = new ElkReasonerFactory().createReasoner(source);
    writeClosureToFile(reasoner, targetFile);
    reasoner.dispose();
    System.exit(0);
  }

  def writeClosureToFile(reasoner: OWLReasoner, file: File): Unit = {
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), "utf-8"));
    val allClasses = reasoner.getRootOntology().getClassesInSignature(true).asScala;
    val classCount = allClasses.size;
    println("Total classes: " + classCount);
    var progress = 0;
    for (ontClass <- allClasses) {
      writer.append(
        String
          .format("<%s> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <%s> .", ontClass.getIRI(), ontClass.getIRI())
      );
      writer.newLine();
      val superClasses =
        reasoner.getSuperClasses(ontClass, false).getFlattened().asScala.filterNot(_ == factory.getOWLThing());
      for (superClass <- superClasses) {
        writer.append(
          String.format(
            "<%s> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <%s> .",
            ontClass.getIRI(),
            superClass.getIRI()
          )
        );
        writer.newLine();
      }
      progress += 1;
      if ((progress % 1000) == 0)
        println(progress.floatValue() / classCount * 100 + "%");
    }
    writer.close();
  }

}
