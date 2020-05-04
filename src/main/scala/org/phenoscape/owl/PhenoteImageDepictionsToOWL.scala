package org.phenoscape.owl

import scala.io.Source
import org.phenoscape.kb.ingest.util.OBOUtil
import org.semanticweb.owlapi.model.IRI
import org.apache.commons.lang3.StringUtils
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary
import java.io.File
import org.semanticweb.owlapi.apibinding.OWLManager
import org.phenoscape.owl.Vocab._

object PhenoteImageDepictionsToOWL extends OWLTask {

  val imageClass = factory.getOWLClass(Vocab.IMAGE);
  val hasDescription = factory.getOWLAnnotationProperty(DublinCoreVocabulary.DESCRIPTION.getIRI());

  def main(args: Array[String]): Unit = {
    val manager = OWLManager.createOWLOntologyManager();
    val depictionsOntology = manager.createOntology();
    val annotations = Source.fromFile(args(0), "utf-8").getLines();
    val targetFile = new File(args(1));
    val imageIRIPrefix = if (args.length > 2) args(2) else "";
    val line1 = annotations.next().split("\t");
    val imageURLIndex = line1.indexOf("Image URL");
    val depictedStructureIndex = line1.indexOf("Depicted Structure ID");
    val locatorIndex = line1.indexOf("Containing Structure (optional) ID");
    val taxonIndex = line1.indexOf("Taxon ID");
    val descriptionIndex = line1.indexOf("Description");
    for (line <- annotations) {
      val items = line.split("\t", -1);
      val image = factory.getOWLNamedIndividual(IRI.create(imageIRIPrefix + items(imageURLIndex).trim()));
      val depictedStructure = factory.getOWLClass(OBOUtil.iriForTermID(items(depictedStructureIndex).trim()));
      val locatorOption =
        Option(StringUtils.stripToNull(items(locatorIndex))).map(id => factory.getOWLClass(OBOUtil.iriForTermID(id)));
      val taxon = factory.getOWLClass(OBOUtil.iriForTermID(items(taxonIndex).trim()));
      val descriptionOption = Option(StringUtils.stripToNull(items(descriptionIndex))).map(factory.getOWLLiteral(_));
      manager.addAxiom(depictionsOntology, factory.getOWLDeclarationAxiom(image));
      manager.addAxiom(depictionsOntology, factory.getOWLClassAssertionAxiom(imageClass, image));
      val depictedClass = locatorOption match {
        case Some(locator) =>
          factory.getOWLObjectIntersectionOf(
            depictedStructure,
            factory.getOWLObjectSomeValuesFrom(part_of, locator),
            factory.getOWLObjectSomeValuesFrom(part_of, taxon)
          );
        case None =>
          factory.getOWLObjectIntersectionOf(depictedStructure, factory.getOWLObjectSomeValuesFrom(part_of, taxon));
      }
      manager.addAxiom(
        depictionsOntology,
        factory.getOWLClassAssertionAxiom(factory.getOWLObjectSomeValuesFrom(DEPICTS, depictedClass), image)
      );
      descriptionOption.foreach { description =>
        manager.addAxiom(
          depictionsOntology,
          factory.getOWLAnnotationAssertionAxiom(hasDescription, image.getIRI(), description)
        );
      };
    }
    manager.saveOntology(depictionsOntology, IRI.create(targetFile));
  }

}
