package org.phenoscape.owl.report

import scala.collection.JavaConverters._
import scala.collection.mutable.HashMap
import scala.collection.mutable
import scala.collection.Map
import org.jdom2.input.SAXBuilder
import java.io.File
import org.jdom2.Namespace
import org.jdom2.Element
import org.jdom2.filter.ElementFilter

object AnnotationStatus {

  val nexmlNS = Namespace.getNamespace("http://www.nexml.org/2009");
  val phenoNS = Namespace.getNamespace("http://www.bioontologies.org/obd/schema/pheno");
  val publications = mutable.Set[String]();
  val allCharacters = mutable.Set[Element]();
  val completedCharacters = mutable.Set[Element]();
  val partiallyAnnotatedCharacters = mutable.Set[Element]();
  val unannotatedCharacters = mutable.Set[Element]();
  val annotatedStates = mutable.Set[Element]();
  val allAnnotations = mutable.Set[Element]();

  def main(args: Array[String]): Unit = {
    val builder = new SAXBuilder();
    for (arg <- args) {
      val file = new File(arg);
      val nexml = builder.build(file).getRootElement();
      val pub = file.getName();
      publications.add(pub);
      val format = nexml.getChild("characters", nexmlNS).getChild("format", nexmlNS);
      val stateSets = format.getChildren("states", nexmlNS);
      val stateSetsByID = stateSets.asScala
        .map(states => (states.getAttributeValue("id"), states.getChildren("state", nexmlNS).asScala.toIterable))
        .toMap;
      val characters = format.getChildren("char", nexmlNS);
      var i = 0;
      for (character <- characters.asScala) {
        allCharacters.add(character);
        val states = AnnotationReport.getStates(character, stateSetsByID);
        val annotated = mutable.Set[Element]();
        for (state <- states) {
          val phenotypes =
            state.getDescendants(new ElementFilter("phenotype_character", phenoNS)).iterator().asScala.toList;
          allAnnotations.asJava.addAll(phenotypes.asJava);
          if (!phenotypes.isEmpty) {
            annotatedStates.add(state);
            annotated.add(state);
          }
        }
        if (annotated.size == 0)
          unannotatedCharacters.add(character);
        else if (annotated.size < states.size)
          partiallyAnnotatedCharacters.add(character);
        else
          completedCharacters.add(character);
      }
    }
    println("Publication count: " + publications.size);
    println("Total characters: " + allCharacters.size);
    println("Completed characters: " + completedCharacters.size);
    println("Partially annotated characters: " + partiallyAnnotatedCharacters.size);
    println("Unannotated characters: " + unannotatedCharacters.size);
    println("Annotated states: " + annotatedStates.size);
    println("Total EQ annotations: " + allAnnotations.size);
  }

}
