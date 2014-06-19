package org.phenoscape.owl.util

import scala.collection.JavaConversions._

import org.junit.Test
import org.semanticweb.owlapi.apibinding.OWLManager

import org.phenoscape.scowl.OWL._

import junit.framework.Assert

class PostCompositionParserTest {

  @Test
  def testParser() {
    Assert.assertEquals(factory.getOWLObjectIntersectionOf(Class(id("XAO:0004060")),
      (ObjectProperty(id("OBO_REL:part_of")) some (Class(id("XAO:0004060")) and (ObjectProperty(id("OBO_REL:made_from")) some Class(id("XAO:0004396"))))),
      (ObjectProperty(id("OBO_REL:has_part")) some Class(id("XAO:0004396")))),
      PostCompositionParser.parseExpression("XAO:0004060^OBO_REL:part_of(XAO:0004060^OBO_REL:made_from(XAO:0004396))^OBO_REL:has_part(XAO:0004396)").get)
  }

  def id(term: String) = OBOUtil.iriForTermID(term)

}