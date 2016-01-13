package org.phenoscape.owl

import scala.collection.JavaConversions._

import org.junit.Test
import org.semanticweb.owlapi.apibinding.OWLManager

import org.phenoscape.scowl.OWL._

import junit.framework.Assert

class NegationHierarchyAsserterTest {

  val base = "http://owl.phenoscape.org/NegationHierarchyAsserterTest"

  @Test
  def testHierarchy() {
    val manager = OWLManager.createOWLOntologyManager()
    val input = getClass().getClassLoader().getResourceAsStream("NegationHierarchyAsserterTest.ofn")
    val ontology = manager.loadOntologyFromOntologyDocument(input)
    input.close()
    Assert.assertTrue(ontology.containsAxiom(Class(s"$base#B") SubClassOf Class(s"$base#A")))
    Assert.assertFalse(ontology.containsAxiom(Class(s"$base#NotA") SubClassOf Class(s"$base#NotB")))
    Assert.assertTrue(ontology.containsAxiom(Class(s"$base#C") SubClassOf Class(s"$base#B")))
    Assert.assertFalse(ontology.containsAxiom(Class(s"$base#NotB") SubClassOf Class(s"$base#NotC")))
    Assert.assertTrue(ontology.containsAxiom(Class(s"$base#B") EquivalentTo Class(s"$base#D")))
    Assert.assertFalse(ontology.containsAxiom(Class(s"$base#NotB") EquivalentTo Class(s"$base#NotD")))
    manager.addAxioms(ontology, NegationHierarchyAsserter.assertNegationHierarchy(ontology.getAxioms.toSet))
    Assert.assertTrue(ontology.containsAxiom(Class(s"$base#NotA") SubClassOf Class(s"$base#NotB")))
    Assert.assertTrue(ontology.containsAxiom(Class(s"$base#NotB") SubClassOf Class(s"$base#NotC")))
    Assert.assertTrue(ontology.containsAxiom(Class(s"$base#NotB") EquivalentTo Class(s"$base#NotD")))
  }

}