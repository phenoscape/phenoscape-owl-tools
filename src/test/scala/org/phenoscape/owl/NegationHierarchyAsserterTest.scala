package org.phenoscape.owl

import scala.collection.JavaConversions._

import org.junit.Test
import org.semanticweb.owlapi.apibinding.OWLManager

import junit.framework.Assert

class NegationHierarchyAsserterTest {

  @Test
  def testHierarchy() {
    val manager = OWLManager.createOWLOntologyManager()
    val input = getClass().getClassLoader().getResourceAsStream("NegationHierarchyAsserterTest.owl")
    val expectation = getClass().getClassLoader().getResourceAsStream("NegationHierarchyAsserterTestExpectation.owl")
    val ontology = manager.loadOntologyFromOntologyDocument(input)
    val expectedOntology = manager.loadOntologyFromOntologyDocument(expectation)
    input.close()
    expectation.close()
    Assert.assertTrue(ontology.getAxiomCount() < expectedOntology.getAxiomCount())
    manager.addAxioms(ontology, NegationHierarchyAsserter.assertNegationHierarchy(ontology.getAxioms.toSet))
    Assert.assertTrue(ontology.getAxiomCount() == expectedOntology.getAxiomCount())
    Assert.assertEquals(ontology.getAxioms(), expectedOntology.getAxioms())
  }

}