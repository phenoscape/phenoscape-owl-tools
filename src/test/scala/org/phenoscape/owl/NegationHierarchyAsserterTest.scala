package org.phenoscape.owl

import org.junit.Test
import junit.framework.Assert
import org.semanticweb.owlapi.apibinding.OWLManager

class NegationHierarchyAsserterTest {

	@Test
	def testHierarchy() {
		val manager = OWLManager.createOWLOntologyManager();
		val input = getClass().getClassLoader().getResourceAsStream("NegationHierarchyAsserterTest.owl");
		val expectation = getClass().getClassLoader().getResourceAsStream("NegationHierarchyAsserterTestExpectation.owl");
		val ontology = manager.loadOntologyFromOntologyDocument(input);
		val expectedOntology = manager.loadOntologyFromOntologyDocument(expectation);
		input.close();
		expectation.close();
		Assert.assertTrue(ontology.getAxiomCount() < expectedOntology.getAxiomCount());
        NegationHierarchyAsserter.assertNegationHierarchy(ontology);
        Assert.assertTrue(ontology.getAxiomCount() == expectedOntology.getAxiomCount());
        Assert.assertEquals(ontology.getAxioms(), expectedOntology.getAxioms());
	}

}