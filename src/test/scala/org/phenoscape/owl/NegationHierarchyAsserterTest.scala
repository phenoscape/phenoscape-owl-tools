package org.phenoscape.owl

import scala.collection.JavaConversions._

import utest._
import org.semanticweb.owlapi.apibinding.OWLManager

import org.phenoscape.scowl._


object NegationHierarchyAsserterTest extends TestSuite {

  val tests = Tests {

    val base = "http://owl.phenoscape.org/NegationHierarchyAsserterTest"
    val manager = OWLManager.createOWLOntologyManager()
    val input = getClass().getClassLoader().getResourceAsStream("NegationHierarchyAsserterTest.ofn")
    val ontology = manager.loadOntologyFromOntologyDocument(input)
    input.close()

    'beforeAxioms - {
      'test1 - {
        assert(ontology.containsAxiom(Class(s"$base#B") SubClassOf Class(s"$base#A")))
      }

      'test2 - {
        assert(!(ontology.containsAxiom(Class(s"$base#NotA") SubClassOf Class(s"$base#NotB"))))
      }

      'test3 - {
        assert(ontology.containsAxiom(Class(s"$base#C") SubClassOf Class(s"$base#B")))
      }

      'test4 - {
        assert(!(ontology.containsAxiom(Class(s"$base#NotB") SubClassOf Class(s"$base#NotC"))))
      }

      'test5 - {
        assert(ontology.containsAxiom(Class(s"$base#B") EquivalentTo Class(s"$base#D")))
      }

      'test6 - {
        assert(!(ontology.containsAxiom(Class(s"$base#NotB") EquivalentTo Class(s"$base#NotD"))))
      }
    }

    manager.addAxioms(ontology, NegationHierarchyAsserter.assertNegationHierarchy(ontology.getAxioms().toSet))


    'afterAxioms - {

      'test7 - {
        assert(ontology.containsAxiom(Class(s"$base#NotA") SubClassOf Class(s"$base#NotB")))
      }

      'test8 - {
        assert(ontology.containsAxiom(Class(s"$base#NotB") SubClassOf Class(s"$base#NotC")))
      }

      'test9 - {
        assert(ontology.containsAxiom(Class(s"$base#NotB") EquivalentTo Class(s"$base#NotD")))
      }
    }
  }
}