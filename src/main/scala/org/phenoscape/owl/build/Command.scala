package org.phenoscape.owl.build

import com.bigdata.rdf.store.DataLoader
import org.phenoscape.owl.{EvolutionaryProfiles, NegationHierarchyAsserter, TaxonNode}
import org.semanticweb.owlapi.model.OWLOntology

object Command extends App {

  val commandArgs = args.drop(1)
  args(0) match {
    case "build-kb"             => PhenoscapeKB.main(commandArgs)
    case "pairwise-sim"         => RunPairwiseOWLSim.main(commandArgs)
    case "load-triples"         => DataLoader.main(commandArgs)
    case "output-ics"           => ComputeICs.main(commandArgs)
    //case "output-match-scores"  => //requires sparql query to DB and output tab-delimited; just run sparql query
    case "output-profile-sizes" => PrintTaxonAndGeneProfileSizes.main(commandArgs)
    //case "compute-expects"      => //run just run python directly
    case "expects-to-triples"   => ConvertScoresToTriples.main(commandArgs)
    case "sparql-select"        => RunSPARQLQuery.main(commandArgs)
    case "sparql-construct"     => RunSPARQLConstruct.main(commandArgs)
    case "sparql-update"        => RunSPARQLUpdate.main(commandArgs)
    case "convert-nexml"        => ConvertNeXML.main(commandArgs)
    case "assert-negation-hierarchy" => NegationHierarchyAsserter.main(commandArgs)
    case "output-evolutionary-profiles" => EvolutionaryProfiles.main(commandArgs)
    case _ => println("""
Valid commands are:

build-kb
pairwise-sim
load-triples
output-ics
output-profile-sizes
expects-to-triples
sparql-select

Consult the source code for required parameters.
      """)
  }

}