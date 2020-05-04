package org.phenoscape.owl.build

import com.bigdata.rdf.store.DataLoader
import org.phenoscape.owl._

object Command extends App {

  val commandArgs = args.drop(1)
  args(0) match {
    case "build-kb" => PhenoscapeKB.main(commandArgs)
    case "pairwise-sim" => RunPairwiseOWLSim.main(commandArgs)
    case "load-triples" => DataLoader.main(commandArgs)
    case "output-ics" => ComputeICs.main(commandArgs)
    case "output-profile-sizes" => PrintTaxonAndGeneProfileSizes.main(commandArgs)
    case "expects-to-triples" => ConvertScoresToTriples.main(commandArgs)
    case "sparql-select" => RunSPARQLQuery.main(commandArgs)
    case "sparql-construct" => RunSPARQLConstruct.main(commandArgs)
    case "sparql-update" => RunSPARQLUpdate.main(commandArgs)
    case "convert-nexml" => ConvertNeXML.main(commandArgs)
    case "assert-negation-hierarchy" => NegationHierarchyAsserter.main(commandArgs)
    case "output-evolutionary-profiles" => EvolutionaryProfiles.main(commandArgs)

    case "homology-table-to-owl" =>
      val homologyArgs = commandArgs.drop(1)
      commandArgs(0).toLowerCase match {
        case "rea" => HomologyTableWithTaxa.main(homologyArgs)
        case "ava" => HomologyTableToOWLVAHM.main(homologyArgs)
        case "rolification" => HomologyTableDemoRolification.main(homologyArgs)
      }
    case _ => {
      println(
        """
Valid commands are:

build-kb
pairwise-sim
load-triples
output-ics
output-profile-sizes
expects-to-triples
sparql-select
sparql-construct
sparql-update
convert-nexml
assert-negation-hierarchy
output-evolutionary-profiles
homology-table-to-owl

Consult the source code for required parameters.
      """
      )

      System.exit(1)
    }
  }

}
