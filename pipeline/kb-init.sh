#!/bin/bash
#
#SBATCH --error=error
#SBATCH --mail-user=balhoff@renci.org
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=20
#SBATCH --mem=75G

set -e # Abort if any command fails

export JAVA_OPTS="-Xmx70G"
export TARGET=/scratch/balhoff/phenoscape-kb

$HOME/phenoscape-owl-tools/get_sources.sh
$HOME/phenoscape-owl-tools/target/universal/stage/bin/kb-owl-tools build-kb $TARGET $HOME/phenoscape-owl-tools/blazegraph.properties
