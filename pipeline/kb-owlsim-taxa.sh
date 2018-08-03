#!/bin/bash
#
#SBATCH --output=output_%a
#SBATCH --error=error_%a
#SBATCH --mail-user=balhoff@renci.org
#SBATCH --mail-type=FAIL
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=3750
#SBATCH --array=1-100

TARGET=/scratch/balhoff/phenoscape-kb
OUTPUT=$TARGET/owlsim-taxa
mkdir $OUTPUT
cd $OUTPUT

export JAVA_OPTS="-Xmx25G"
$HOME/phenoscape-owl-tools/target/universal/stage/bin/kb-owl-tools pairwise-sim 100 $SLURM_ARRAY_TASK_ID $TARGET/kb/tbox-hierarchy-only.owl $TARGET/kb/profiles.ttl taxa
