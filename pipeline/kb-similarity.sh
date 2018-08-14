#!/bin/bash
#
#SBATCH --output=output-all
#SBATCH --error=error-all
#SBATCH --mail-user=balhoff@renci.org
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=20
#SBATCH --mem=75G

set -e # Abort if any command fails
source $HOME/pkb-py/bin/activate # this sets up python environment with needed libraries
export JAVA_OPTS="-Xmx70G"

TARGET=/scratch/balhoff/phenoscape-kb
PSOT=$HOME/phenoscape-owl-tools
SPARQL=$PSOT/src/main/sparql
kbot=$PSOT/target/universal/stage/bin/kb-owl-tools
bgrunner=$HOME/blazegraph-runner/target/universal/stage/bin/blazegraph-runner

DB_FILE=$TARGET/blazegraph-loaded-all.jnl

echo "Copying database"
cp $TARGET/blazegraph.jnl $DB_FILE

echo "Querying subclass closure."
$kbot sparql-construct $PSOT/blazegraph.properties $DB_FILE $SPARQL/subclass_closure_construct.rq $TARGET/subclass_closure.ttl 
echo "Querying profile instance closure."
$kbot sparql-construct $PSOT/blazegraph.properties $DB_FILE $SPARQL/profile_instance_closure_construct.rq $TARGET/instance_closure.ttl

echo "Loading subclass closure."
$bgrunner load --informat=turtle --journal=$DB_FILE --properties=$PSOT/blazegraph.properties --graph="http://kb.phenoscape.org/closure" $TARGET/subclass_closure.ttl
echo "Loading instance closure."
$bgrunner load --informat=turtle --journal=$DB_FILE --properties=$PSOT/blazegraph.properties --graph="http://kb.phenoscape.org/closure" $TARGET/instance_closure.ttl

echo "Loading owlsim-taxa"
$bgrunner load --informat=turtle --journal=$DB_FILE --properties=$PSOT/blazegraph.properties --graph="http://kb.phenoscape.org/sim/taxa" $TARGET/owlsim-taxa
echo "Loading owlsim-genes"
$bgrunner load --informat=turtle --journal=$DB_FILE --properties=$PSOT/blazegraph.properties --graph="http://kb.phenoscape.org/sim/genes" $TARGET/owlsim-genes

echo "Outputting ICs"
$kbot output-ics $TARGET/kb/tbox-hierarchy-only.owl $TARGET/kb/profiles.ttl taxa $TARGET/corpus_ics-taxa.ttl
$kbot output-ics $TARGET/kb/tbox-hierarchy-only.owl $TARGET/kb/profiles.ttl genes $TARGET/corpus_ics-genes.ttl

$bgrunner load --informat=turtle --journal=$DB_FILE --properties=$PSOT/blazegraph.properties --graph="http://kb.phenoscape.org/sim/taxa" $TARGET/corpus_ics-taxa.ttl
$bgrunner load --informat=turtle --journal=$DB_FILE --properties=$PSOT/blazegraph.properties --graph="http://kb.phenoscape.org/sim/genes" $TARGET/corpus_ics-genes.ttl

$kbot output-profile-sizes $TARGET/kb/tbox-hierarchy-only.owl $TARGET/kb/profiles.ttl $TARGET/ProfileSizes.txt

mkdir $TARGET/taxa-regression
cp $TARGET/ProfileSizes.txt $TARGET/taxa-regression/
cd $TARGET/taxa-regression
echo "Running and loading taxa regression"
$kbot sparql-select $PSOT/blazegraph.properties $DB_FILE $PSOT/src/getscores-taxa-corpus.rq Scores.tsv
python $PSOT/src/regression.py `grep 'VTO_' ProfileSizes.txt | wc -l`
$kbot expects-to-triples RankStatistics.txt taxa-expect-scores.ttl
$bgrunner load --informat=turtle --journal=$DB_FILE --properties=$PSOT/blazegraph.properties --graph="http://kb.phenoscape.org/sim/taxa" taxa-expect-scores.ttl

mkdir $TARGET/genes-regression
cp $TARGET/ProfileSizes.txt $TARGET/genes-regression/
cd $TARGET/genes-regression
echo "Running and loading genes regression"
$kbot sparql-select $PSOT/blazegraph.properties $DB_FILE $PSOT/src/getscores-genes-corpus.rq Scores.tsv
python $PSOT/src/regression.py `grep -v 'VTO_' ProfileSizes.txt | wc -l`
$kbot expects-to-triples RankStatistics.txt genes-expect-scores.ttl
$bgrunner load --informat=turtle --journal=$DB_FILE --properties=$PSOT/blazegraph.properties --graph="http://kb.phenoscape.org/sim/genes" genes-expect-scores.ttl
