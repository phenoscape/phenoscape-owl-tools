#!/bin/bash

mkdir staging
mkdir staging/sources
cd staging/sources
curl -L 'https://github.com/phenoscape/phenoscape-data/blob/master/MGI_static_data/mgi_anatomy.owl.gz?raw=true' >mgi_anatomy.owl.gz
gunzip mgi_anatomy.owl.gz
curl -L 'https://github.com/phenoscape/phenoscape-data/blob/master/MGI_static_data/mgi_expression_data.txt.gz?raw=true' >mgi_expression_data.txt.gz
gunzip mgi_expression_data.txt.gz
curl -L 'https://github.com/phenoscape/phenoscape-data/blob/master/MGI_static_data/mgi_phenotypes.txt.gz?raw=true' >mgi_phenotypes.txt.gz
gunzip mgi_phenotypes.txt.gz
curl -L 'http://www.informatics.jax.org/downloads/reports/MRK_List2.rpt' >mgi_genes.txt
curl -L 'http://zfin.org/downloads/genetic_markers.txt' >zfin_genetic_markers.txt
curl -L 'http://zfin.org/downloads/aliases.txt' >zfin_aliases.txt
curl -L 'http://zfin.org/downloads/wildtype-expression_fish.txt' >zfin_wildtype_expression.txt
curl -L 'http://zfin.org/downloads/phenoGeneCleanData_fish.txt' >zfin_phenotypes.txt
curl -L 'ftp://ftp.xenbase.org/pub/GenePageReports/GenePageGeneralInfo_ManuallyCurated.txt' >xenbase_genes.txt
curl -L 'ftp://ftp.xenbase.org/pub/GenePageReports/XenbaseGenepageToGeneIdMapping.txt' >xenbase_genepage_mappings.txt
curl -L -O 'ftp://ftp.xenbase.org/pub/GenePageReports/GeneExpression_laevis.txt'
curl -L -O 'ftp://ftp.xenbase.org/pub//GenePageReports/GeneExpression_tropicalis.txt'
curl -L http://compbio.charite.de/hudson/job/hpo.annotations.monthly/lastStableBuild/artifact/annotation/ALL_SOURCES_ALL_FREQUENCIES_genes_to_phenotype.txt >hp_phenotypes.txt

svn checkout 'https://github.com/phenoscape/phenoscape-data/trunk/xenbase-phenotypes' xenbase-phenotypes

cd ..
svn checkout 'https://github.com/phenoscape/phenoscape-data/trunk/Curation Files' nexml
