#!/bin/bash                                                                                                                                       

step1=`sbatch kb-init.sh |  cut -d ' ' -f 4`

step2=`sbatch --dependency=afterok:$step1 kb-owlsim-taxa.sh | cut -d ' ' -f 4`

step3=`sbatch --dependency=afterok:$step1 kb-owlsim-genes.sh | cut -d ' ' -f 4`

step4=`sbatch --dependency=afterok:$step2:$step3 kb-similarity.sh | cut -d ' ' -f 4`
