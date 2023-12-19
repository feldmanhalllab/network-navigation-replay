#!/bin/bash

#SBATCH --account=carney-ofeldman-condo
#SBATCH --mail-type=END
#SBATCH --mail-user=jae@brown.edu

#SBATCH -t 30:00
#SBATCH -N 1
#SBATCH --cpus-per-task 1
#SBATCH --mem-per-cpu=1gb

#SBATCH --output=study1_D1_hybrid-bfs-sr_sub-%a.out
#SBATCH --array=1-50

workflow_name="fit-params"

module load R/4.2.0
module load gcc/10.2 pcre2/10.35 intel/2020.2 texlive/2018 pandoc

parent_dir=$(Rscript -e "cat(here::here())")

cd ${parent_dir}/code/${workflow_name}

Rscript fit_params.R 1 D1 hybrid-bfs-sr