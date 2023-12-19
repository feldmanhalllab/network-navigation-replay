#! /bin/bash

#SBATCH --account=carney-ofeldman-condo
#SBATCH --mail-type=END
#SBATCH --mail-user=jae@brown.edu

#SBATCH -t 3:00:00
#SBATCH -N 1
#SBATCH --cpus-per-task 1
#SBATCH --mem-per-cpu=4gb

#SBATCH --output=bfs-sim-learned.out

module load R/4.2.0
module load gcc/10.2 pcre2/10.35 intel/2020.2 texlive/2018 pandoc

workflow_name="simulate-bfs"

parent_dir=$(Rscript -e "cat(here::here())")
output_dir=${parent_dir}/outputs
# save_dir=${output_dir}/${workflow_name}

mkdir -m 775 ${output_dir} || echo "Output directory already exists"
# mkdir -m 775 ${save_dir} || echo "Save directory already exists"

cd ${parent_dir}/code/${workflow_name}

Rscript bfs_sim_learned.R