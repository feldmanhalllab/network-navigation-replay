#!/bin/bash

#SBATCH --account=carney-ofeldman-condo
#SBATCH --mail-type=END
#SBATCH --mail-user=jae@brown.edu

#SBATCH -t 1:00
#SBATCH -N 1
#SBATCH --cpus-per-task 1
#SBATCH --mem-per-cpu=1gb

#SBATCH --output=fit_model_bfs_forward_true_model_bfs_forward_sub_%a.out
#SBATCH --array=1-500

workflow_name="netnav_05_model_confusion"


### Boilerplate setup

module load r/4.4.0

start_dir=$(pwd)
home_dir=$HOME

while :
do
	current_dir=$(pwd)
	if [[ "$current_dir" == "$home_dir" ]]; then
		echo "Failed to find .here"
		cd $start_dir
		break
	fi

	if test -f ".here"; then
		here=$(realpath ${current_dir})
		echo ".here found at $here"
		cd $start_dir
		break
	else
		cd ../
	fi
done


### Run script

cd ${here}/code/${workflow_name}/fitting_scripts/
Rscript confusion_bfs_forward.R bfs_forward
