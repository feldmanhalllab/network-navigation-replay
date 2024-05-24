#!/bin/bash

#SBATCH --account=carney-ofeldman-condo
#SBATCH --mail-type=END
#SBATCH --mail-user=jae@brown.edu

#SBATCH -t 15:00
#SBATCH -N 1
#SBATCH --cpus-per-task 1
#SBATCH --mem-per-cpu=1gb

#SBATCH --output=fit_sr_delta_rule_config_id_%a.out
#SBATCH --array=1-288

workflow_name="fit-params"


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

cd ${here}/code/${workflow_name}


### Get mappings from config
# config file SLURM task ID (from job array) into study/subject/measurement IDs

study_id=$(\
	awk -v ArrayTaskID=$SLURM_ARRAY_TASK_ID \
	'$1==ArrayTaskID {print $2}' \
	task_config.txt \
	)

sub_id=$(\
	awk -v ArrayTaskID=$SLURM_ARRAY_TASK_ID \
	'$1==ArrayTaskID {print $3}' \
	task_config.txt \
	)

measurement_id=$(\
	awk -v ArrayTaskID=$SLURM_ARRAY_TASK_ID \
	'$1==ArrayTaskID {print $4}' \
	task_config.txt \
	)

Rscript fit_sr_delta_rule.R $study_id $sub_id $measurement_id

