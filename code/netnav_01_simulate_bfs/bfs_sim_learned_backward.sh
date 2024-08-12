#! /bin/bash

#SBATCH --account=carney-ofeldman-condo
#SBATCH --mail-type=END
#SBATCH --mail-user=jae@brown.edu

#SBATCH -t 3:00:00
#SBATCH -N 1
#SBATCH --cpus-per-task 1
#SBATCH --mem-per-cpu=4gb

#SBATCH --output=bfs_sim_learned_backward.out

workflow_name="netnav_01_simulate_bfs"

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


### Run script

Rscript bfs_sim_learned_backward.R

