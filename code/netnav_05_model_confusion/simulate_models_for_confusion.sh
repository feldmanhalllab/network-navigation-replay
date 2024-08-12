#! /bin/bash

workflow_name="netnav_05_model_confusion"

### Boilerplate setup

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

output_dir=${here}/outputs
save_dir=${output_dir}/${workflow_name}

mkdir -m 775 ${output_dir}
mkdir -m 775 ${save_dir}

R -e "rmarkdown::render('simulate_models_for_confusion.Rmd', output_dir='${save_dir}')"

