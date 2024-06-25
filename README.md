# Replay shapes abstract cognitive maps for efficient social navigation

Written by Jae-Young Son.

R Markdown files can be knit either from within RStudio, or from a terminal. You may need to install a system-level `pandoc` to knit from terminal, as it won't "know" to use the installation bundled with RStudio.


## Workflow 0: Data-cleaning (local)

A script walking through how data were cleaned, included for archival purposes + transparency.

**Run script(s):**
```bash
cd ~/Documents/GitHub/network-navigation-replay/code/netnav_00_data_cleaning
source ./data_cleaning.sh
```


## Workflow 1: Simulate BFS (cluster)

In our use of BFS as a psychologically-plausible model-based planning algorithm, we're interested in knowing what choices a BFS-agent would make if performing two parallel searches in the "forward" direction, as well as the expected number of nodes a BFS-agent (in either the forward or backward direction) would have to search before making a choice.

The simulations are time-intensive enough that they're best performed on a computing cluster. Unless you're trying to verify the reproducibility of these analyses, we recommend against re-running this workflow; pre-computed outputs from these simulations are already available in this repository.

On Brown University's SLURM-managed cluster (Oscar), here's the basic workflow for reproducing the analysis:

**Upload the following:**
1. `/data/clean_data/*`
2. `code/netnav_01_simulate_bfs/*`

**Run script(s):**
```bash
cd ~/data/${USER}/network-navigation-replay/code/netnav_01_simulate_bfs
sbatch bfs_sim_learned_forward.sh
sbatch bfs_sim_learned_backward.sh
sbatch bfs_sim_reevaluated_forward.sh
sbatch bfs_sim_reevaluated_backward.sh
```

**Download:**
1. `/data/bfs_sims/*`


## Workflow 2: Simulate models (local)

Before doing parameter recovery on any of our models, it'd be useful to know what parameter ranges are worth testing. These simulations are relatively lightweight, so they were run locally.

**Run script(s):**
```bash
cd ~/Documents/GitHub/network-navigation-replay/code/netnav_02_simulate_model_behaviors
source ./simulate_models.sh
```


## Workflow 3: Parameter recovery (local + cluster)

### On the cluster

The parameter estimation scripts need to be run on the cluster:

**Upload the following:**
1. `/data/simulated_model_behaviors/*`
2. `/code/netnav_03_parameter_recovery/*`
3. `/code/utils/*`
4. `/code/netnav_04_fit_params/objective_functions.R`

**Run script(s):**
```bash
cd ~/data/${USER}/network-navigation-replay/code/netnav_03_parameter_recovery
sbatch recovery_bfs_backward_no_lapse.sh
sbatch recovery_bfs_forward_no_lapse.sh
sbatch recovery_bfs_forward_with_lapse.sh
sbatch recovery_ideal_obs_no_lapse.sh
sbatch recovery_sr_no_lapse.sh
sbatch recovery_sr_with_lapse.sh
```

**Download:**
1. `/data/param_recovery/*`

### Locally

The subsequent analysis was performed locally:

```bash
cd ~/Documents/GitHub/network-navigation-replay/code/netnav_03_parameter_recovery
source ./parameter_recovery.sh
```


## Workflow 4: Parameter-fitting (cluster)

**Upload the following:**
1. `/code/netnav_04_fit_params/*`

**Run script(s):**
```bash
cd ~/data/${USER}/network-navigation-replay/code/netnav_04_fit_params
sbatch fit_bfs_backward.sh
sbatch fit_bfs_forward.sh
sbatch fit_ideal_obs.sh
sbatch fit_sr_analytic.sh
sbatch fit_sr_delta_rule.sh
```

**Download:**
1. `/data/param_fits/*`


## Workflow 5: Model confusion (local + cluster)

### Locally

In the model confusion analysis, we'll first take a look at our actual distribution of estimated parameters, then simulate new data from the models using parameter distributions that more closely match the empirically-observed distributions.

```bash
cd ~/Documents/GitHub/network-navigation-replay/code/netnav_05_model_confusion
source ./simulate_models_for_confusion.sh
```

### On the cluster

Using the newly-simulated behaviors, we'll then fit every model on every dataset.

**Upload the following:**
1. `/data/simulated_model_behaviors/sim_nav_*_confusion.csv`
2. `/code/netnav_05_model_confusion/fitting_scripts/*`
3. `/code/netnav_05_model_confusion/shell_scripts/*`

**Run script(s):**
```bash
# Note: due to "max CPUs per user" limits, can't batch all of these at once!

cd ~/data/${USER}/network-navigation-replay/code/netnav_05_model_confusion/shell_scripts

# Fit BFS-backward
sbatch fit_bfs_backward_true_model_bfs_backward.sh
sbatch fit_bfs_backward_true_model_bfs_forward.sh
sbatch fit_bfs_backward_true_model_ideal_obs.sh
sbatch fit_bfs_backward_true_model_sr.sh

# Fit BFS-forward
sbatch fit_bfs_forward_true_model_bfs_backward.sh
sbatch fit_bfs_forward_true_model_bfs_forward.sh
sbatch fit_bfs_forward_true_model_ideal_obs.sh
sbatch fit_bfs_forward_true_model_sr.sh

# Fit ideal observer
sbatch fit_ideal_obs_true_model_bfs_backward.sh
sbatch fit_ideal_obs_true_model_bfs_forward.sh
sbatch fit_ideal_obs_true_model_ideal_obs.sh
sbatch fit_ideal_obs_true_model_sr.sh

# Fit SR
sbatch fit_sr_true_model_bfs_backward.sh
sbatch fit_sr_true_model_bfs_forward.sh
sbatch fit_sr_true_model_ideal_obs.sh
sbatch fit_sr_true_model_sr.sh
```

### Locally, again

Finally, we'll compute the probability that the model-fitting process leads us to conclude that model Y is the best-fitting model for some set of agents, given that we know that model X is the true data-generating mechanism.

```bash
cd ~/Documents/GitHub/network-navigation-replay/code/netnav_05_model_confusion
source ./analyze_model_confusion.sh
```


## Workflow 6: Model comparison/selection (local)

We want to compare how well the models fit the data, and select a "best-fitting" model to interpret.

```bash
cd ~/Documents/GitHub/network-navigation-replay/code/netnav_06_model_comparison
source ./model_comparison.sh
```


## Workflow 7: Analyze behavior (local)

Here, we perform statistical tests for all behavioral analyses, except those involving transition reevaluation. We also formally test whether changes in behavior are related to changes in model parameters.

```bash
cd ~/Documents/GitHub/network-navigation-replay/code/netnav_07_behavioral_analyses
source ./analyze_behavior.sh
```


## Workflow 8: Transition reevaluation (local)

Here, we perform all analyses related to transition reevaluation.

```bash
cd ~/Documents/GitHub/network-navigation-replay/code/netnav_08_transition_reevaluation
source ./transition_reevaluation.sh
```


## Workflow 99: Figures for the main text

Finally, we create figures that are as close to being publication-ready as we can make them.

```bash
cd ~/Documents/GitHub/network-navigation-replay/code/netnav_99_figures_for_main_text
source ./figures.sh
```

