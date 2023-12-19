# Replay shapes abstract cognitive maps for efficient social navigation

Written by Jae-Young Son.


## Workflow 1: Run simulations (on cluster)

The simulations are time-intensive enough that they're best performed on a computing cluster. Unless you're trying to verify the reproducibility of these analyses, we recommend against re-running this workflow; pre-computed outputs from these simulations are already available in this repository.

On Brown University's SLURM-managed cluster (Oscar), here's the basic workflow for reproducing the analysis:

**Upload the following:**
1. `/data/clean-data/*`
2. `code/simulate-bfs/*`

**Run scripts**
```bash
cd ~/data/${USER}/network-navigation-replay/code/simulate-bfs
sbatch simulate_bfs_learned.sh
```


## Workflow 2: Fit model parameters (on cluster)

Likewise, parameter-fitting is time-intensive enough that we recommend against re-running this workflow; pre-computed outputs are available in this repository.

**Upload the following:**
1. `/code/fit-params/*`
2. `/code/utils/*`

**Run scripts**

Embarrassingly parallel.

```bash
cd ~/data/${USER}/network-navigation-replay/code/fit-params

### BFS-only
# Day 1, all studies
sbatch study1_D1_bfs-online.sh;
sbatch study2_D1_bfs-online.sh;
sbatch study3_D1_bfs-online.sh;
# Day 2, studies 2-3
sbatch study2_D2_bfs-online.sh;
sbatch study3_D2_bfs-online.sh;
# Day 1b, study 3
sbatch study3_D1b_bfs-online.sh;
###
### SR-only
# Day 1, all studies
sbatch study1_D1_sr-cached.sh;
sbatch study2_D1_sr-cached.sh;
sbatch study3_D1_sr-cached.sh;
# Day 2, studies 2-3
sbatch study2_D2_sr-cached.sh;
sbatch study3_D2_sr-cached.sh;
# Day 1b, study 3
sbatch study3_D1b_sr-cached.sh;
###
### Hybrid BFS-SR
# Day 1, all studies
sbatch study1_D1_hybrid-bfs-sr.sh;
sbatch study2_D1_hybrid-bfs-sr.sh;
sbatch study3_D1_hybrid-bfs-sr.sh;
# Day 2, studies 2-3
sbatch study2_D2_hybrid-bfs-sr.sh;
sbatch study3_D2_hybrid-bfs-sr.sh;
# Day 1b, study 3
sbatch study3_D1b_hybrid-bfs-sr.sh


# Wait until all jobs are finished before running next bit...
mkdir -m 775 slurm_out
mv ./*.out ./slurm_out
```


## Workflow 3: Analyses for the paper

These R Markdown files can be knit either from within RStudio, or from a terminal. You may need to install a system-level `pandoc` to knit from terminal, as it won't "know" to use the installation bundled with RStudio.

```bash
cd ~/Documents/GitHub/network-navigation-replay/code/analyses/
source ./main_results.sh
source ./supplementary_results.sh
```

