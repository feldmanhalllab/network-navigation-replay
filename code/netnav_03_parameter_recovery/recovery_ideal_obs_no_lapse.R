#### Initialize ####

run_on_cluster <- TRUE
this_true_model <- "ideal_obs"

# Keep dependencies to a minimum
library(tidyverse)
library(here)
library(tictoc)

# Load in utils
source(here("code", "utils", "modeling_utils.R"))
source(here("code", "netnav_04_fit_params", "objective_functions.R"))

create_path <- function(this_path) {
  if (!dir.exists(this_path)) {
    dir.create(this_path, recursive = TRUE)
  }
}

save_results_to <- here("data", "param_recovery", "ideal_obs_no_lapse", "")

if (run_on_cluster) {
  # Get args from shell script and SLURM environment
  this_sub <- Sys.getenv("SLURM_ARRAY_TASK_ID")[1]
  
  # Meta-parameters
  this_many_runs <- 25
  this_many_iter_per_run <- 5000
  
  # Create all needed directories
  create_path(save_results_to)
} else {
  # For local testing/debugging
  this_sub <- 1
  
  this_many_runs <- 2
  this_many_iter_per_run <- 1000
}


#### Load/tidy data ####

behavior <- here(
  "data", "simulated_model_behaviors",
  str_c("sim_nav_", this_true_model, "_no_lapse.csv")
) %>%
  read_csv(show_col_types = FALSE) %>%
  filter(sub_id == this_sub) %>%
  select(
    sub_id, shortest_path,
    startpoint_id, endpoint_id,
    opt1_id, opt2_id,
    correct_choice,
    sub_choice = simulated_choice,
    opt1_distance, opt2_distance
  ) %>%
  mutate(shortest_path = factor(shortest_path))


#### Fit parameters ####

# Define params for objective function
these_params <- "softmax_temperature"
this_obj_fun <- obj_fun_ideal_obs

tic("Total model-fitting time")

out <- run_optim(
  max_iter_per_run = this_many_iter_per_run,
  objective_function = this_obj_fun,
  param_guesses = runif(length(these_params), -10, 10),
  param_names = these_params,
  # Supply arguments to objective function
  choice_data = behavior,
  # Additional arguments because this is a 1-dim optimization problem
  method = "Brent",
  lower = -5, upper = 0
)

toc()

# Print out warnings...
warnings()


#### Save model fits ####
if (run_on_cluster) {
  out %>%
    write_csv(
      str_c(
        save_results_to,
        "recovery_", this_true_model, "_",
        "sub_", this_sub, ".csv"
      )
    )
}

