#### Initialize ####

run_on_cluster <- TRUE

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

save_results_to <- here("data", "param_fits", "ideal_obs", "")

if (run_on_cluster) {
  # Get args from shell script and SLURM environment
  this_study <- commandArgs(trailingOnly = TRUE)[1]
  this_sub <- commandArgs(trailingOnly = TRUE)[2]
  this_measurement <- commandArgs(trailingOnly = TRUE)[3]
  
  # Meta-parameters
  this_many_runs <- 25
  this_many_iter_per_run <- 5000
  
  # Create all needed directories
  create_path(save_results_to)
} else {
  # For local testing/debugging
  this_study <- 3
  this_sub <- 13
  this_measurement <- "D1"
  
  this_many_runs <- 2
  this_many_iter_per_run <- 1000
}


#### Load/tidy data ####

behavior <- here(
  "data", "clean_data", str_c("study", this_study, "_message_passing.csv")
) %>%
  read_csv(show_col_types = FALSE) %>%
  filter(
    two_correct_options == FALSE,
    shortest_path_given_opts == shortest_path_given_start_end
  ) %>%
  mutate(
    shortest_path = factor(shortest_path_given_opts),
    measurement_id = case_when(
      network == "reevaluated" ~ "D2b",
      measurement_id == 1 ~ "D1",
      measurement_id == 2 & .env$this_study == 2 ~ "D2",
      measurement_id == 2 & .env$this_study == 3 ~ "D1b",
      measurement_id == 3 & .env$this_study == 3 ~ "D2"
    )
  ) %>%
  select(
    sub_id, measurement_id, shortest_path,
    startpoint_id, endpoint_id,
    opt1_id, opt2_id,
    correct_choice, sub_choice,
    correct, rt,
    opt1_distance = dist_opt1,
    opt2_distance = dist_opt2
  ) %>%
  filter(
    sub_id == .env$this_sub,
    measurement_id == .env$this_measurement
  ) %>%
  # Replace undefined distances (corresponding to impossible options)
  # so that the softmax gets non-NA inputs; we assume that impossible
  # options are just as bad as the longest distance found in this set
  # of trials, i.e., a distance of 8
  mutate(across(c(opt1_distance, opt2_distance), ~replace_na(.x, 8)))


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
        "fit_model_ideal_obs_",
        "study", this_study, "_",
        this_measurement, "_",
        "sub_", this_sub,
        ".csv"
      )
    )
}

