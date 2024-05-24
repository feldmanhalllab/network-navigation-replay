#### Initialize ####

run_on_cluster <- TRUE

# Keep dependencies to a minimum
library(tidyverse)
library(here)
library(tictoc)

# Load in utils
source(here("code", "utils", "modeling_utils.R"))
source(here("code", "fit-params", "objective_functions.R"))

create_path <- function(this_path) {
  if (!dir.exists(this_path)) {
    dir.create(this_path, recursive = TRUE)
  }
}

save_results_to <- here("data", "param-fits", "bfs-forward", "")

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

bfs_forward_sims <- here("data", "bfs-sims", "bfs_sims_learned.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  filter(
    shortest_path_given_opts == shortest_path_given_start_end,
    two_correct_options == FALSE
  ) %>%
  mutate(shortest_path = factor(shortest_path_given_opts)) %>%
  select(-starts_with("shortest_path_given"), -two_correct_options) %>%
  group_by(shortest_path, startpoint_id, endpoint_id, opt1_id, opt2_id) %>%
  summarise(
    p_bfs_chooses_opt1 = mean(bfs_choice == opt1_id),
    bfs_visits = mean(bfs_n_visits_total),
    .groups = "drop"
  )

behavior <- here(
  "data", "clean-data", str_c("study", this_study, "_message_passing.csv")
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
  )


#### Fit parameters ####

# Define params for objective function
these_params <- "search_threshold"
this_obj_fun <- obj_fun_bfs

tic("Total model-fitting time")

out <- run_optim(
  max_iter_per_run = this_many_iter_per_run,
  objective_function = this_obj_fun,
  param_guesses = runif(length(these_params), -10, 10),
  param_names = these_params,
  # Supply arguments to objective function
  bfs_predictions = bfs_forward_sims,
  choice_data = behavior,
  # Additional arguments because this is a 1-dim optimization problem
  method = "Brent",
  lower = 0, upper = 16
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
        "fit_model_bfs_forward_",
        "study", this_study, "_",
        this_measurement, "_",
        "sub_", this_sub,
        ".csv"
      )
    )
}

