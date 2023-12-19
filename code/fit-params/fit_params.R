#### Initialize ####

# Set meta-parameters (for local use/testing)
run_on_cluster <- TRUE
this_study <- 3
this_measurement <- "D1"
# this_model <- "bfs-online"
this_model <- "sr-cached"
# this_model <- "hybrid-bfs-sr"
this_many_runs <- 2
this_many_iter_per_run <- 1000
this_sub <- 13  # Just for testing

# Keep dependencies to a minimum
library(tidyverse)
library(here)
library(tictoc)

# If running on the cluster, overwrite all the meta-parameters
if ( run_on_cluster ) {
  # What's the objective function for the model we're estimating?
  # Get arg from shell script
  this_study <- commandArgs(trailingOnly = TRUE)[1]
  this_measurement <- commandArgs(trailingOnly = TRUE)[2]
  this_model <- commandArgs(trailingOnly = TRUE)[3]
  
  # Use SLURM array to parallelize model-fitting across jobs
  this_sub <- Sys.getenv("SLURM_ARRAY_TASK_ID")[1]
  
  # Meta-parameters
  this_many_runs <- 25
  this_many_iter_per_run <- 5000
}

# Create all needed directories
scratch_dir <- "/oscar/scratch/json11/param-fits"
save_dir <- here("data", "param-fits")

if (run_on_cluster) {
  if (dir.exists(scratch_dir) == FALSE) {
    dir.create(scratch_dir)
  }
  
  if (!dir.exists(save_dir)) {
    dir.create(save_dir)
  }
}

# Load in utils
source(here("code", "utils", "modeling_utils.R"))
source(here("code", "utils", "representation_utils.R"))


#### Load/tidy data ####

online_bfs <- here("data", "bfs-sims", "bfs_sims_learned.csv") %>%
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
    correct, rt
  ) %>%
  filter(
    sub_id == .env$this_sub,
    measurement_id == .env$this_measurement
  )

sr_obs <- here("data", "clean-data", "adjlist_learned.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  filter(edge == 1) %>%
  select(from, to) %>%
  # ~15 minutes of replay
  expand_grid(iteration = 1:115, .) %>%
  group_by(iteration) %>%
  slice_sample(prop = 1) %>%
  ungroup()


#### Fit parameters ####

# Define params for objective function
if (this_model == "bfs-online") {
  these_params <- c("search_threshold", "lapse_rate")
} else if (this_model == "sr-cached") {
  these_params <- c("sr_gamma", "softmax_temperature", "lapse_rate")
} else if (this_model == "hybrid-bfs-sr") {
  these_params <- c(
    "search_threshold", "sr_gamma", "softmax_temperature"
  )
} else {
  stop("Invalid value of `this_model`")
}

# Load objective function
source(here("code", "fit-params", "obj_fun.R"))

tictoc::tic("Total model-fitting time")

for (j in 1:this_many_runs) {
  print(str_c("Starting run ", j))
  tictoc::tic(str_c("Run ", j, " finished"))
  
  tryCatch(
    expr = {
      this_run <- run_optim(
        max_iter_per_run = this_many_iter_per_run,
        objective_function = this_obj_fun,
        param_guesses = runif(length(these_params), -1, 1),
        param_names = these_params,
        # Supply arguments to objective function
        bfs_predictions = online_bfs,
        sr_obs = sr_obs,
        choice_data = behavior
      ) %>%
        mutate(optimizer_run = j) %>%
        mutate(
          param_value_human_readable = case_when(
            param_name == "search_threshold" ~ exp(param_value),
            param_name == "lapse_rate" ~ logistic_standard(param_value),
            param_name == "sr_gamma" ~ logistic_general(param_value, 0, 0.99)
            TRUE ~ param_value
          )
        ) %>%
        rename(param_value_raw = param_value)
      
      if (j == 1) {
        out <- this_run
      } else {
        out <- bind_rows(out, this_run)
      }
    },
    error = function(e) {
      warning(str_c("tryCatch error caught on run "), j)
    }
  )
  
  tictoc::toc()
  
  # Save to scratch instead of constantly writing to data
  if (run_on_cluster) {
    out %>%
      write_csv(
        str_c(
          scratch_dir,
          "/",
          "study", this_study, "_",
          this_measurement, "_",
          this_model, "_",
          "sub-", this_sub, ".csv"
        )
      )
  }
}

# Save final model fits
if (run_on_cluster) {
  out %>%
    write_csv(
      str_c(
        save_dir,
        "/",
        "study", this_study, "_",
        this_measurement, "_",
        this_model, "_",
        "sub-", this_sub, ".csv"
      )
    )
}

tictoc::toc()
