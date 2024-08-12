#### Initialize ####

run_on_cluster <- TRUE
this_true_model <- "sr"

# Keep dependencies to a minimum
library(tidyverse)
library(here)
library(tictoc)

# Load in utils
source(here("code", "utils", "representation_utils.R"))
source(here("code", "utils", "modeling_utils.R"))
source(here("code", "netnav_04_fit_params", "objective_functions.R"))

create_path <- function(this_path) {
  if (!dir.exists(this_path)) {
    dir.create(this_path, recursive = TRUE)
  }
}

save_results_to <- here("data", "param_recovery", "sr_no_lapse", "")

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
    sub_choice = simulated_choice
  ) %>%
  mutate(shortest_path = factor(shortest_path))

adjlist <- here("data", "clean_data", "adjlist_learned.csv") %>%
  read_csv(show_col_types = FALSE)

transmat <- adjlist %>%
  group_by(from) %>%
  mutate(edge = edge / sum(edge)) %>%
  ungroup() %>%
  pivot_wider(names_from = to, values_from = edge) %>%
  column_to_rownames("from") %>%
  as.matrix()


#### Fit parameters ####

# Define params for objective function
these_params <- c("sr_gamma", "softmax_temperature")
this_obj_fun <- obj_fun_sr_analytic

tic("Total model-fitting time")

for (j in 1:this_many_runs) {
  print(str_c("Starting run ", j))
  tic(str_c("Run ", j, " finished"))
  
  tryCatch(
    expr = {
      this_run <- run_optim(
        max_iter_per_run = this_many_iter_per_run,
        objective_function = this_obj_fun,
        param_guesses = runif(length(these_params), -10, 10),
        param_names = these_params,
        # Supply arguments to objective function
        transition_matrix = transmat,
        choice_data = behavior
      ) %>%
        mutate(optimizer_run = j) %>%
        mutate(
          param_value_human_readable = case_when(
            param_name == "sr_gamma" ~ logistic_general(param_value, 0, 0.99),
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
      warning("tryCatch error caught on run ", j, ": ", e)
    }
  )
  
  toc()
}

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

