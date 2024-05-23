#### Initialize ####

run_on_cluster <- TRUE
this_true_model <- "bfs_forward"

# Keep dependencies to a minimum
library(tidyverse)
library(here)
library(tictoc)

# Load in utils
source(here("code", "utils", "modeling_utils.R"))

create_path <- function(this_path) {
  if (!dir.exists(this_path)) {
    dir.create(this_path, recursive = TRUE)
  }
}

save_results_to <- here("data", "param-recovery", "bfs-forward-with-lapse", "")

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

bfs_forward_sims <- here(
  "data", "bfs-sims", "bfs_sims_learned.csv"
) %>%
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
  "data", "simulated-model-behaviors",
  str_c("sim_nav_", this_true_model, "_with_lapse.csv")
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


#### Objective function (with lapse) ####

obj_fun_bfs <- function(
    param_values, param_names,
    bfs_predictions, choice_data
) {
  
  #### Unpack parameters
  
  # Assign variable names/values automatically
  for (j in 1:length(param_names)) {
    assign(param_names[j], param_values[j])
  }
  
  # Bound search threshold to [0, 16]
  search_threshold <- logistic_general(
    search_threshold, lower_bound = 0, upper_bound = 16
  )
  
  # Bound lapse rate to (0, 1]
  # Need lapse rate to be technically non-zero because otherwise we run into
  # situations where we're evaluating log(0) = -Inf
  # Our proxy for 0 will be machine epsilon 2.22e-16
  lapse_rate <- logistic_general(
    lapse_rate, lower_bound = 2.22e-16, upper_bound = 1
  )
  
  #### Calculate likelihood
  
  sum_neg_loglik <- choice_data %>%
    left_join(
      bfs_predictions,
      by = join_by(shortest_path, startpoint_id, endpoint_id, opt1_id, opt2_id)
    ) %>%
    mutate(
      p_sub_choice_bfs = if_else(
        sub_choice == opt1_id,
        p_bfs_chooses_opt1,
        1 - p_bfs_chooses_opt1
      )
    ) %>%
    # What's the probability of *completing* BFS-online all the way through?
    rowwise() %>%
    mutate(
      search_threshold = .env$search_threshold,
      p_complete_bfs = softmax(
        option_values = c(search_threshold, bfs_visits),
        option_chosen = 1,
        temperature = 1
      )
    ) %>%
    ungroup() %>%
    # Weigh BFS predictions accordingly
    mutate(
      p_give_up = 1 - p_complete_bfs,
      p_sub_choice = (p_complete_bfs * p_sub_choice_bfs) + (p_give_up * 1/2),
      # Add lapse rate
      # Dividing by 2 is because there are two options to choose from
      # Therefore, when lapse rate = 1, this becomes chance = 1/2
      p_sub_choice = p_sub_choice * (1-lapse_rate) + (lapse_rate/2)
    ) %>%
    mutate(neg_ll = neg_loglik_logistic(p_sub_choice)) %>%
    summarise(sum(neg_ll)) %>%
    deframe()
  
  return(sum_neg_loglik)
}


#### Fit parameters ####

# Define params for objective function
these_params <- c("search_threshold", "lapse_rate")
this_obj_fun <- obj_fun_bfs

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
        bfs_predictions = bfs_forward_sims,
        choice_data = behavior
      ) %>%
        mutate(optimizer_run = j) %>%
        mutate(
          param_value_human_readable = case_when(
            param_name == "search_threshold" ~ logistic_general(
              param_value, lower_bound = 0, upper_bound = 16
            ),
            param_name == "lapse_rate" ~ logistic_general(
              param_value, lower_bound = 2.22e-16, upper_bound = 1
            ),
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

