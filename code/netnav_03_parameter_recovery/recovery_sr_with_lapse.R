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

create_path <- function(this_path) {
  if (!dir.exists(this_path)) {
    dir.create(this_path, recursive = TRUE)
  }
}

save_results_to <- here("data", "param_recovery", "sr_with_lapse", "")

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

adjlist <- here("data", "clean_data", "adjlist_learned.csv") %>%
  read_csv(show_col_types = FALSE)

transmat <- adjlist %>%
  group_by(from) %>%
  mutate(edge = edge / sum(edge)) %>%
  ungroup() %>%
  pivot_wider(names_from = to, values_from = edge) %>%
  column_to_rownames("from") %>%
  as.matrix()


#### Objective function (with lapse) ####

obj_fun_sr_analytic <- function(
    param_values, param_names,
    transition_matrix, choice_data
) {
  
  #### Unpack parameters
  
  # Assign variable names/values automatically
  for (j in 1:length(param_names)) {
    assign(param_names[j], param_values[j])
  }
  
  # Bound SR discount to [0, 1)
  sr_gamma <- logistic_general(
    x = sr_gamma,
    lower_bound = 0,
    upper_bound = 0.99
  )
  
  # Bound lapse rate to [0, 1]
  lapse_rate <- logistic_standard(x = lapse_rate)
  
  
  #### Predicted representation
  
  predicted_representation <- build_successor_analytically(
    transition_matrix,
    successor_horizon = sr_gamma,
    normalize = TRUE
  )
  
  
  #### Calculate likelihood
  
  sum_neg_loglik <- choice_data %>%
    left_join(
      predicted_representation %>%
        select(endpoint_id = to, opt1_id = from, opt1_sr = sr_value),
      by = join_by(endpoint_id, opt1_id)
    ) %>%
    left_join(
      predicted_representation %>%
        select(endpoint_id = to, opt2_id = from, opt2_sr = sr_value),
      by = join_by(endpoint_id, opt2_id)
    ) %>%
    rowwise() %>%
    mutate(
      p_sub_choice = softmax(
        option_values = c(opt1_sr, opt2_sr),
        option_chosen = if_else(sub_choice == opt1_id, 1, 2),
        temperature = softmax_temperature,
        use_inverse_temperature = TRUE,
        lapse_rate = lapse_rate
      )
    ) %>%
    ungroup() %>%
    mutate(neg_ll = neg_loglik_logistic(p_sub_choice)) %>%
    summarise(sum(neg_ll)) %>%
    deframe()
  
  return(sum_neg_loglik)
}


#### Fit parameters ####

# Define params for objective function
these_params <- c("sr_gamma", "softmax_temperature", "lapse_rate")
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
            param_name == "lapse_rate" ~ logistic_standard(param_value),
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

