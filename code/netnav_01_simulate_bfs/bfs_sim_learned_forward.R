#### Setup ####

# Set meta-parameters (for local use/testing)
run_on_cluster <- TRUE

if ( run_on_cluster ) {
  n_bfs_iterations <- 5000
} else {
  n_bfs_iterations <- 5
}

# Set a random seed for reproducibility
set.seed(sum(utf8ToInt("pigeons do bread-first search")))

library(tidyverse)
library(here)
library(tidygraph)
library(tictoc)

source(here("code", "simulate_bfs", "bfs_utils.R"))


#### Load data ####

# MPT = Message-Passing Task, aka the social navigation task
mpt_data <- here("data", "clean_data", "study1_message_passing.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  filter(sub_id == 1) %>%
  arrange(startpoint_id, endpoint_id, opt1_id, opt2_id) %>%
  select(
    startpoint_id, endpoint_id,
    opt1_id, opt2_id,
    correct_choice,
    dist_opt1, dist_opt2,
    shortest_path_given_opts, shortest_path_given_start_end,
    one_impossible_option, two_correct_options
  ) %>%
  mutate(trial = row_number())

adjlist <- here("data", "clean_data", "adjlist_learned.csv") %>%
  read_csv(show_col_types = FALSE)

g <- adjlist %>%
  filter(from < to, edge == 1) %>%
  select(-edge) %>%
  tbl_graph(edges = ., directed = FALSE)


#### Simulate behavior ####

tic("BFS simulation time")
bfs_sims <- mpt_data %>%
  expand_grid(rep = 1:n_bfs_iterations, .) %>%
  group_by(rep, trial) %>%
  nest() %>%
  mutate(
    test = map(
      .x = data,
      .f = ~mpt_bfs(g, .x$startpoint_id, .x$endpoint_id, .x$opt1_id, .x$opt2_id)
    )
  ) %>%
  unnest(test) %>%
  unnest(data) %>%
  ungroup() %>%
  mutate(bfs_correct_choice = bfs_choice == correct_choice)
toc()


#### Save results ####

if (run_on_cluster) {
  if (!dir.exists(here("data", "bfs_sims"))) {
    dir.create(here("data", "bfs_sims"))
  }
  
  bfs_sims %>%
    select(-trial) %>%
    arrange(startpoint_id, endpoint_id, opt1_id, opt2_id, rep) %>%
    write_csv(here("data", "bfs_sims", "bfs_sims_learned_forward.csv"))
}

