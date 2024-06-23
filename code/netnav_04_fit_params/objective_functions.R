#### Breadth-First Search ####
# Works for either the forward or backward direction

obj_fun_bfs <- function(
    param_values, param_names,
    bfs_predictions, choice_data
) {
  
  #### Unpack parameters
  
  # Assign variable names/values automatically
  for (j in 1:length(param_names)) {
    assign(param_names[j], param_values[j])
  }
  
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
      # To prevent log(0) = -Inf from showing up in the likelihoods, replace
      # zeroes with the smallest non-zero value a computer can represent, aka
      # machine-epsilon = 2.22e-16.  This is conceptually similar to estimating
      # a small constant lapse rate, but without introducing any bias to
      # nonzero probabilities.
      p_sub_choice = if_else(p_sub_choice < 2.22e-16, 2.22e-16, p_sub_choice)
    ) %>%
    mutate(neg_ll = neg_loglik_logistic(p_sub_choice)) %>%
    summarise(sum(neg_ll)) %>%
    deframe()
  
  return(sum_neg_loglik)
}


#### Ideal observer ####

obj_fun_ideal_obs <- function(param_values, param_names, choice_data) {
  
  #### Unpack parameters
  
  # Assign variable names/values automatically
  for (j in 1:length(param_names)) {
    assign(param_names[j], param_values[j])
  }
  
  #### Calculate likelihood
  sum_neg_loglik <- choice_data %>%
    rowwise() %>%
    mutate(
      p_sub_choice = softmax(
        option_values = c(opt1_distance, opt2_distance),
        option_chosen = if_else(sub_choice == opt1_id, 1, 2),
        temperature = softmax_temperature,
        use_inverse_temperature = TRUE
      )
    ) %>%
    ungroup() %>%
    mutate(neg_ll = neg_loglik_logistic(p_sub_choice)) %>%
    summarise(sum(neg_ll)) %>%
    deframe()
  
  return(sum_neg_loglik)
}


#### SR (analytic) ####

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
        use_inverse_temperature = TRUE
      )
    ) %>%
    ungroup() %>%
    mutate(neg_ll = neg_loglik_logistic(p_sub_choice)) %>%
    summarise(sum(neg_ll)) %>%
    deframe()
  
  return(sum_neg_loglik)
}


#### SR (delta-rule) ####

obj_fun_sr_delta_rule <- function(
    param_values, param_names,
    learning_obs, choice_data
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
  
  
  #### Predicted representation
  
  predicted_representation <- build_successor_td_0(
    # NOTE: 13 is hard-coding the number of network members
    successor_matrix = diag(nrow = 13, ncol = 13),
    observation_matrix = learning_obs,
    sr_alpha = 0.1,
    sr_gamma = sr_gamma,
    bidirectional = TRUE
  ) %>%
    # Normalize count matrix into probability matrix
    mutate(sr_value = sr_value * (1 - sr_gamma))
  
  
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
        use_inverse_temperature = TRUE
      )
    ) %>%
    ungroup() %>%
    mutate(neg_ll = neg_loglik_logistic(p_sub_choice)) %>%
    summarise(sum(neg_ll)) %>%
    deframe()
  
  return(sum_neg_loglik)
}

