this_obj_fun <- function(
    param_values, param_names,
    bfs_predictions, sr_obs, choice_data
) {
  #### Unpack parameters ####
  
  # Assign variable names/values automatically
  for (j in 1:length(param_names)) {
    assign(param_names[j], param_values[j])
  }
  
  # Bound SR gamma to [0, 1)
  if ("sr_gamma" %in% param_names) {
    sr_gamma <- logistic_general(
      x = sr_gamma,
      lower_bound = 0,
      # SR becomes undefined when gamma = 1
      upper_bound = 0.99
    )
  }
  
  # Bound search threshold to [0, Inf]
  if ("search_threshold" %in% param_names) {
    search_threshold <- exp(search_threshold)
  }
  
  # Bound lapse rate to [0, 1]
  if ("lapse_rate" %in% param_names) {
    lapse_rate <- logistic_standard(lapse_rate)
  }
  
  #### Build SR ####
  
  if ("sr_gamma" %in% param_names) {
    sr_rep <- build_rep_sr(
      learning_data = sr_obs,
      # Assume learning rate instead of estimating
      this_alpha = 0.1,
      this_gamma = sr_gamma
    )
  }
  
  #### Calculate likelihood ####
  
  sr_scaling_constant <- 100
  
  # We'll always need the choice data
  eval_fit <- choice_data
  
  # Add BFS-online predictions
  if ("search_threshold" %in% param_names) {
    eval_fit <- eval_fit %>%
      left_join(
        bfs_predictions,
        by = c(
          "shortest_path", "startpoint_id", "endpoint_id", "opt1_id", "opt2_id"
        )
      ) %>%
      mutate(
        p_sub_choice_bfs = if_else(
          sub_choice == opt1_id,
          p_bfs_chooses_opt1,
          1 - p_bfs_chooses_opt1
        )
      )
  }
  
  # Add SR-cached predictions
  if ("sr_gamma" %in% param_names) {
    eval_fit <- eval_fit %>%
      left_join(
        sr_rep %>% rename(opt1_sr = sr_value),
        by = c("opt1_id"="from", "endpoint_id"="to")
      ) %>%
      left_join(
        sr_rep %>% rename(opt2_sr = sr_value),
        by = c("opt2_id"="from", "endpoint_id"="to")
      ) %>%
      rowwise() %>%
      mutate(
        p_sub_choice_sr = softmax(
          option_values = c(opt1_sr, opt2_sr) * sr_scaling_constant,
          option_chosen = if_else(sub_choice == opt1_id, 1, 2),
          temperature = softmax_temperature,
          use_inverse_temperature = TRUE,
          lapse_rate = lapse_rate
        )
      ) %>%
      ungroup()
  }
  
  # Compute likelihood
  if (all(c("search_threshold", "sr_gamma") %in% param_names)) {
    # Hybrid model: BOTH BFS-online and SR-cached
    likelihood <- eval_fit %>%
      # What's the probability of *completing* BFS-online all the way through?
      rowwise() %>%
      mutate(
        search_threshold = search_threshold,
        p_online = softmax(
          option_values = c(search_threshold, bfs_visits),
          option_chosen = 1,
          temperature = 1
        )
      ) %>%
      ungroup() %>%
      # Weigh the two models' predictions accordingly
      mutate(
        p_cached = 1 - p_online,
        p_sub_choice = (p_online * p_sub_choice_bfs) +
          (p_cached * p_sub_choice_sr),
      )
  } else if ("search_threshold" %in% param_names) {
    # ONLY BFS-online
    likelihood <- eval_fit %>%
      # What's the probability of *completing* BFS-online all the way through?
      rowwise() %>%
      mutate(
        search_threshold = search_threshold,
        p_online = softmax(
          option_values = c(search_threshold, bfs_visits),
          option_chosen = 1,
          temperature = 1
        )
      ) %>%
      ungroup() %>%
      # Weigh BFS predictions accordingly
      mutate(
        p_give_up = 1 - p_online,
        p_sub_choice = (p_online * p_sub_choice_bfs) + (p_give_up * 1/2),
        ### Add lapse rate
        #   Dividing by 2 is because there are two options to choose from
        #   Therefore, when lapse rate = 1, this becomes chance = 1/2
        p_sub_choice = p_sub_choice * (1-lapse_rate) + (lapse_rate/2)
      )
  } else if ("sr_gamma" %in% param_names) {
    likelihood <- eval_fit %>%
      # Just to make some code work later on in the script
      mutate(p_sub_choice = p_sub_choice_sr)
  }
  
  return(
    likelihood %>%
      mutate(neg_ll = neg_loglik_logistic(p_sub_choice)) %>%
      summarise(sum(neg_ll)) %>%
      deframe()
  )
}
