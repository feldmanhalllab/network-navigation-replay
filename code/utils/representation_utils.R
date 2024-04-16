#### SR utils ####

build_successor <- function(
    successor_matrix, alpha, gamma,
    observations, sr_value_col_name,
    bidirectional = TRUE) {
  
  sr_update <- function (
    input_matrix, alpha, gamma,
    previous_state, current_state, 
    bidirectional = TRUE
  ) {
    forward_onehot <- input_matrix[previous_state, ] * 0
    forward_onehot[current_state] <- 1
    forward_delta <- (
      forward_onehot +
        (gamma * input_matrix[current_state, ]) -
        input_matrix[previous_state, ]
    )
    
    output <- input_matrix
    output[previous_state, ] <- (
      input_matrix[previous_state, ] + (alpha * forward_delta)
    )
    
    if (bidirectional) {
      backward_onehot <- input_matrix[current_state, ] * 0
      backward_onehot[previous_state] <- 1
      backward_delta <- (
        backward_onehot +
          (gamma * input_matrix[previous_state, ]) -
          input_matrix[current_state, ]
      )
      output[current_state, ] <- (
        input_matrix[current_state, ] + (alpha * backward_delta)
      )
    }
    return(output)
  }
  
  obs_matrix <- as.matrix(select(observations, from, to))
  for (j in 1:nrow(observations)) {
    previous_state <- obs_matrix[j, 1]
    current_state <- obs_matrix[j, 2]
    successor_matrix <- sr_update(
      successor_matrix,
      alpha,
      gamma,
      previous_state,
      current_state,
      bidirectional = TRUE
    )
  }
  
  return(
    successor_matrix %>%
      as.data.frame() %>%
      mutate(from = row_number()) %>%
      pivot_longer(
        cols = -from,
        names_to = "to",
        values_to = sr_value_col_name
      ) %>%
      mutate(
        to = str_remove(to, "V"),
        to = as.numeric(to)
      ) %>%
      mutate(
        alpha = alpha,
        gamma = gamma
      )
  )
}

# Use the analytical method to compute the asymptotic SR
build_successor_analytically <- function(
  transition_matrix, lookahead_steps=NA, successor_horizon=NA, normalize=TRUE
) {
  if (!is.matrix(transition_matrix)) {
    stop("Transition matrix must be provided.")
  }
  
  if (length(dim(transition_matrix)) != 2) {
    stop("Two-dimensional transition matrix must be provided.")
  }
  
  if (dim(transition_matrix)[1] != dim(transition_matrix)[2]) {
    stop("Transition matrix must be square.")
  }
  
  if (
    (is.na(lookahead_steps) & is.na(successor_horizon)) |
    (!is.na(lookahead_steps) & !is.na(successor_horizon))
  ) {
    stop(
      "Specify EITHER the number of lookahead steps, OR the successor horizon."
    )
  }
  
  if (
    (!is.na(successor_horizon)) &
    ((successor_horizon < 0) | (successor_horizon > 1))
  ) {
    stop(
      "The successor horizon MUST fall in the range [0, 1]."
    )
  }
  
  if (!is.na(lookahead_steps)) {
    successor_horizon <- 1 - (1/lookahead_steps)
  }
  
  if (!is.na(successor_horizon)) {
    lookahead_steps <- 1 / (1-successor_horizon)
  }
  
  if (lookahead_steps == 1) {
    sr_matrix <- transition_matrix
  } else {
    identity_matrix <- diag(dim(transition_matrix)[1])
    sr_matrix <- solve(identity_matrix - successor_horizon*transition_matrix)
  }
  
  if (normalize) {
    sr_matrix <- sr_matrix / lookahead_steps
  }
  
  return(
    sr_matrix %>%
      as.data.frame() %>%
      mutate(from = row_number()) %>%
      pivot_longer(
        cols = -from,
        names_to = "to"
      ) %>%
      mutate(
        to = str_remove(to, "V"),
        to = as.numeric(to)
      ) %>%
      mutate(gamma = successor_horizon)
  )
}


#### Build representations ####

build_rep_sr <- function(
    learning_data, this_alpha, this_gamma, bidirectional = TRUE
) {
  
  these_cols <- colnames(learning_data)
  
  if (!any(these_cols == "from") | !any(these_cols == "to")) {
    stop("Your dataframe does not contain the columns `from` and `to`.")
  } else if (
    !is.numeric(learning_data$from) |
    !is.numeric(learning_data$to)
  ) {
    stop("One or both of the columns `from` and `to` contain non-numbers.")
  }
  
  n_nodes <- with(learning_data, max(from, to))
  
  # sr_rep <- matrix(0, n_nodes, n_nodes) %>%
  #   build_successor(
  #     alpha = this_alpha,
  #     gamma = this_gamma,
  #     observations = learning_data,
  #     sr_value_col_name = "sr_value",
  #     bidirectional = bidirectional
  #   ) %>%
  #   # Normalize expected-counts into transition matrix
  #   mutate(sr_value = sr_value * (1 - this_gamma)) %>%
  #   select(from, to, sr_value) %>%
  #   # Make sure datatypes are consistent
  #   mutate(across(c(from, to), as.integer))
  
  # Try initializing the SR matrix w/ flat prior, instead of zeroes
  # Equal probability of transitioning from X -> X' (but X -> X is impossible)
  sr_rep <- matrix(1/(n_nodes-1), n_nodes, n_nodes)
  diag(sr_rep) <- 0
  
  sr_rep <- sr_rep %>%
    build_successor(
      alpha = this_alpha,
      gamma = this_gamma,
      observations = learning_data,
      sr_value_col_name = "sr_value",
      bidirectional = bidirectional
    ) %>%
    # Normalize expected-counts into transition matrix
    mutate(sr_value = sr_value * (1 - this_gamma)) %>%
    select(from, to, sr_value) %>%
    # Make sure datatypes are consistent
    mutate(across(c(from, to), as.integer))
  
  return ( sr_rep )
}

