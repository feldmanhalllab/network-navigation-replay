#### SR utils ####

build_successor_td_0 <- function(
    successor_matrix, observation_matrix,
    sr_alpha, sr_gamma,
    bidirectional = FALSE
) {
  
  sr_update <- function (
    input_matrix, sr_alpha, sr_gamma,
    previous_state, current_state, 
    bidirectional
  ) {
    forward_onehot <- input_matrix[previous_state, ] * 0
    forward_onehot[current_state] <- 1
    forward_delta <- (
      forward_onehot +
        (sr_gamma * input_matrix[current_state, ]) -
        input_matrix[previous_state, ]
    )
    
    output <- input_matrix
    output[previous_state, ] <- (
      input_matrix[previous_state, ] + (sr_alpha * forward_delta)
    )
    
    if (bidirectional) {
      backward_onehot <- input_matrix[current_state, ] * 0
      backward_onehot[previous_state] <- 1
      backward_delta <- (
        backward_onehot +
          (sr_gamma * input_matrix[previous_state, ]) -
          input_matrix[current_state, ]
      )
      output[current_state, ] <- (
        input_matrix[current_state, ] + (sr_alpha * backward_delta)
      )
    }
    return(output)
  }
  
  for (j in 1:nrow(observation_matrix)) {
    previous_state <- observation_matrix[j, 1]
    current_state <- observation_matrix[j, 2]
    successor_matrix <- sr_update(
      successor_matrix,
      sr_alpha,
      sr_gamma,
      previous_state,
      current_state,
      bidirectional
    )
  }
  
  # Return states using integers or strings?
  if (is.null(rownames(successor_matrix))) {
    rownames_are_strings <- FALSE
  } else {
    rownames_are_strings <- suppressWarnings(
      all(is.na(as.integer(rownames(successor_matrix))))
    )
  }
  
  if (rownames_are_strings == FALSE) {
    out <- successor_matrix %>%
      as.data.frame() %>%
      mutate(from = row_number()) %>%
      pivot_longer(cols = -from, names_to = "to", values_to = "sr_value") %>%
      mutate(
        to = str_remove(to, "V"),
        across(c(from, to), as.numeric)
      ) %>%
      mutate(sr_alpha = sr_alpha, sr_gamma = sr_gamma)
  } else {
    out <- successor_matrix %>%
      as.data.frame() %>%
      rownames_to_column("from") %>%
      pivot_longer(cols = -from, names_to = "to", values_to = "sr_value") %>%
      mutate(sr_alpha = sr_alpha, sr_gamma = sr_gamma)
  }
  
  return(out)
}


learn_successor_td_lambda <- function(
    successor_matrix, observation_matrix,
    sr_alpha, sr_gamma, sr_lambda
) {
  # Should the output states be returned as integers or strings?
  if (is.null(rownames(successor_matrix))) {
    rownames_are_strings <- FALSE
  } else {
    rownames_are_strings <- suppressWarnings(
      all(is.na(as.integer(rownames(successor_matrix))))
    )
  }
  
  # Function depends on indexing named rows/columns
  if (rownames_are_strings == FALSE) {
    colnames(successor_matrix) <- 1:ncol(successor_matrix)
    rownames(successor_matrix) <- 1:ncol(successor_matrix)
  }
  
  # Initialize eligibility traces to 0
  eligibility_traces <- rep(0, ncol(successor_matrix))
  names(eligibility_traces) <- colnames(successor_matrix)
  
  for (trial in 1:nrow(observation_matrix)) {
    current_state <- observation_matrix[trial, 2]
    previous_state <- observation_matrix[trial, 1]
    
    # Update eligibility traces
    eligibility_traces <- sr_gamma * sr_lambda * eligibility_traces
    eligibility_traces[previous_state] <- eligibility_traces[previous_state] + 1
    
    # Element-wise update
    for (M_i in rownames(successor_matrix)) {
      for (M_j in colnames(successor_matrix)) {
        # Update SR matrix
        prediction_error <- (
          as.numeric(current_state == M_j) +
            (sr_gamma * successor_matrix[current_state, M_j]) -
            successor_matrix[previous_state, M_j]
        )
        
        successor_matrix[M_i, M_j] <- successor_matrix[M_i, M_j] + (
          sr_alpha * prediction_error * eligibility_traces[M_i]
        )
      }
    }
  }
  
  if (rownames_are_strings == FALSE) {
    out <- successor_matrix %>%
      as.data.frame() %>%
      rownames_to_column("from") %>%
      pivot_longer(cols = -from, names_to = "to", values_to = "sr_value") %>%
      mutate(across(c(from, to), as.numeric)) %>%
      mutate(sr_alpha = sr_alpha, sr_gamma = sr_gamma)
  } else {
    out <- successor_matrix %>%
      as.data.frame() %>%
      rownames_to_column("from") %>%
      pivot_longer(cols = -from, names_to = "to", values_to = "sr_value") %>%
      mutate(sr_alpha = sr_alpha, sr_gamma = sr_gamma)
  }
  
  return(out)
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
  
  if (is.null(rownames(sr_matrix))) {
    rownames_are_strings <- FALSE
  } else {
    rownames_are_strings <- suppressWarnings(
      all(is.na(as.integer(rownames(sr_matrix))))
    )
  }
  
  if (rownames_are_strings == FALSE) {
    out <- sr_matrix %>%
      as.data.frame() %>%
      mutate(from = row_number()) %>%
      pivot_longer(cols = -from, names_to = "to", values_to = "sr_value") %>%
      mutate(to = str_remove(to, "V"), to = as.integer(to)) %>%
      mutate(sr_gamma = successor_horizon)
  } else {
    out <- sr_matrix %>%
      as.data.frame() %>%
      rownames_to_column("from") %>%
      pivot_longer(cols = -from, names_to = "to", values_to = "sr_value") %>%
      mutate(sr_gamma = successor_horizon)
  }
  
  return(out)
}
