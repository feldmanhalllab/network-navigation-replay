library(tidyverse)
library(tidygraph)


single_bfs_iter <- function(input_graph, visits, queue, stop_for) {
  # Retrieve the first element in queue, find its neighbors
  first_queue_neighbors <- igraph::neighbors(
    input_graph, as.character(queue[1])
  ) %>% igraph::as_ids()
  
  # We only care about neighbors who we have not yet visited
  unvisited_neighbors <- (
    first_queue_neighbors[!first_queue_neighbors %in% visits]
  )
  
  # Visit/queue neighbors in a random order
  unvisited_neighbors <- sample(
    unvisited_neighbors, length(unvisited_neighbors)
  )
  
  # Don't add redundant neighbors to queue
  unqueued_neighbors <- unvisited_neighbors[!unvisited_neighbors %in% queue]
  
  # Add novel neighbors to visit and queue vectors
  visits <- append(visits, unvisited_neighbors)
  queue <- append(queue, unqueued_neighbors)
  
  # De-queue first element
  queue <- queue[-1]
  
  # Check whether to stop search
  endpoint_reached <- any(stop_for %in% unvisited_neighbors)
  queue_empty <- length(queue) == 0
  
  return(
    list(
      visits = visits,
      queue = queue,
      endpoint_reached = endpoint_reached,
      queue_empty = queue_empty
    )
  )
}


mpt_bfs <- function(
    input_tbl_graph,
    startpoint, endpoint, option_a, option_b,
    save_every_iter = FALSE
) {
  # Message-passing task forbids returning the message back to sender
  modified_graph <- input_tbl_graph %>%
    mutate(name = row_number()) %>%
    igraph::delete_vertices(startpoint)
  
  # Initialize visit/queue vectors
  visits_a <- option_a
  queue_a <- option_a
  visits_b <- option_b
  queue_b <- option_b
  
  # Initialize flags for stopping search
  endpoint_reached_a <- FALSE
  queue_empty_a <- FALSE
  endpoint_reached_b <- FALSE
  queue_empty_b <- FALSE
  
  # For storing info about each BFS iteration
  iteration <- 0
  iter_id <- c()
  iter_n_visits_total <- c()
  iter_n_visits_unique <- c()
  iter_n_visits_opt1 <- c()
  iter_n_visits_opt2 <- c()
  
  # Search until an answer is found
  while(
    !any(queue_empty_a, queue_empty_b, endpoint_reached_a, endpoint_reached_b)
  ) {
    # Randomly choose to explore the "tree" associated with option A or B
    search_this_tree <- sample(c(option_a, option_b), size = 1)
    
    # Increment iterations
    iteration <- iteration + 1
    
    # Perform a single BFS iteration
    if(search_this_tree == option_a) {
      bfs_iter <- single_bfs_iter(modified_graph, visits_a, queue_a, endpoint)
      visits_a <- bfs_iter$visits
      queue_a <- bfs_iter$queue
      endpoint_reached_a <- bfs_iter$endpoint_reached
      queue_empty_a <- bfs_iter$queue_empty
    } else {
      bfs_iter <- single_bfs_iter(modified_graph, visits_b, queue_b, endpoint)
      visits_b <- bfs_iter$visits
      queue_b <- bfs_iter$queue
      endpoint_reached_b <- bfs_iter$endpoint_reached
      queue_empty_b <- bfs_iter$queue_empty
    }
    
    # Update info about each BFS iteration
    if(save_every_iter) {
      iter_id <- append(iter_id, iteration)
      iter_n_visits_total <- append(
        iter_n_visits_total, length(c(visits_a, visits_b))
      )
      iter_n_visits_unique <- append(
        iter_n_visits_unique, length(unique(c(visits_a, visits_b)))
      )
      iter_n_visits_opt1 <- append(iter_n_visits_opt1, length(visits_a))
      iter_n_visits_opt2 <- append(iter_n_visits_opt2, length(visits_b))
    }
  }
  
  # Update info about each BFS (summarizing over iterations)
  if(!save_every_iter) {
    iter_id <- append(iter_id, iteration)
    iter_n_visits_total <- append(
      iter_n_visits_total, length(c(visits_a, visits_b))
    )
    iter_n_visits_unique <- append(
      iter_n_visits_unique, length(unique(c(visits_a, visits_b)))
    )
    iter_n_visits_opt1 <- append(iter_n_visits_opt1, length(visits_a))
    iter_n_visits_opt2 <- append(iter_n_visits_opt2, length(visits_b))
  }
  
  # Which option is chosen?
  if(endpoint_reached_a | queue_empty_b) {
    option_chosen <- option_a
    n_visits_chosen <- length(visits_a)
  } else {
    option_chosen <- option_b
    n_visits_chosen <- length(visits_b)
  }
  
  # Why?
  if(endpoint_reached_a | endpoint_reached_b) {
    why_chosen <- "endpoint reached"
  } else {
    why_chosen <- "empty queue"
  }
  
  return(
    tibble(
      bfs_iter = iter_id,
      bfs_n_visits_total = iter_n_visits_total,
      bfs_n_visits_unique = iter_n_visits_unique,
      bfs_n_visits_opt1 = iter_n_visits_opt1,
      bfs_n_visits_opt2 = iter_n_visits_opt2,
      bfs_choice = option_chosen,
      bfs_why_chosen = why_chosen
    )
  )
}

mpt_bfs_reverse <- function(
    input_tbl_graph, startpoint, endpoint, option_a, option_b
) {
  # Message-passing task forbids returning the message back to sender
  modified_graph <- input_tbl_graph %>%
    mutate(name = row_number()) %>%
    igraph::delete_vertices(startpoint)
  
  # Initialize visit/queue vectors
  visits <- endpoint
  queue <- endpoint
  
  # Initialize flag for stopping search
  endpoint_reached <- FALSE
  
  # For storing info about each BFS iteration
  iteration <- 0
  
  # Search until an answer is found
  while(!endpoint_reached) {
    # Increment iterations
    iteration <- iteration + 1
    
    # Perform a single BFS iteration
    bfs_iter <- single_bfs_iter(
      modified_graph, visits, queue, c(option_a, option_b)
    )
    visits <- bfs_iter$visits
    queue <- bfs_iter$queue
    endpoint_reached <- bfs_iter$endpoint_reached
  }
  
  # Which option is chosen?
  if(all(c(option_a, option_b) %in% visits)) {
    option_chosen = NA
  } else if(option_a %in% visits) {
    option_chosen = option_a
  } else {
    option_chosen = option_b
  }
  
  return(
    tibble(
      bfs_iter = iteration,
      bfs_n_visits_total = length(unique(visits)),
      bfs_choice = option_chosen
    )
  )
}

