theme_custom <- function(include_y_grid = FALSE) {
  out <- list(
    theme_bw(),
    theme(
      plot.title = element_text(hjust = 0.5, size = 13),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.75, "lines"),
      legend.box.spacing = unit(0.5, "lines"),
      legend.margin = margin(c(0, 0, 0, 0), unit = "lines")
    )
  )
  
  if (include_y_grid) {
    out <- list(out, theme(panel.grid.major.x = element_blank()))
  } else {
    out <- list(out, theme(panel.grid.major = element_blank()))
  }
  
  return(out)
}

theme_heatmap <- function() {
  return(
    list(
      theme_custom(),
      theme(
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "grey"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
    )
  )
}

theme_network <- function() {
  list(
    theme_custom(),
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  )
}

scale_RdBu <- function(direction = 1, midpoint = 0, ...) {
  if(direction == 1) {
    low_color <- "#b2182b"
    high_color <- "#2166ac"
  } else if(direction == -1) {
    high_color <- "#b2182b"
    low_color <- "#2166ac"
  }
  
  return(
    scale_fill_gradient2(
      low = low_color,
      mid = "white",
      high = high_color,
      na.value = "grey",
      midpoint = midpoint,
      ...
    )
  )
}
