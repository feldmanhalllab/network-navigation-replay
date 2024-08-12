library(kableExtra)

kable_custom <- function(df, captions = "", grouping_var = NULL, digits = 3) {
  # For the caption
  strings_flat <- stringr::str_flatten(captions, collapse = "<br>")
  
  # For creating a table with grouped rows
  n_rows_per_group <- df %>%
    group_by({{ grouping_var }}) %>%
    summarise(N = max(row_number()))
  
  no_grouping_vars <- length(deframe(n_rows_per_group)) == 1
  
  if (no_grouping_vars) {
    # No grouping variables
    out <- df %>%
      kbl(
        caption = str_c(
          "<center>",
          strings_flat,
          "</center>"
        ),
        digits = digits
      ) %>%
      kable_styling(bootstrap_options = c("responsive"))
  } else {
    groups_in_order <- df %>%
      select({{grouping_var}}) %>%
      distinct() %>%
      left_join(n_rows_per_group, by = join_by({{grouping_var}})) %>%
      deframe()
    
    out <- df %>%
      select(-{{ grouping_var }}) %>%
      kbl(
        caption = str_c(
          "<center>",
          strings_flat,
          "</center>"
        ),
        digits = digits
      ) %>%
      kable_styling(bootstrap_options = c("responsive")) %>%
      group_rows(index = groups_in_order)
  }
  
  return(out)
}
