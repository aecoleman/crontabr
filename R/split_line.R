split_line <- function(line) {

  line_args <-
    line %>%
    stringr::str_split_fixed(
      pattern = " ",
      n = 6L
      ) %>%
    apply(
      MARGIN = 2L,
      FUN = identity
      ) %>%
    lapply(
      FUN = identity
      ) %>%
    magrittr::set_names(
      c("minute", "hour", "day_of_month", "month", "weekday", "description")
      )

  return(line_args)

}
