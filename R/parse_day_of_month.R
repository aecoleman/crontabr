parse_day_of_month <- function(day) {

  if (is.null(day)) {
    NULL
  } else if (length(day) > 1L) {
    lapply(day, parse_day_of_month) %>% unlist() %>% sort() %>% unique()
  # Case: Wildcard
  } else if (day == "*") {
    seq.int(from = 1L, to = 31L)
  # Case: Last
  } else if (day == "last") {
    -1L
  # Case: Specified Range
  } else if (stringr::str_detect(day, "^.+-.+$")) {
    parse_range(day, type = "numeric")
  # Case: None of the above
  } else {
    as.integer(day)
  }

}


