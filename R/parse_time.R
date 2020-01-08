
parse_minute <- function(minute) {

  if (is.null(minute)) {
    NULL
    # Case: Vector or list supplied
  } else if (length(minute) > 1L) {
    lapply(minute, parse_minute) %>% unlist() %>% sort() %>% unique()
  # Case: Wildcard
  } else if (minute == "*") {
    seq.int(from = 0L, to = 59L)
  # Case: Specified Range
  } else if (stringr::str_detect(minute, "^.+-.+$")) {
    parse_range(minute, type = "numeric")
  # Case: None of the above
  } else {
    as.integer(minute)
  }

}

parse_hour <- function(hour) {

  if (is.null(hour)) {
    NULL
    # Case: Vector or list supplied
  } else if (length(hour) > 1L) {
    lapply(hour, parse_hour) %>% unlist() %>% sort() %>% unique()
  # Case: Wildcard
  } else if (hour == "*") {
    seq.int(from = 0L, to = 23L)
  # Case: Specified Range
  } else if (stringr::str_detect(hour, "^.+-.+$")) {
    parse_range(hour, type = "numeric")
  # Case: None of the above
  } else {
    as.integer(hour)
  }

}
