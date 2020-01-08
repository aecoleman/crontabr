parse_weekday <- function(weekday) {

   # Case: Vector or list supplied
  if (length(weekday) > 1L) {
    lapply(weekday, parse_weekday) %>% unlist() %>% sort() %>% unique()
  # Case: Wildcard
  } else if (weekday == "*") {
    seq.int(from = 0L, to = 6L)
  # Case: Specified Range
  } else if (stringr::str_detect(weekday, "^.+-.+$")) {
    parse_range(weekday, type = "weekday")
  # Case: None of the above
  } else {
    convert_to_int(weekday, type = "weekday")
  }

}
