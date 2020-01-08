parse_month <- function(month) {

  if (is.null(month)) {
    NULL
  } else if (length(month) > 1L) {
    lapply(month, parse_month) %>% unlist() %>% sort() %>% unique()
  } else if (month == "*") {
    seq.int(from = 1L, to = 12L)
  } else if (stringr::str_detect(month, "^.+-.+$")) {
    parse_range(month, type = "month")
  } else {
    convert_to_int(month, type = "month")
  }

}
