parse_range <- function(range, type = c("numeric", "month", "weekday")) {

  if (length(type) > 1L) type <- type[[1L]]

  from <-
    range %>%
    stringr::str_extract("^(.[^-]*)") %>%
    stringr::str_to_title() %>%
    convert_to_int(type = type)

  to <-
    range %>%
    stringr::str_remove("^(.[^-]*)-") %>%
    stringr::str_to_title() %>%
    convert_to_int(type = type)

  stopifnot(from < to)

  seq.int(from = from, to = to)

}
