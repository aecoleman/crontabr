convert_to_int <- function(string, type = c("numeric", "month", "weekday")) {

  weekday.abb <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

  if (type == "month" && string %in% month.abb) {
    int <- match(string, month.abb)
  } else if (type == "weekday" && string %in% weekday.abb) {
    int <- match(string, weekday.abb) - 1L
  } else if (stringr::str_detect(string, "^[0-9]+$")) {
    int <- as.integer(string)
  } else {
    stop("string cannot be interpreted")
  }

  int

}
