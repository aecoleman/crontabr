check_date <- function(date = Sys.Date(), line) {

  line_args <-
    line %>%
    split_line() %>%
    magrittr::extract(3L:5L) %>%
    purrr::discard(~ .x == "*")

  c(check_month,
    check_day_of_month,
    check_weekday
    ) %>%
  purrr::map_lgl(
    ~ .x(date, line_args)
    ) %>%
  all()

}

check_month <- function(date, line_args) {

  test_val <-
    line_args %>%
    purrr::pluck("month") %>%
    split_arg() %>%
    parse_month()

  any(
    is.null(test_val),
    lubridate::month(date) %in% test_val
    )

}

check_day_of_month <- function(date, line_args) {

  test_val <-
    line_args %>%
    purrr::pluck("day_of_month") %>%
    split_arg() %>%
    parse_day_of_month()

  any(
    is.null(test_val),
    lubridate::day(date) %in% test_val,
    test_val == -1L && lubridate::day(date) == lubridate::days_in_month(date)
    )

}

check_weekday <- function(date, line_args) {

  test_val <-
    line_args %>%
    purrr::pluck("weekday") %>%
    split_arg() %>%
    parse_month()

  any(
    is.null(test_val),
    (lubridate::wday(x = date, label = FALSE, abbr = FALSE) %% 7L) %in% test_val
  )

}
