parse_line <- function(line) {

  line_args <- split_line(line)

  line_args[-6] %>%
    purrr::map(
      split_arg
      ) %>%
    purrr::map2(
      list(
        parse_minute,
        parse_hour,
        parse_day_of_month,
        parse_month,
        parse_weekday
        ),
      ~ .y(.x)
    )

}
