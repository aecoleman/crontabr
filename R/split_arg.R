split_arg <- function(arg) {

  arg %>%
    stringr::str_split(
      pattern = ","
      ) %>%
    unlist()

}
