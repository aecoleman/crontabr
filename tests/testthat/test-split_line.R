test_that("split_line works", {

  line <- "0 * * * * split line test"

  expect_equal(
    split_line(line),
    list(
      minute = "0",
      hour = "*",
      day_of_month = "*",
      month = "*",
      weekday = "*",
      description = "split line test"
      )
    )

})
