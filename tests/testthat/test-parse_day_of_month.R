test_that("parse_day_of_month works", {

  expect_equal(
    parse_day_of_month("3"),
    3L)

  expect_equal(
    parse_day_of_month("last"),
    -1L
  )

  expect_equal(
    parse_day_of_month("4-10"),
    4L:10L
  )

})
