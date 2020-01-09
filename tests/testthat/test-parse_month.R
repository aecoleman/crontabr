test_that("parse_month works", {

  expect_equal(
    parse_month("Mar"),
    3L)

  expect_equal(
    parse_month("Mar-Jun"),
    3L:6L
  )

  expect_equal(
    parse_month("3"),
    3L)

  expect_equal(
    parse_month("3-8"),
    3L:8L)


})
