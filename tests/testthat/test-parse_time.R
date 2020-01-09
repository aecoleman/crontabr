test_that("parse_time works", {

  expect_equal(
    parse_minute("4"),
    4L)

  expect_equal(
    parse_minute("*"),
    0L:59L)

  expect_equal(
    parse_minute("0-4"),
    0L:4L)


})
