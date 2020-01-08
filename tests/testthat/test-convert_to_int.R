test_that("convert_to_int works", {

  expect_equal(
    convert_to_int("4"),
    4L)

  expect_equal(
    convert_to_int("Sun", type = "weekday"),
    0L
  )

  expect_equal(
    convert_to_int("Feb", type = "month"),
    2L
  )

  expect_error(
    convert_to_int("Sun", type = "month")
  )

  expect_error(
    convert_to_int("Feb", type = "weekday")
  )

  expect_error(
    convert_to_int("Sun")
  )

})


