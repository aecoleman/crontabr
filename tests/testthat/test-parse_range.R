test_that("parse_range works", {

  expect_equal(
    parse_range("0-5"),
    c(0L:5L)
    )

  expect_equal(
    parse_range("Jan-Mar", type = "month"),
    1L:3L
  )

  expect_equal(
    parse_range("Sun-Wed", type = "weekday"),
    0L:3L
  )

})

test_that("parse_range errors for numbers out of order", {

   expect_error(
     parse_range("5-1")
   )


  expect_error(
    parse_range("Wed-Mon")
  )

  expect_error(
    parse_range("Mar-Jan")
  )

  }
)
