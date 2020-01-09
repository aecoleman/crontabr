test_that("check_date works", {

  expect_equal(
    check_date(
      date = as.Date("2020-01-01"),
      line = "* * 1 1 * Jan 1"),
    TRUE
    )

  expect_equal(
    check_date(
      date = as.Date("2020-01-01"),
      line = "* * 1 2 * Jan 1"),
    FALSE
    )

  expect_equal(
    check_date(
      date = as.Date("2020-01-08"),
      line = "* * * 1 3 Jan Wed"
      ),
    TRUE
  )

  expect_equal(
    check_date(
      date = as.Date("2020-01-08"),
      line = "* * * 1 2 Jan Wed"
      ),
    FALSE
  )

})
