test_that("split_arg works on numbers", {

  arg <- "0,10,5"

  expect_equal(
    split_arg(arg),
    c("0", "10", "5")
    )
})

test_that("split_arg works on ranges", {

  arg <- "0,3-5"

  expect_equal(
    split_arg(arg),
    c("0", "3-5")
  )

})

test_that("split_arg works on strings", {

  arg <- "mon,thu,fri"

  expect_equal(
    split_arg(arg),
    c("mon","thu", "fri")
  )

})
