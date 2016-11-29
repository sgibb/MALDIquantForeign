context("msg")


test_that("msg-functions", {
  expect_message(MALDIquantForeign:::.msg(TRUE, "foobar"), "foobar")
  expect_message(MALDIquantForeign:::.msg(TRUE, "foo", "bar"), "foobar")
  expect_silent(MALDIquantForeign:::.msg(FALSE, "foobar"))
})
