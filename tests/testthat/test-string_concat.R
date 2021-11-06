test_that("string concat", {
  a <- "a"
  b <- "b"

  expect_equal(a %++% b %++% a, "aba")
})

test_that("filepath concat", {
  expect_equal("hello" %//% "world", "hello/world")
  expect_equal("hello/" %//% "world", "hello/world")
  expect_equal("hello" %//% "/world", "hello/world")
  expect_equal("hello/" %//% "/world", "hello/world")
})
