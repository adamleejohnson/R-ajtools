test_that("string concat", {
  a <- "a"
  b <- "b"

  expect_equal(a %paste% b %paste% a, "aba")
})

test_that("filepath concat", {
  expect_equal("hello" %slash% "world", "hello/world")
  expect_equal("hello/" %slash% "world", "hello/world")
  expect_equal("hello" %slash% "/world", "hello/world")
  expect_equal("hello/" %slash% "/world", "hello/world")
})