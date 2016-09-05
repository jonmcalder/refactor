context("cfactor")

case1 <- cfactor(rep("x", 5))
case2 <- cfactor(letters, labels = "letter")
case3 <- cfactor(sample(letters, size = 400, replace = TRUE), levels = letters)
case4 <- cfactor(sample(letters, size = 400, replace = TRUE))

test_that("cfactor returns a factor", {
  expect_output(str(case1), "Factor")
  expect_output(str(case2), "Factor")
  expect_output(str(case3), "Factor")
})

test_that("cfactor returns expected levels", {
  expect_equal(levels(case1), "x")
  expect_equal(levels(case2), paste("letter", 1:26, sep = "")) # is this really desired
  expect_equal(levels(case3), letters)
  expect_equal(levels(case4), letters)
})
