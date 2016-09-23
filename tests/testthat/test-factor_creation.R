context("cfactor")

case1 <- cfactor(rep("x", 5))
case2 <- cfactor(letters, labels = "letter")
case3 <- cfactor(sample(letters, size = 400, replace = TRUE), levels = letters)

test_that("cfactor returns a factor", {
  expect_output(str(case1), "Factor")
  expect_output(str(case2), "Factor")
  expect_output(str(case3), "Factor")
})

test_that("cfactor returns expected levels", {
  expect_equal(levels(case1), "x")
  
  # is this really desired?
  expect_equal(levels(case2), paste("letter", 1:26, sep = "")) 
  expect_equal(levels(case3), letters)

})

# connector esape hatch

# width-1-categories with no separator
