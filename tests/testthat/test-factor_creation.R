context("cfactor")

case1 <- cfactor(rep("x", 5))
case2 <- cfactor(letters, labels = "letter")
case3 <- cfactor(sample(letters, size = 400, replace = TRUE), levels = letters)

# regex ordering used
hard_to_dectect <- c("EUR 21 - EUR 22", "EUR 100 - 101", 
                     "EUR 1 - EUR 10", "EUR 11 - EUR 20")
case4 <- cfactor(hard_to_dectect, ordered = TRUE)


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
  expect_equal(levels(case4), c("EUR 1 - EUR 10", "EUR 11 - EUR 20", 
                                "EUR 21 - EUR 22", "EUR 100 - 101")
  )

})

test_that("warnings", {
  
  # empty levels
  expect_warning(cfactor(x = c("a", "b", "c"), levels = c("a", "b", "c", "d")), 
                 "the following levels were empty") 
  
  # removed levels
  expect_warning(cfactor(x = c("a", "b", "c"), levels = c("b", "c")), 
                 "the following levels were removed")
  
  
  
})
# connector esape hatch

# width-1-categories with no separator
