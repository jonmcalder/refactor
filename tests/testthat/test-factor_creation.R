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
  
  # intersecting x and levels
  ## case 1: only is represented
  expect_warning(cfactor(x = c("a", "b", "c"), levels = c("a", "b", "c"), 
                         labels = c("b", "a", "laste")), 
                 "Some values now used .* is now represented")
  
  ## case 2: only still message
  expect_warning(cfactor(x = c("a", "b", "c"), levels = c("a", "b", "c"), 
                         labels = c("a", "g", "laste")), 
                 "Some values now used .* still represents")
  
  ## case 3: 1 and 2
  expect_warning(cfactor(x = c("a", "b", "c"), levels = c("a", "b", "c"), 
                         labels = c("a", "now", "b")), 
                 "Some values now used .* is now represented .* still represents")
  
  # duplicated factor inputs
  expect_warning(cfactor(c("a", "b"), levels = c("a", "a", "b")),
                 "the following duplicated levels were removed: \n a")
})


test_that("errors because of wrong input types", {
  # exclude
  expect_error(cfactor(letters, exclude = TRUE), 
               "Must have class 'character'")
  expect_error(cfactor(1:26, exclude = "a"), 
               "Must have class 'integer'")
  
  # ordered
  expect_error(cfactor(1:26, ordered = 3), 
               "Must have class 'logical'")
  expect_error(cfactor(sample(letters), nmax = 4), 
               "hash table is full")
})
# connector esape hatch

# width-1-categories with no separator
# labels as character of length 1
# cfactor(1:26, ordered = NA) should not yield error
