context("cc-factor")

factor1 <- factor(sample(letters, 100, replace = TRUE))
factor2 <- factor(sample(letters, 100, replace = TRUE))

numfactor1 <- factor(sample(1:100, replace = TRUE))
numfactor2 <- factor(sample(1:100, replace = TRUE))

test_that("cc returns a factor", {
  # char factor
  expect_factor(cc(factor1, factor2))
  expect_identical(factor1 %in% cc(factor1, factor2), rep(TRUE, length(factor1)))
  expect_identical(factor2 %in% cc(factor1, factor2), rep(TRUE, length(factor2)))
  # numeric factor
  expect_factor(cc(numfactor1, numfactor2))
  expect_identical(numfactor1 %in% cc(numfactor1, numfactor2), rep(TRUE, length(numfactor1)))
  expect_identical(numfactor2 %in% cc(numfactor1, numfactor2), rep(TRUE, length(numfactor2)))
})

context("cc-ordered")

orderedfactor1 <- factor(sample(letters, replace = FALSE), ordered = TRUE)
orderedfactor2 <- factor(sample(letters, replace = FALSE), ordered = TRUE)

orderednumfactor1 <- factor(sample(1:100, replace = TRUE), ordered = TRUE)
orderednumfactor2 <- factor(sample(1:100, replace = TRUE), ordered = FALSE)
orderednumfactor3 <- factor(sample(1:100, replace = TRUE), ordered = TRUE)

test_that("cc returns an ordered factor", {
  # char factor
  expect_factor(cc(orderedfactor1, orderedfactor2), ordered = TRUE)
  expect_identical(orderedfactor1 %in% cc(orderedfactor1, orderedfactor2), 
                   rep(TRUE, length(orderedfactor1)))
  expect_identical(orderedfactor2 %in% cc(orderedfactor1, orderedfactor2), 
                   rep(TRUE, length(orderedfactor2)))
  # numeric factor
  expect_warning(cc(orderednumfactor1, orderednumfactor3))
  expect_factor(cc(orderednumfactor2, orderednumfactor2))
})

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
newlevels <- cfactor(orderedfactor1, levels = rev(levels(orderedfactor1)))
newlevels_unordered1 <- cfactor(orderedfactor1, 
                               levels = rev(levels(orderedfactor1)), 
                               ordered = FALSE)
newlevels_unordered2 <- cfactor(orderedfactor1, 
                               levels = levels(orderedfactor1), 
                               ordered = FALSE)

test_that("cc returns warnings", {
  # for ordered factors
  expect_warning(cc(orderedfactor1, orderednumfactor1), 
                 "ordering and levels not preserved since levels not identical")
  expect_identical(class(cc(orderedfactor1, orderednumfactor1)), 
                   "factor")
  expect_warning(cc(orderedfactor1, newlevels), 
                 "ordering not preserved since ordering of levels not identical")
  expect_identical(class(cc(orderedfactor1, newlevels)), 
                   "factor")
  
  
  # for ordered factors
  expect_warning(cc(newlevels_unordered2, newlevels_unordered1), 
                 "ordering not preserved since ordering of levels not identical")
  expect_identical(class(cc(newlevels_unordered2, newlevels_unordered1)), 
                   "factor")
  
  # for mixed input
  expect_warning(cc(newlevels_unordered2, newlevels),
                 "ordering not preserved since ordering of levels not identical")

})

context("cc mixed input")
char1 <- sample(letters)
num1 <- 1:10
boo1 <- c(TRUE, FALSE)
test_that("cc returns same as c if not all input elements are at least factors", {
  # two different classes
  expect_identical(cc(char1, factor1), c(char1, factor1))
  expect_identical(cc(char1, num1), c(char1, num1))
  expect_identical(cc(factor1, num1), c(factor1, num1))
  # same class
  expect_identical(cc(char1, char1), c(char1, char1))
  expect_identical(cc(boo1, boo1), c(boo1, boo1))
})
