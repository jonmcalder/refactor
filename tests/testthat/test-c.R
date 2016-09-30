context("c.factor")

factor1 <- factor(sample(letters, 100, replace = TRUE))
factor2 <- factor(sample(letters, 100, replace = TRUE))

numfactor1 <- factor(sample(1:100, replace = TRUE))
numfactor2 <- factor(sample(1:100, replace = TRUE))

test_that("c.factor returns a factor", {
  # char factor
  expect_factor(c(factor1, factor2))
  expect_identical(factor1 %in% c(factor1, factor2), rep(TRUE, length(factor1)))
  expect_identical(factor2 %in% c(factor1, factor2), rep(TRUE, length(factor2)))
  # numeric factor
  expect_factor(c(numfactor1, numfactor2))
  expect_identical(numfactor1 %in% c(numfactor1, numfactor2), rep(TRUE, length(numfactor1)))
  expect_identical(numfactor2 %in% c(numfactor1, numfactor2), rep(TRUE, length(numfactor2)))
})

context("c.ordered")

orderedfactor1 <- factor(sample(letters, 100, replace = TRUE), ordered = TRUE)
orderedfactor2 <- factor(sample(letters, 100, replace = TRUE), ordered = TRUE)

orderednumfactor1 <- factor(sample(1:100, replace = TRUE), ordered = TRUE)
orderednumfactor2 <- factor(sample(1:100, replace = TRUE), ordered = TRUE)

test_that("c.ordered returns an ordered factor", {
  # char factor
  expect_factor(c(orderedfactor1, orderedfactor2), ordered = TRUE)
  expect_identical(orderedfactor1 %in% c(orderedfactor1, orderedfactor2), 
                   rep(TRUE, length(orderedfactor1)))
  expect_identical(orderedfactor2 %in% c(orderedfactor1, orderedfactor2), 
                   rep(TRUE, length(orderedfactor2)))
  # numeric factor
  expect_factor(c(orderednumfactor1, orderednumfactor2), ordered = TRUE)
  expect_identical(orderednumfactor1 %in% c(orderednumfactor1, orderednumfactor2), 
                   rep(TRUE, length(orderednumfactor1)))
  expect_identical(orderednumfactor2 %in% c(orderednumfactor1, orderednumfactor2), 
                   rep(TRUE, length(orderednumfactor2)))
})