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

orderedfactor1 <- factor(sample(letters, 100, replace = TRUE), ordered = TRUE)
orderedfactor2 <- factor(sample(letters, 100, replace = TRUE), ordered = TRUE)

orderednumfactor1 <- factor(sample(1:100, replace = TRUE), ordered = TRUE)
orderednumfactor2 <- factor(sample(1:100, replace = TRUE), ordered = TRUE)

test_that("cc returns an ordered factor", {
  # char factor
  expect_factor(cc(orderedfactor1, orderedfactor2), ordered = TRUE)
  expect_identical(orderedfactor1 %in% cc(orderedfactor1, orderedfactor2), 
                   rep(TRUE, length(orderedfactor1)))
  expect_identical(orderedfactor2 %in% cc(orderedfactor1, orderedfactor2), 
                   rep(TRUE, length(orderedfactor2)))
  # numeric factor
  expect_factor(cc(orderednumfactor1, orderednumfactor2), ordered = TRUE)
  expect_identical(orderednumfactor1 %in% cc(orderednumfactor1, orderednumfactor2), 
                   rep(TRUE, length(orderednumfactor1)))
  expect_identical(orderednumfactor2 %in% cc(orderednumfactor1, orderednumfactor2), 
                   rep(TRUE, length(orderednumfactor2)))
})

context("cc mixed input")
char1 <- sample(letters)
num1 <- 1:10
boo1 <- c(TRUE, FALSE)
test_that("cc returns a character vector if input is mixed (factor and character", {
  expect_character(cc(char1, factor1 ))
  expect_character(cc(num1, factor1 ))
  expect_character(cc(boo1, factor1 ))
    
})
  