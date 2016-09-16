library(testthat)
context("index_factor_creation")
data <- data.frame(var1 = sample(x = 1:10, size = 20, replace = TRUE),
                   var2 = rep(1:2, 20),
                   var3 = sample(20),
                   var4 = 2, 
                   var5 = sample(row.names(USArrests), size = 20),
                   stringsAsFactors = FALSE)

index <- data.frame(var = rep(paste0("var", 1:3), c(10, 2, 20)),
                    encoding = c(1:10, 1:2, 1:20),
                    label = c(letters[1:10], c("male", "female"), LETTERS[1:20]))

test_that("basics", {
  index_cfactor(data = data, index = index, variable = "var")
  index_cfactor(data = data, index = index, variable = "var", ordered = TRUE)
  index_cfactor(data = data, index = index, variable = "var", ordered = c(TRUE, FALSE, FALSE))
})


test_that("error handling", {
  expect_error(index_cfactor(data = data, index = index, variable = "var", ordered = c(TRUE,FALSE)), 
                paste("argument 'ordered' is not recycled fully. Number of columns to be decoded should",
                "match the length of 'ordered' or mulitiple thereof."))
})

# tests for ... behavior with cfactor not tested