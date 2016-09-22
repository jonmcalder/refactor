context("index_factor_creation")
data <- data.frame(var1 = sample(rep(1:10, 20)),
                   var2 = rep(1:2, 20),
                   var3 = sample(20),
                   var4 = 2, 
                   var5 = sample(row.names(USArrests), size = 20),
                   var6 = as.character(sample(x = 1:10, size = 20, replace = TRUE)),
                   stringsAsFactors = FALSE)

index <- data.frame(var = rep(paste0("var", 1:3), c(10, 2, 20)),
                    encoding = c(1:10, 1:2, 1:20),
                    label = c(letters[1:10], c("male", "female"), LETTERS[1:20]))
# try encoding a non-numeric column
index2 <- data.frame(var = "var5",
                    encoding = c(1:10, 1:2, 1:20),
                    label = c(letters[1:10], c("male", "female"), LETTERS[1:20]))

index3 <- data.frame(var = rep(paste0("var", c(1:3, 6)), c(10, 2, 20, 10)),
                    encoding = c(1:10, 1:2, 1:20, 1:10),
                    label = c(letters[1:10], c("male", "female"), LETTERS[1:20], letters[1:10]))

index4 <- data.frame(var = "var99",
                     encoding = c(1:10, 1:2, 1:20),
                     label = c(letters[1:10], c("male", "female"), LETTERS[1:20]))

index5 <- data.frame(var = rep(c("var99", "var3", "var2"), c(10, 20, 2)),
                     encoding = c(1:10, sample(20), 1:2),
                     label = c(letters[1:10], letters[1:20], c("male", "female")))


test_that("basics", {
  # expect no error
  expect_error(index_cfactor(data = data, index = index, variable = "var"), NA)
  expect_error(index_cfactor(data = data, index = index, variable = "var", ordered = TRUE), NA)
  expect_error(index_cfactor(data = data, index = index, variable = "var", ordered = c(TRUE, FALSE, FALSE)), NA)
})


test_that("error handling", {
  
  # invalid recycling
  expect_error(index_cfactor(data = data, index = index, variable = "var", ordered = c(TRUE,FALSE)), 
                paste("argument 'ordered' is not recycled fully. Number of columns to be decoded should",
                "match the length of 'ordered' or mulitiple thereof."))
  # when the class is character, but it could be coreced to integer. Test not implemented though.
  expect_error(index_cfactor(data = data, index = index3, variable = "var"),
               "The following columns in 'data' cannot be decoded since they do not inherit from class numeric or integer: \n var6")
  
  # when the variables of index can't be found in data
  expect_error(index_cfactor(data = data, index = index4, variable = "var"),
               "The variables in 'index' do not match any of the variables in 'data'")
})

test_that("warnings", {
  # not all variabels from index are used in data
  expect_warning(index_cfactor(data = data, index = index5, variable = "var"), 
                 "The following variables from 'index' are not found in 'data': \n var99")
})

# tests for ... behavior with cfactor not tested
# when the class is character and it could not be coreced to integer. Test not implemented though.
