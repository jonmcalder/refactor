context("test ifelse")

f_char <- c('c','a','a','b','a')
f_fact <- cfactor(c('c','a','a','b','a'), levels= c('a','b','c'))
g_fact <- cfactor(letters[4:10], levels = letters[4:10])
g_char <- letters[4:10]

h_char <- c('c', NA, 'a','a','b','a')
h_fact <- cfactor(c('c', NA,'a','a','b','a'), levels= c('a','b','c'))


test_that("ifelse returns factor if yes or no are factor", {
  expect_equal(class(ifelse(is.na(f_fact), "g", f_fact)), "factor")
  expect_equal(class(ifelse(is.na(f_char), factor("g"), f_fact)), "factor")
  expect_equal(class(ifelse(is.na(f_char), "g", f_char)), "character")
  
})

# further tests: 
# - ordering / interaction with cfactor