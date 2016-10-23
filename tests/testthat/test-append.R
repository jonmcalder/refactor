context("test append")
f_char <- c('c','a','a','b','a')
f_fact <- cfactor(c('c','a','a','b','a'), levels= c('a','b','c'))
g_fact <- cfactor(letters[4:10], levels = letters[4:10])
g_char <- letters[4:10]

h_char <- c('c', NA, 'a','a','b','a')
h_fact <- cfactor(c('c', NA,'a','a','b','a'), levels= c('a','b','c'))


test_that("append returns factor if x or values are factor", {
  expect_equal(class(append(f_fact, g_fact)), "factor")
  expect_equal(class(append(f_fact, g_char)), "factor")
  expect_equal(class(append(f_char, g_char)), "character")
  
})


# further tests: 
# - ordering / interaction with cfactor