library("testthat")

context("ordered_cut")
cfactor1 <- cfactor(sample(c(letters, sample(letters, 100, replace = T))), ordered = T)
cfactor2 <- cfactor(sample(letters[3:16], 100, replace = T), ordered = T)


# breaks_mode = 'default'

## no labels
case1a <- cut(cfactor1, breaks = c("a", "q", "z"), right = T, include.lowest = T)
case2a <- cut(cfactor1, breaks = c("a", "q", "z"), right = F, include.lowest = T)

# custom labels
case1b <- cut(cfactor1, breaks = c("a", "q", "z"), 
              labels = c("group one", "group 2"), right = T, include.lowest = T)
case2b <- cut(cfactor1, breaks = c("a", "q", "z"), 
              labels = c("a first group", "another one"), 
              right = F, include.lowest = T)

# breaks as integer with no label
some_letters <- cfactor(sample(letters), ordered = TRUE)
case7a <- cut(some_letters, breaks = 2, include.lowest = TRUE)


# breaks of length 1
case3a <- cut(cfactor1, breaks = 2, labels = c("a first group", "another one"), 
              right = F, include.lowest = T)
case3b <- cut(cfactor1, breaks = 2, labels = c("a first group", "another one"), 
              right = T, include.lowest = F)

## labels
# labels = F
case4a <- cut(cfactor1, breaks = 2, labels = F, 
              right = T, include.lowest = T)

## separator
case5a <- cut(cfactor1, breaks = 2, labels = NULL, 
              right = T, include.lowest = T, label_sep = "|")

## binwidth 1


case6a <- cut(cfactor(sample(letters), ordered = T), breaks = letters)
################################################################################

test_that("cut.ordered with breaks_mode = 'default'", {
  ## breaks of length > 1
  # simple cases
  expect_equal(levels(case1a), c("a-q", "r-z"))
  expect_equal(levels(case2a), c("a-p", "q-z"))
  expect_equal(levels(case1b), c("group one", "group 2"))
  expect_equal(levels(case2b), c("a first group", "another one"))
  expect_equal(levels(case5a), c("a|m", "n|z"))
  
  # breaks of length 1
  expect_equal(levels(case3a), c("a first group", "another one"))
  expect_equal(levels(case3b), c("a first group", "another one"))
  
  
  # labels F
  expect_error(case4a, NA)
  expect_equal(levels(case7a), c("a-m", "n-z"))
  
})



# specify breaks out of range
test_that("warnings", {
  # breaks that create missing values
  expect_warning(cut(cfactor1, breaks = c("a", "q", "y"), 
                     right = T, include.lowest = T), 
                 "[[:digit:]] missing values generated")
})

test_that("errors", {
  # breaks that do not exist in data
  expect_error(cut(cfactor2, breaks = c("a", "q", "y"), 
                   right = T, include.lowest = T), 
               "specified breakpoints inexistent in data")
  expect_error(cut(cfactor1, breaks = 2, labels = "a label", 
                   right = T, include.lowest = T))
})

# binwidth 1 
# when braks are integer, right argument does not work