library("testthat")

context("ordered_cut")
cfactor1 <- cfactor(sample(c(letters, sample(letters, 100, replace = TRUE))), ordered = TRUE)
cfactor2 <- cfactor(sample(letters[3:16], 9, replace = TRUE), ordered = TRUE)

## no labels
# with braks
case1a <- cut(cfactor1, breaks = c("a", "q", "z"), right = TRUE, include.lowest = TRUE)
case2a <- cut(cfactor1, breaks = c("a", "q", "z"), right = FALSE, include.lowest = TRUE)
# without breaks
cut(cfactor1, breaks = c("a", "q", "z"), right = TRUE, include.lowest = TRUE)
# custom labels
case1b <- cut(cfactor1, breaks = c("a", "q", "z"),
              labels = c("group one", "group 2"), right = TRUE, include.lowest = TRUE)
case2b <- cut(cfactor1, breaks = c("a", "q", "z"),
              labels = c("a first group", "another one"),
              right = FALSE, include.lowest = TRUE)

# breaks as integer with no label
some_letters <- cfactor(sample(letters), ordered = TRUE)
case7a <- cut(some_letters, breaks = 2, include.lowest = TRUE)


# breaks of length 1
case3a <- cut(cfactor1, breaks = 2, labels = c("a first group", "another one"),
              right = FALSE, include.lowest = TRUE)
case3b <- cut(cfactor1, breaks = 2, labels = c("a first group", "another one"),
              right = TRUE, include.lowest = FALSE)

## labels
# labels = FALSE
case4a <- cut(cfactor1, breaks = 2, labels = FALSE,
              right = TRUE, include.lowest = TRUE)

## separator
case5a <- cut(cfactor1, breaks = 2, labels = NULL,
              right = TRUE, include.lowest = TRUE, label_sep = "|")

## binwidth 1


case6a <- cut(cfactor(sample(letters), ordered = TRUE), breaks = letters)
################################################################################

test_that("cut.ordered simple tests", {
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


  # labels = FALSE
  expect_error(case4a, NA)
  expect_equal(levels(case7a), c("a-m", "n-z"))
})



# specify breaks out of range
test_that("warnings", {
  # breaks that create missing values
  expect_warning(cut(cfactor1, breaks = c("a", "q", "y"),
                     right = TRUE, include.lowest = TRUE),
                 "[[:digit:]] missing values generated")
})

test_that("errors", {
  # breaks that do not exist in data
  expect_error(cut(cfactor2, breaks = c("a", "q", "y"),
                   right = TRUE, include.lowest = TRUE),
               "specified breakpoints inexistent in data")
  expect_error(cut(cfactor1, breaks = 2, labels = "a label",
                   right = TRUE, include.lowest = TRUE))
})

test_that("return type", {
  # returns ordered factor
  expect_factor(case1a, ordered = TRUE)
  expect_factor(case1b, ordered = TRUE)
  expect_factor(case2a, ordered = TRUE)
  expect_factor(case2b, ordered = TRUE)
  expect_factor(case3a, ordered = TRUE)
  expect_factor(case3b, ordered = TRUE)
  expect_factor(case5a, ordered = TRUE)
  expect_factor(case6a, ordered = TRUE)
  expect_factor(case7a, ordered = TRUE)
})

# binwidth 1
test_that("binwidth 1", {

  # get a warning with scalar breaks
  expect_warning(cut(cfactor(rep(letters, 2),
                             ordered = TRUE), 20),
                 "are: a, b, c, d, e, f, g, h, i, j, k, l, m, n")

  # don't get a waring with specified breakpoints
  expect_warning(cut(cfactor(rep(letters[1:20], 2), ordered = T), letters[1:20]), NA)
})
# when braks are integer, right argument does not work
