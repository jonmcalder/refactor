library("testthat")

context("ordered_cut")
cfactor1 <- cfactor(sample(letters, 100, replace = T), ordered = T)
cfactor2 <- cfactor(sample(letters[3:16], 100, replace = T), ordered = T)


# breaks_mode = 'default'

## no labels
case1a <- cut(cfactor1, breaks = c("a", "q", "z"), right = T, include.lowest = T)
case2a <- cut(cfactor1, breaks = c("a", "q", "z"), right = F, include.lowest = T)

# custom labels
case1b <- cut(cfactor1, breaks = c("a", "q", "z"), labels = c("group one", "group 2"), right = T, include.lowest = T)
case2b <- cut(cfactor1, breaks = c("a", "q", "z"), labels = c("a first group", "another one"), right = F, include.lowest = T)


test_that("cut.ordered with breaks_mode = 'default'", {
  # simple cases
  expect_equal(levels(case1a), c("a-q", "r-z"))
  expect_equal(levels(case2a), c("a-p", "q-z"))
  expect_equal(levels(case1b), c("group one", "group 2"))
  expect_equal(levels(case2b), c("a first group", "another one"))

  # simple cases
  q_pos <- which(letters) == "q"
  expect_equal(case1b[1:q_pos], rep("group one", 1:q_pos))
  expect_equal(case1b[q_pos+1:length(case1b)], rep("group 2", (q_pos+1):length(case1b)))
  expect_equal(levels(case2b), c("a-r", "s-z"))

})



# specify breaks out of range
test_that("warnings", {
  expect_warning(cut(cfactor1, breaks = c("a", "q", "y"), right = T, include.lowest = T), 
                 "missing values generated")
})
