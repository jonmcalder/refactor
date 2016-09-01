context("integer_cut")

int_rep <- sample(10, replace = T)
int_norep <- sample(10, replace = F)
num <- runif(10, 1, 10)

# see what cut.default returns
cut(num, breaks = c(1, 5, 10), right = T)
cut(num, breaks = c(1, 5, 10), right = T, include.lowest = T)
cut(num, breaks = c(1, 5, 10), right = F)
cut(num, breaks = c(1, 5, 10), right = F, include.lowest = T)

cut(int_norep, breaks = c(1, 5, 10), right = T)
cut(int_norep, breaks = c(1, 5, 10), right = T, include.lowest = T)
cut(int_norep, breaks = c(1, 5, 10), right = F)
cut(int_norep, breaks = c(1, 5, 10), right = F, include.lowest = T)

test_that("cut.integer returns same as cut.default but pretty lables for length(break) > 1", {
  # right = T
  expect_equal(levels(cut(int_norep, breaks = c(1, 5, 10), right = T, include.lowest = F)), c("2-5", "6-10"))
  expect_equal(levels(cut(int_norep, breaks = c(1, 5, 10), right = T, include.lowest = T)), c("1-5", "6-10"))
  
  # right = F
  expect_equal(levels(cut(int_norep, breaks = c(1, 5, 10), right = F, include.lowest = F)), c("1-4", "5-9"))
  expect_equal(levels(cut(int_norep, breaks = c(1, 5, 10), right = F, include.lowest = T)), c("1-4", "5-10"))
})



test_that("cut.integer returns same as cut.default but pretty lables for length(break) == 1", {
  # right = T
  expect_equal(levels(cut(int_norep, breaks = 3, right = T, include.lowest = F)), c("2-5", "6-10"))
  expect_equal(levels(cut(int_norep, breaks = 3, right = T, include.lowest = T)), c("1-5", "6-10"))
  
  # right = F
  expect_equal(levels(cut(int_norep, breaks = 3, right = F, include.lowest = F)), c("1-4", "5-9"))
  expect_equal(levels(cut(int_norep, breaks = 3, right = F, include.lowest = T)), c("1-4", "5-10"))
})