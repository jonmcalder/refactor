context("integer_cut")



# num <- runif(10, 1, 10)
# 
# # see what cut.default returns
# cut(num, breaks = c(1, 5, 10), right = T)
# cut(num, breaks = c(1, 5, 10), right = T, include.lowest = T)
# cut(num, breaks = c(1, 5, 10), right = F)
# cut(num, breaks = c(1, 5, 10), right = F, include.lowest = T)

int_norep <- sample(10, replace = F)

c1 = cut(int_norep, breaks = c(1, 5, 10), right = T)
c2 = cut(int_norep, breaks = c(1, 5, 10), right = T, include.lowest = T)
c3 = cut(int_norep, breaks = c(1, 5, 10), right = F)
c4 = cut(int_norep, breaks = c(1, 5, 10), right = F, include.lowest = T)

c5 = cut(int_norep, breaks = 2, right = T)
c6 = cut(int_norep, breaks = 2, right = T, include.lowest = T)
c7 = cut(int_norep, breaks = 2, right = F)
c8 = cut(int_norep, breaks = 2, right = F, include.lowest = T)

test_that("cut.integer returns same as cut.default but pretty lables for length(break) > 1", {
  # right = T
  expect_equal(levels(c1), c("2-5", "6-10"))
  expect_equal(levels(c2), c("1-5", "6-10"))
  
  # right = F
  expect_equal(levels(c3), c("1-4", "5-9"))
  expect_equal(levels(c4), c("1-4", "5-10"))
})



test_that("cut.integer returns same as cut.default but pretty lables for length(break) == 1", {
  # right = T
  expect_equal(levels(c5), c("2-5", "6-10"))
  expect_equal(levels(c6), c("1-5", "6-10"))
  
  # right = F
  expect_equal(levels(c7), c("1-4", "5-9"))
  expect_equal(levels(c8), c("1-4", "5-10"))
})