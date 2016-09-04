context("integer_cut")

int_norep <- sample(10, replace = F)

case1 <- cut(int_norep, breaks = c(1, 5, 10), right = T)
case2 <- cut(int_norep, breaks = c(1, 5, 10), right = T, include.lowest = T)
case3 <- cut(int_norep, breaks = c(1, 5, 10), right = F)
case4 <- cut(int_norep, breaks = c(1, 5, 10), right = F, include.lowest = T)

case5 <- cut(int_norep, breaks = 2, balance = "left")
case6 <- cut(int_norep, breaks = 2, balance = "right")
case7 <- cut(int_norep, breaks = 3, balance = "left")
case8 <- cut(int_norep, breaks = 3, balance = "right")

int2_norep <- sample(15, replace = F)

case9 <- cut(int2_norep, breaks = 3, balance = "left")
case10 <- cut(int2_norep, breaks = 3, balance = "right")
case11 <- cut(int2_norep, breaks = 4, balance = "left")
case12 <- cut(int2_norep, breaks = 4, balance = "right")
case13 <- cut(int2_norep, breaks = 5, balance = "left")
case14 <- cut(int2_norep, breaks = 5, balance = "right")

int3_norep <- sample(99, replace = F)

case15 <- cut(int3_norep, breaks = 3, balance = "left")
case16 <- cut(int3_norep, breaks = 3, balance = "right")
case17 <- cut(int3_norep, breaks = 4, balance = "left")
case18 <- cut(int3_norep, breaks = 4, balance = "right")
case19 <- cut(int3_norep, breaks = 5, balance = "left")
case20 <- cut(int3_norep, breaks = 5, balance = "right")

# for extremely few values
case21 <- cut(1L, breaks = c(0, 1, 9), include.lowest = T)


test_that("cut.integer returns same as cut.default but with better labels for length(break) > 1", {
  # right = T
  expect_equal(levels(case1), c("2-5", "6-10"))
  expect_equal(levels(case2), c("1-5", "6-10"))
  
  # right = F
  expect_equal(levels(case3), c("1-4", "5-9"))
  expect_equal(levels(case4), c("1-4", "5-10"))
})



test_that("cut.integer returns expected (natural) intervals with better labels for length(break) == 1", {
  
  # 1:10 - 2 breaks ("left & "right")
  expect_equal(levels(case5), c("1-5", "6-10"))
  expect_equal(levels(case6), c("1-5", "6-10"))
  
  # 1:10 - 3 breaks ("left & "right")
  expect_equal(levels(case7), c("1-4", "5-7", "8-10"))
  expect_equal(levels(case8), c("1-3", "4-6", "7-10"))
  
  # 1:15 - 3 breaks ("left & "right")
  expect_equal(levels(case9), c("1-5", "6-10", "11-15"))
  expect_equal(levels(case10), c("1-5", "6-10", "11-15"))
  
  # 1:15 - 4 breaks ("left & "right")
  expect_equal(levels(case11), c("1-4", "5-8", "9-12", "13-15"))
  expect_equal(levels(case12), c("1-3", "4-7", "8-11", "12-15"))
  
  # 1:15 - 5 breaks ("left & "right")
  expect_equal(levels(case13), c("1-3", "4-6", "7-9", "10-12", "13-15"))
  expect_equal(levels(case14), c("1-3", "4-6", "7-9", "10-12", "13-15"))
  
  # 1:99 - 3 breaks ("left & "right")
  expect_equal(levels(case15), c("1-33", "34-66", "67-99"))
  expect_equal(levels(case16), c("1-33", "34-66", "67-99"))
  
  # 1:99 - 4 breaks ("left & "right")
  expect_equal(levels(case17), c("1-25", "26-50", "51-75", "76-99"))
  expect_equal(levels(case18), c("1-24", "25-49", "50-74", "75-99"))
  
  # 1:99 - 5 breaks ("left & "right")
  expect_equal(levels(case19), c("1-20", "21-40", "41-60", "61-80", "81-99"))
  expect_equal(levels(case20), c("1-19", "20-39", "40-59", "60-79", "80-99"))

})

# can't be assigned because of error. Created within testthat
test_that("cut.integer error cases", {
  
  ## should produce an error: length(breaks) == length(x) == 1
  expect_error(cut(1L, breaks = 2), 
               "if x is a scalar, breaks must be given in intervals")
  
  ## should not produce an error if breaks are already given
  expect_equal(levels(case21), c("0-1", "2-9"))
  
  ## should produce an error if breaks > length(x), since integer bins can't be created if bins should contain at least two integers.
  expect_error(cut(sample(2), breaks = 3), 
               "range too small for the number of breaks specified")

})

test_that("cut.integer if breaks outside range(x)", {
  
}) 