context("integer_cut")

int_norep <- sample(10, replace = FALSE)

case1 <- cut(int_norep, breaks = c(1, 5, 10), right = TRUE, include.lowest = FALSE)
case2 <- cut(int_norep, breaks = c(1, 5, 10), right = TRUE, include.lowest = TRUE)
case3 <- cut(int_norep, breaks = c(1, 5, 10), right = FALSE, include.lowest = FALSE)
case4 <- cut(int_norep, breaks = c(1, 5, 10), right = FALSE, include.lowest = TRUE)

case5 <- cut(int_norep, breaks = 2, right = FALSE)
case6 <- cut(int_norep, breaks = 2, right = TRUE)
case7 <- cut(int_norep, breaks = 3, right = FALSE)
case8 <- cut(int_norep, breaks = 3, right = TRUE)

int2_norep <- sample(15, replace = FALSE)

case9 <- cut(int2_norep, breaks = 3, right = FALSE)
case10 <- cut(int2_norep, breaks = 3, right = TRUE)
case11 <- cut(int2_norep, breaks = 4, right = FALSE)
case12 <- cut(int2_norep, breaks = 4, right = TRUE)
case13 <- cut(int2_norep, breaks = 5, right = FALSE)
case14 <- cut(int2_norep, breaks = 5, right = TRUE)

int3_norep <- sample(99, replace = FALSE)

case15 <- cut(int3_norep, breaks = 3, right = FALSE)
case16 <- cut(int3_norep, breaks = 3, right = TRUE)
case17 <- cut(int3_norep, breaks = 4, right = FALSE)
case18 <- cut(int3_norep, breaks = 4, right = TRUE)
case19 <- cut(int3_norep, breaks = 5, right = FALSE)
case20 <- cut(int3_norep, breaks = 5, right = TRUE)

# for extremely few values in x 
case21 <- cut(1L, breaks = c(0, 1, 9), include.lowest = TRUE)

# non-default labels
case22 <- cut(sample(10), breaks = 3, labels = letters[1:3])
case23 <- cut(sample(10), breaks = 3, labels = FALSE)

# for extremely few values in breaks
## breaks as scalar

case24 <- cut(sample(10), breaks = 1, labels = FALSE) # desired outcome although 
# not in line with cut.defalt
case25 <- cut(sample(10), breaks = 1, labels = NULL)
case26 <- cut(sample(10), breaks = 1, labels = "lion") 

# breaks as vector
case27 <- cut(sample(10), breaks = c(1, 10), labels = FALSE) 
case28 <- cut(sample(10), breaks = c(1, 10), labels = NULL)
case29 <- cut(sample(10), breaks = c(1, 10), labels = "lion")

# not desired outcome although it should be without 
# cut.default(sample(10), breaks = 1, labels = FALSE)

# when breaks are of class integer
case30 <- cut(sample(10), breaks = c(1L, 3L, 10L))

# when breaks need to be rounded
case31 <- cut(sample(10), breaks = c(1, 2.6, 5.1, 10))

test_that(paste("cut.integer returns same as cut.default but with better", 
                "labels for length(break) > 1"), {
  # right = TRUE
  expect_equal(levels(case1), c("2-5", "6-10"))
  expect_equal(levels(case2), c("1-5", "6-10"))
  
  # right = FALSE
  expect_equal(levels(case3), c("1-4", "5-9"))
  expect_equal(levels(case4), c("1-4", "5-10"))
  
  # extremely few break values
  expect_equal(case27, rep(1, 10))
  expect_equal(levels(case28), "1-10")
  expect_equal(levels(case29), "lion")
})



test_that(paste("cut.integer returns expected (natural) intervals with better",  
                "labels for length(break) == 1"), {
  
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
  
  # extremely few break values
  expect_equal(case24, rep(1, 10))
  expect_equal(levels(case25), "1-10")
  expect_equal(levels(case26), "lion")
  
  # when breaks are of class integer
  expect_equal(levels(case30), c("1-3", "4-10"))

  # when breaks need to be rounde
  expect_equal(levels(case31), c("1-2", "3-5", "6-10"))
})

test_that("cut.integer with user-defined labels", {
  expect_equal(levels(case22), c("a", "b", "c"))
  expect_equal(sort(unique(case23)), c(1, 2, 3))
})


# can't be assigned because of error. Created within testthat
test_that("cut.integer error cases", {
  
  ## should produce an error: length(breaks) == length(x) == 1
  expect_error(cut(1L, breaks = 2), 
               "if x is a scalar, breaks must be given in intervals")
  
  ## should not produce an error if breaks are already given
  expect_equal(levels(case21), c("0-1", "2-9"))
  
  ## should produce an error if breaks > length(x), since integer bins can't be 
  # created if bins should contain at least two integers.
  expect_error(cut(sample(2), breaks = 3), 
               "range too small for the number of breaks specified")
  expect_error(cut(sample(10), breaks = 3, labels = letters[1:4]), 
               paste("if labels not 'NULL' and not 'FALSE', it must be the same", 
                     "length as the number of bins resulting from 'breaks'"))
  expect_error(cut(sample(10), breaks = 3, labels = letters[1:99]), 
               paste("if labels not 'NULL' and not 'FALSE', it must be the same", 
                      "length as the number of bins resulting from 'breaks'"))
  # edge case
  expect_error(cut(sample(10), breaks = 1, labels = c("lion", "tiger")),
    paste("if labels not 'NULL' and not 'FALSE', it must be the same length as the", 
          "number of bins resulting from 'breaks'"))
  
})



test_that("cut.integer warning cases", {
  expect_warning(cut(sample(10), breaks = c(0, 4, 5)), 
                 "[[:digit:]]+ missing values generated$")
  
  expect_warning(cut(sample(10), breaks = c(10, 0, 3)), 
                 "^breaks were unsorted and are now sorted in the following order:")
  
  expect_warning(cut(sample(10), breaks = c(NA, 0, 10)), 
                 "missing values in breaks were removed$")
  
  # when breaks are to be rounded to coerce to integers
  expect_warning(cut(sample(10), breaks = c(1, 2.6, 5.1, 10)), 
                 "^When coerced to integers, the following breaks were truncated")
  
  # when bins with width 1 are produced
  expect_warning(cut(sample(10), breaks = c(1, 4, 6, 8, 9, 10)),
                 paste("^this break specification produces [[:digit:]]+", 
                       "bin\\(s\\) of width 1. The corresponding label\\(s\\)", 
                       "are: 9, 10"))
  
})




test_that("cut.integer if breaks outside range(x)", {
  # not yet finished
}) 

# what happens in binwidth 1 if none is this width (line 99)
# not optimal:
# cut(int_norep, breaks = c(1, 2, 3, 10), right = T, include.lowest = FALSE)