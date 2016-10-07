context("integer_cut (default)")

context("integer_cut (spread)")

int_norep <- sample(10, replace = FALSE)

case1 <- cut(int_norep, breaks = c(1, 5, 10), right = TRUE, 
             include.lowest = FALSE, breaks_mode = "spread")
case2 <- cut(int_norep, breaks = c(1, 5, 10), right = TRUE, 
             include.lowest = TRUE, breaks_mode = "spread")
case3 <- cut(int_norep, breaks = c(1, 5, 10), right = FALSE, 
             include.lowest = FALSE, breaks_mode = "spread")
case4 <- cut(int_norep, breaks = c(1, 5, 10), right = FALSE, 
             include.lowest = TRUE, breaks_mode = "spread")

case5 <- cut(int_norep, breaks = 2, right = FALSE, breaks_mode = "spread")
case6 <- cut(int_norep, breaks = 2, right = TRUE, breaks_mode = "spread")
case7 <- cut(int_norep, breaks = 3, right = FALSE, breaks_mode = "spread")
case8 <- cut(int_norep, breaks = 3, right = TRUE, breaks_mode = "spread")

int2_norep <- sample(15, replace = FALSE)

case9 <- cut(int2_norep, breaks = 3, right = FALSE, breaks_mode = "spread")
case10 <- cut(int2_norep, breaks = 3, right = TRUE, breaks_mode = "spread")
case11 <- cut(int2_norep, breaks = 4, right = FALSE, breaks_mode = "spread")
case12 <- cut(int2_norep, breaks = 4, right = TRUE, breaks_mode = "spread")
case13 <- cut(int2_norep, breaks = 5, right = FALSE, breaks_mode = "spread")
case14 <- cut(int2_norep, breaks = 5, right = TRUE, breaks_mode = "spread")

int3_norep <- sample(99, replace = FALSE)

case15 <- cut(int3_norep, breaks = 3, right = FALSE, breaks_mode = "spread")
case16 <- cut(int3_norep, breaks = 3, right = TRUE, breaks_mode = "spread")
case17 <- cut(int3_norep, breaks = 4, right = FALSE, breaks_mode = "spread")
case18 <- cut(int3_norep, breaks = 4, right = TRUE, breaks_mode = "spread")
case19 <- cut(int3_norep, breaks = 5, right = FALSE, breaks_mode = "spread")
case20 <- cut(int3_norep, breaks = 5, right = TRUE, breaks_mode = "spread")

# for extremely few values in x 
case21 <- cut(1L, breaks = c(0, 1, 9), include.lowest = TRUE, 
              breaks_mode = "spread")

# non-default labels
case22 <- cut(sample(10), breaks = 3, labels = letters[1:3], breaks_mode = "spread")
case23 <- cut(sample(10), breaks = 3, labels = FALSE, breaks_mode = "spread")

# for extremely few values in breaks
## breaks as scalar

# desired outcome although not in line with cut.defalt
case24 <- cut(sample(10), breaks = 1, labels = FALSE, breaks_mode = "spread") 
case25 <- cut(sample(10), breaks = 1, labels = NULL, breaks_mode = "spread")
case26 <- cut(sample(10), breaks = 1, labels = "lion", breaks_mode = "spread") 

# breaks as vector
case27 <- cut(sample(10), breaks = c(1, 10), labels = FALSE, 
              breaks_mode = "spread") 
case28 <- cut(sample(10), breaks = c(1, 10), labels = NULL, 
              breaks_mode = "spread")
case29 <- cut(sample(10), breaks = c(1, 10), labels = "lion", 
              breaks_mode = "spread")

# not desired outcome although it should be without 
# cut.default(sample(10), breaks = 1, labels = FALSE)

# when breaks are of class integer
case30 <- cut(sample(10), breaks = c(1L, 3L, 10L), breaks_mode = "spread")

# when breaks need to be rounded
case31 <- cut(sample(10), breaks = c(1, 2.6, 5.1, 10), breaks_mode = "spread")


## where binwidth is 1
case32a <- cut(1:10, breaks = 9, right = FALSE, breaks_mode = "spread")


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
  
  # when bins with width 1 are produced from breaks, this does not produce a warning
  expect_warning(cut(sample(10), breaks = c(1, 4, 6, 8, 9, 10)), NA)
  
})
test_that("binwidth 1", {
  # the level itself
  expect_equal(levels(case32a),
               c("1-2", "3", "4", "5", "6", "7", "8", "9", "10"))
  
  # the warning that goes with it for right = FALSE
  ## x is even
  expect_warning(cut(1:10, breaks = 9, right = FALSE, breaks_mode = "spread"), 
                "are: 3, 4, 5, 6, 7, 8, 9, 10")
  ## x is odd
  expect_warning(cut(1:11, breaks = 9, right = FALSE, breaks_mode = "spread"), 
                 "are: 5, 6, 7, 8, 9, 10, 11")
  # the waring that goes with it for right = TRUE
  ## breaks are even
  expect_warning(cut(1:10, breaks = 8, right = TRUE, breaks_mode = "spread"), 
                 "are: 1, 2, 3, 4, 5, 6")
})



test_that("cut.integer if breaks outside range(x)", {
  # not yet finished
}) 

# what happens in binwidth 1 if none is this width (line 99)
# not optimal:
# cut(int_norep, breaks = c(1, 2, 3, 10), right = T, include.lowest = FALSE)

context("integer_cut (pretty)")