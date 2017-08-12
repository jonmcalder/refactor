context("test append")
f_char <- c("c", "a", "a", "b", "a")
f_fact <- cfactor(c("c", "a", "a", "b", "a"), levels = c("a", "b", "c"))
g_fact <- cfactor(letters[4:10], levels = letters[4:10])
g_char <- letters[4:10]

h_char <- c("c", NA, "a", "a", "b", "a")
h_fact <- cfactor(c("c", NA, "a", "a", "b", "a"), levels = c("a", "b", "c"))

i_fact <- cfactor(c("a", "b", "c"), levels = letters[3:1])

j_fact <- cfactor(c("a", "b", "c"), levels = letters[3:1], ordered = TRUE)
k_fact <- cfactor(c("a", "b", "c"), levels = letters[1:3], ordered = TRUE)
l_fact <- cfactor(c("b", "b", "c", "a"), levels = letters[1:3], ordered = TRUE)

test_that("append returns factor if x and values are factor", {
  expect_equal(class(append(f_fact, g_fact)), "factor")
  expect_equal(class(append(f_fact, g_char)), "character")
  expect_equal(class(append(f_char, g_char)), "character")
})

test_that("level and order preserving works fine", {
  # levels preserved when identical
  expect_equal(levels(i_fact), levels(append(i_fact, i_fact)))

  # levels ordered when identical and ordered
  expect_factor(append(l_fact, k_fact), ordered = TRUE)

  # levels not preserved when not identical
  expect_factor(append(j_fact, k_fact), levels = c("a", "b", "c"))
  # levels not ordered when not identical
  expect_factor(append(j_fact, k_fact), ordered = FALSE)
})


# further tests:
# - ordering / interaction with cfactor
