context("cfactor")

x_factor <- cfactor(rep("x", 5))
y_factor <- cfactor(letters, labels = "letter")
z_factor <- cfactor(sample(letters, size = 400, replace = TRUE), levels = letters)

test_that("cfactor returns a factor", {
  expect_output(str(x_factor), "Factor")
  expect_output(str(y_factor), "Factor")
  expect_output(str(z_factor), "Factor")
})
