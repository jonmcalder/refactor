#' Convert Numeric to Factor
#'
#' cut divides the range of x into intervals and codes the values in x according to the interval they fall into.
#'
#' @param x A numeric vector which is to be converted to a factor by cutting.
#' @param breaks Either a numeric vector of two or more unique cut points or a single number (greater than or equal to 2) giving the
#' number of intervals into which x is to be cut.
#' @param labels Labels for the levels of the resulting category. By default, labels are constructed using "(a,b]" interval notation.
#' If labels = FALSE, simple integer codes are returned instead of a factor.
#' @param include.lowest Logical, indicating if an ‘x[i]’ equal to the lowest (or highest, for right = FALSE) ‘breaks’ value should be
#' included.
#' @param right	Logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
#' @param digit.lab	Integer which is used when labels are not given. It determines the number of digits used in formatting the break
#' numbers.
#' @param ordered_result Logical: should the result be an ordered factor?
#' @return A factor is returned, unless labels = FALSE which results in an integer vector of level codes.
#' @examples Z <- stats::rnorm(10000)
#' cut.integer(Z, breaks = -6:6)
cut.integer <- function(x, breaks, labels = NULL, include.lowest = FALSE,
                        right = TRUE, digit.lab = 3, ordered_result = FALSE, ...) {
  b1 <- breaks
  b2 <- breaks - 1
  if(right == T) {
    shift <- 1
  } else {
    shift <- 0
  }
  lab <- sapply(seq_len(length(b1)-1), function(i) paste0(b1[i]+shift, " - ", b2[i + 1]+shift, sep = ""))

  cut.default(x, breaks = breaks, labels = lab, include.lowest = include.lowest,
              right = right, digit.lab = digit.lab, ordered_result = ordered_result, ...)

}
