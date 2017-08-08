#' Enhanced control for combining values
#'
#' @inheritParams base::c
#' @details This function returns the same as base \code{\link{c}}, but if all
#'  elements of \code{...} are of class factor or ordered, it returns the lowest
#'  common denominator, i.e. an object of class ordered or factor.
#'  Ordering of the levels (also for unordered factors) is only preserved if it
#'  is identical accross all element in \code{...}. Otherwise, it is determined
#'  by \code{\link{cfactor}}, which is called internarlly. Refer to the section
#' 'Details' in the \code{\link{cfactor}} documentation for further details.
#' @examples
#' char1 <- as.factor(sample(letters, 5))
#' char2 <- as.factor(sample(letters, 5))
#' cc(char1, char2) # probably more reasonable than
#' c(char1, char2)
#' \dontrun{
#' ## warnings
#' # no warning since levels identical
#' a_to_d <- cfactor(letters[1:2], levels = letters[1:5])
#' b_to_d <- cfactor(letters[1:5])
#' cc(a_to_d, b_to_d)
#'
#' # warning for differing order of levels
#' a_to_d <- cfactor(letters[1:2], levels = letters[1:5])
#' d_to_a <- cfactor(letters[1:2], levels = rev(letters[1:5]))
#' cc(a_to_d, d_to_a)
#'
#' # warning for different levels
#' some_levels <- cfactor(letters[1:2], levels = letters[1:5])
#' all_levels <- cfactor(letters[1:2], levels = letters)
#' cc(all_levels, some_levels, some_levels)
#' }
#' @export
cc <- function(..., recursive = FALSE) {
  dots <- list(...)
  dots_chars <- lapply(dots, FUN = as.character)

  levels <- lapply(dots, function(x) levels(x))

  levels_identical <- Reduce(identical, levels)
  levels_same_set <- Reduce(setequal, levels)
##  ............................................................................
##  test whether objects inherit from ordered or factor
  dots_ordered <- vapply(dots, function(x) inherits(x, "ordered"), logical(1))
  dots_factor <- vapply(dots, function(x) inherits(x, "factor"), logical(1))

  if (all(dots_factor)) { # if all elements are at least factors
    if (levels_same_set) {
      if (levels_identical) {
        return(cfactor(unlist(dots_chars), levels[[1]], ordered = all(dots_ordered)))
      } else if (!levels_identical) {
        warning("ordering not preserved since ordering of levels not identical",
                call. = FALSE)
        return(cfactor(unlist(dots_chars), ordered = FALSE))
      }
    } else if (!levels_same_set) {
      warning("ordering and levels not preserved since levels not identical",
              call. = FALSE)
      return(cfactor(unlist(dots_chars), ordered = FALSE))
    }
  } else {
##  ............................................................................
##  it's neither factorial nor ordered, so we simply call base::c
    return(c(unlist(dots)))
  }
}
