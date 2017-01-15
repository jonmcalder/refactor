#' Combine Values into an (ordered) Factor
#' 
#' @inheritParams base::c
#' @details Note that the order of the levels is internally determined by
#' \code{\link{cfactor}} and thus its ordering scheme. Refer to the section 
#' 'Details' in the \code{\link{cfactor}} documentation.
#' @examples
#' char1 <- as.factor(sample(letters, 5))
#' char2 <- as.factor(sample(letters, 5))
#' cc(char1, char2)
#' @export
cc <- function(..., recursive = FALSE){
  dots = list(...)

  dots_chars <- lapply(dots, FUN = as.character)
  
##  ............................................................................
##  test whether objects inherit from ordered
  dots_ordered <- vapply(dots, function(x) inherits(x, "ordered"), logical(1))
  if (all(dots_ordered)) {
    # ok, its all ordered. Keep as ordered if all levels and their ordering is 
    # the same
    levels <- lapply(dots, function(x) levels(x))
    levels_identical <- Reduce(identical, levels)
    if (levels_identical) {
      return(cfactor(unlist(dots_chars), levels[[1]], ordered = TRUE))
    } else {
      warning("ordering not preserved since not all levels identical", 
              call. = FALSE)
      return(cfactor(unlist(dots_chars), ordered = FALSE))
    }
  }
  
##  ............................................................................
##  test whether objects inherit from factor
  dots_factor <- vapply(dots, function(x) inherits(x, "factor"), logical(1))
  if (all(dots_factor)) {
    # ok, its all factorial
    return(cfactor(unlist(dots_chars), ordered = FALSE))
  }
  
##  ............................................................................
##  it's not factorial nor ordered, so we simply call base::c
  return(c(unlist(dots_chars)))
}
