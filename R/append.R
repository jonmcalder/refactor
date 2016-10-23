#' Vector Merging
#' 
#' @inheritParams base::append
#' @param ... further arguments to be passed to \code{\link{cfactor}}.
#' @details A refined version of \code{\link[base]{append}} that allows for
#'   factor appending.
#' @return 
#'   If either \code{x} or \code{values} are of class \code{factor}, 
#'   the function returns an object of class \code{factor} whereas levels are 
#'   determined using \code{\link{cfactor}} internally. Otherwise, the base 
#'   method is called directly.
#' @examples 
#' f <- factor(c('c','a','a',NA,'b','a'), levels= c('a','b','c'))
#' g <- factor(sample(letters[4:10]), levels = sample(letters[4:10]))
#' base::append(f, g) # not nice
#' refactor::append(f, g) # probably what you want
#' @export

append <- function (x, values, after = length(x), ...) {
  was_factor <- FALSE
  if(is.factor(x)) {
    was_factor <- TRUE 
    x <- as.character(x)
  }
  if(is.factor(values)) {
    was_factor <- TRUE 
    values <- as.character(values)
  }
  out <- base::append(x, values, after)
  if(was_factor) {
    cfactor(out, ...)
  } else {
    out
  }
}
