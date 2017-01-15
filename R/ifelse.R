#' Conditional Element Selection
#' 
#' @inheritParams base::ifelse
#' @param ... further arguments to be passed to \code{\link{cfactor}}.
#' @details A refined version of \code{\link[base]{ifelse}} that allows for
#'   factor appending.
#' @return 
#'   If either \code{yes} or \code{no} are of class \code{factor}, 
#'   the function returns an object of class \code{factor} whereas levels are 
#'   determined using \code{\link{cfactor}} internally. Otherwise, the base 
#'   method is called directly.
#' @examples 
#' f <- factor(c('c','a','a',NA,'b','a'), levels= c('a','b','c'))
#' base::ifelse(is.na(f), "g", f) # not nice
#' refactor::ifelse(is.na(f), "g", f) # probably what you wanted

#' @export

ifelse <- function (test, yes, no, ...) {
  was_factor <- FALSE
  if (is.factor(yes)) {
    yes <- as.character(yes)
    was_factor <- TRUE
  }
  if (is.factor(no)) {
    no <- as.character(no)
    was_factor <- TRUE
  }
  out <- base::ifelse(test, yes, no)
  if(was_factor) {
    cfactor(out)
  } else {
    out
  }
}
