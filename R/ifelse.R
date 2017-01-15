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
  levels <- ""
  # a funciton that test whether x (interpreted as a name) is a factor and 
  # if yes, it is converted to character in the parent environment
  test_factor <- function(x) {
    if (is.factor(eval(as.name(x)))) {
      assign(x, as.character(eval(as.name(x))), 
             envir = parent.env(environment()))
      assign("was_factor", TRUE, 
             envir = parent.env(environment()))
    }
    assign("levels", unique(c(eval(as.name(x)), levels)), 
           envir = parent.env(environment()))
  }
  lapply(list("yes", "no"), test_factor)
  
  out <- base::ifelse(test, yes, no)
  if(was_factor) {
    cfactor(out, levels = levels)
  } else {
    out
  }
}
