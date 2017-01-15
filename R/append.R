#' Vector Merging
#' 
#' @inheritParams base::append
#' @details A refined version of \code{\link[base]{append}} that allows for
#'   factor appending.
#' @return 
#'   If \code{x} and \code{values} are of class \code{factor}, 
#'   the function returns an object of class \code{factor} whereas levels are 
#'   determined using \code{\link{cfactor}} internally. Levels are only
#'   preserved if both elements have the same levels. In addition, ordering
#'   is only preserved if the order is also identical.
#'   Otherwise, the base 
#'   method is called directly.
#' @examples 
#' f <- factor(c('c','a','a',NA,'b','a'), levels= c('a','b','c'))
#' g <- factor(sample(letters[4:10]), levels = sample(letters[4:10]))
#' base::append(f, g) # not nice
#' refactor::append(f, g) # probably what you want
#' @export

append <- function (x, values, after = length(x)) {
  count <- 0
  
  if(is.factor(x)) {
    lvls_x <- levels(x) 
    is_ordered_x <- is.ordered(x)
    x <- as.character(x)
    count <- count + 1 #
  }
  
  if(is.factor(values)) {
    lvls_values <- levels(values)
    is_ordered_values <- is.ordered(values)
    values <- as.character(values)
    count <- count + 1
  }

  out <- base::append(x, values, after)
  
  if(count == 2) { # returns factor if both were factors
    # if levels are the same, preserve them
    if (identical(lvls_x, lvls_values)) {
      
    # if order of the levels is the same, preserve it.
    cfactor(out, levels = lvls_x, ordered = all(is_ordered_x, is_ordered_values))
    } else {
      cfactor(out)
    }
  } else {
    out
  }
}
