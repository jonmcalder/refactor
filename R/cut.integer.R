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
#' @examples Z <- stats::sample(10)
#' cut(Z, breaks = c(0, 5, 10))
#' @export
cut.integer <- function(x, breaks, include.lowest = FALSE,
                        right = TRUE, digit.lab = 3, ordered_result = FALSE, breaks.mode = "default", label.sep = "-", ...) {
  
  # if breaks are not specified (i.e. only the number of breaks is provided)
  if(length(breaks) == 1){
    
    # should the breaks be "pretty"? (‘round’ values which cover the range of the values in x)
    # or based on quantiles?
    # or evenly spaced over the range of the data? ("default")
    if(breaks.mode == "pretty"){
      breaks = pretty(x, breaks)
    } else if(breaks.mode == "quantile"){
      # not yet implemented
    } else if(breaks.mode == "default"){
      breaks = round(seq(from=min(x), to=max(x), length.out = breaks+1))
    } else {
      stop("breaks.mode needs to be either 'default', 'pretty' or 'quantile'")
    }
    
  }
  
  numLabels = length(breaks)-1
  
  # handle break offsets for 'right' and 'left' intervals
  # and also handle include.lowest = TRUE
  if(right == TRUE){
    floorInc    = rep(1, numLabels)
    ceilingDec  = rep(0, numLabels)
    if(include.lowest == TRUE){
      floorInc[1] = 0  
    }
  } else {
    floorInc    = rep(0, numLabels)
    ceilingDec  = rep(1, numLabels)
    if(include.lowest == TRUE){
      ceilingDec[numLabels] = 0  
    }
  }
  
  # create integer-based interval labels using label.sep
  recodeLabels = paste(head(breaks, -1) + floorInc, tail(breaks, -1) - ceilingDec, sep = label.sep)
  
  cut.default(x, breaks = breaks, labels = recodeLabels, include.lowest = include.lowest,
                           right = right, digit.lab = digit.lab, ordered_result = ordered_result, ...)

}
