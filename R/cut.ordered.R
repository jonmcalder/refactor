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
#' cut(Z, breaks = -6:6)
#' @export
cut.ordered <- function(x, breaks, labels = NULL, include.lowest = FALSE,
                        right = TRUE, digit.lab = 3, ordered_result = FALSE, breaks_mode = "default", label_sep = "-", ...) {
  xnum <- as.numeric(x)
  x_lev <- levels(x)
  breakpos <- match(breaks, x_lev)
  
  
  ######## 
  # if breaks are not specified (i.e. only the number of breaks is provided)
  if(length(breaks) == 1){
    
    numLabels <- breaks
    
    # should the breaks be "pretty"? (‘round’ values which cover the range of the values in x_num)
    # or based on quantiles?
    # or evenly spaced over the range of the data? ("default")
    if(breaks_mode == "quantile"){
      
      # not yet implemented
      
    } else if(breaks_mode == "default"){
      
      range <- max(x_num)-min(x_num)+1
      avg_bin_width <- floor(range/breaks)
      rem <- range %% breaks
      num <- breaks+1
      if(rem == 0){
        breakpoints <- seq(from=min(x_num)-1, by = avg_bin_width, length.out = num)
        breakpoints[1] <- min(x_num)
      } else if(rem != 0) {
        if(right == FALSE){
          breakpoints <- rev(seq(from=max(x_num), by = -avg_bin_width, length.out = num))
          breakpoints[1] <- min(x_num)
          for(i in 1:rem){
            breakpoints[i+1] <- min(x_num)-1+avg_bin_width*i+i
          }
        } else if(right == TRUE){
          breakpoints <- seq(from=min(x_num)-1, by = avg_bin_width, length.out = num)
          breakpoints[1] <- min(x_num)
          breakpoints[num] <- max(x_num)
          for(i in num:(num-rem+1)){
            breakpoints[i-1] <- max(x_num)-(avg_bin_width+1)*(num-i+1)
          }
        }
      }
    }
    
    right <- TRUE
    
    
    # use breakpoints as is if provided  
  } else if(length(breaks > 1)){
    
    breakpoints <- breaks
    
    numLabels <- length(breakpoints) - 1
    
  }
  
  # handle break offsets for 'right' and 'left' intervals
  # and also handle include.lowest = TRUE
  if(right == TRUE){
    floorInc    <- rep(1, numLabels)
    ceilingDec  <- rep(0, numLabels)
    if(include.lowest == TRUE){
      floorInc[1] <- 0
    }
  } else if(right == FALSE) {
    floorInc    <- rep(0, numLabels)
    ceilingDec  <- rep(1, numLabels)
    if(include.lowest == TRUE){
      ceilingDec[numLabels] <- 0
    }
  }
  
  # create integer-based interval labels using label_sep
  if(is.null(labels)) {
    recode_labels <- paste(x_lev[head(breakpos, -1) + floorInc], x_lev[tail(breakpos, -1) - ceilingDec], sep = label_sep) 
    
    # correct labels with binwidth 1, that is where to elements separated by label_sep are the same, i.e. the label "10-10"
    # deactivated
    same <- head(breakpos, -1) + floorInc == tail(breakpos, -1) - ceilingDec
    # recode_labels[same] <- (tail(breakpos, -1) - ceilingDec)[same] 
    
  } else if(!is.null(labels)) {
    if(length(labels) == length(breakpoints) - 1) {
      recode_labels <- labels
    } else if(length(labels) != length(breakpoints) - 1) {
      if(length(labels) == 1) {
        if(labels == F) {
          recode_labels <- labels
        } else if(labels != F) {
          stop("if labels not 'NULL' and not 'F', it must be the same length as the number of bins resulting from 'breaks'")
        }
      } else if(length(labels) != 1) {
        stop("if labels not 'NULL' and not 'F', it must be the same length as the number of bins resulting from 'breaks'")
      }
      
    }
  }
  output <- cut.default(xnum, breaks = breakpos, labels = recode_labels, include.lowest = include.lowest,
                        right = right, ordered_result = ordered_result, ...)
  
  
  if(anyNA(output)) {
    warning(paste(sum(is.na(output)), "missing values generated"))
  }
  output
}
