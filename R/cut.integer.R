#' Convert Numeric to Factor
#'
#' cut divides the range of x into intervals and codes the values in x according to the interval they fall into.
#'
#' @param x A numeric vector which is to be converted to a factor by cutting.
#' @param breaks Either an integer vector of two or more unique cut points or a single integer (greater than or equal to 2) giving the
#'  number of intervals into which x is to be cut.
#' @param labels Labels for the levels of the resulting category. By default, labels are constructed using "a-b c-d" interval notation.
#'  If labels = FALSE, simple integer codes are returned instead of a factor.
#' @param include.lowest Logical, indicating if an "x[i]" equal to the lowest (or highest, for right = FALSE) "breaks" value should be
#'  included. Note that unlike \link[base]{cut.default}, here include.lowest defaults to TRUE, since this is more intuitive for integer 
#'  intervals.
#' @param right	Logical, indicating how to create the bins. This is utilized in two different ways based on the type of breaks argument. 
#'  In the conventional case, where a breaks vector is supplied, right = TRUE indicates that bins should be closed on the right (and open 
#'  on the left) or vice versa. If a single integer breaks value is provided, then right = TRUE indicates that bins will be determined 
#'  such that those on the right are larger (if it is not possible for all bins to be evenly sized).
#' @param ordered_result Logical: should the result be an ordered factor?
#' @param breaks_mode A parameter indicating how to determine the intervals when breaks is specified as 
#'  a scalar. \itemize{
#'  \item 'default' will result in intervals spread as evenly as possible over the exact range of x
#'  \item 'pretty' will generate rounded breakpoints for the intervals (often extending slightly beyond the range of x) based on 
#'  \link[base]{pretty}
#' }
#' @param label_sep A single or short character string used to generate labels for the intervals e.g. the default value of "-" 
#'  will result in labels like 1-10 11-20 21-30 etc
#' @return A factor is returned, unless labels = FALSE which results in an integer vector of level codes.
#' @examples Z <- sample(10)
#'  cut(Z, breaks = c(0, 5, 10))
#' @export

cut.integer <- function(x, breaks, labels = NULL, include.lowest = TRUE, right = TRUE, ordered_result = FALSE,
                        breaks_mode = "default", label_sep = "-", ...) {
  
  # check function arguments
  assert_class(x, "integer")
  
  # breaks are either numeric or integer
  assert(
    test_class(breaks, "numeric"),
    test_class(breaks, "integer")
  )

  assert_class(include.lowest, "logical")
  assert_class(right, "logical")
  assert_class(ordered_result, "logical")
  assert_choice(breaks_mode, c("default", "pretty"))
  assert_class(label_sep, "character")
  
  # NAs in breaks
  if(anyNA(breaks)) {
    breaks <- na.omit(breaks)
    warning("missing values in breaks were removed")
  }
  
  # corece breaks to integers
  new_breaks <- floor(breaks)
  if(!setequal(new_breaks, breaks)){
    differ <- new_breaks != breaks
    warning(paste("When coerced to integers, the following breaks were truncated (rounded down): \n ", 
          paste(paste(breaks[differ], "to", new_breaks[differ]), " \n ", collapse = " ")))
          
    breaks <- new_breaks
  }
  
  # unsorted breaks
  if(is.unsorted(breaks)){
    breaks <- sort(breaks)
    warning(paste("breaks were unsorted and are now sorted in the following order:", paste0(breaks, collapse = " ")))
  }
  
  # breaks that create bins of width 1
  n_of_1bins <- diff(breaks)[-1] == 1
  if(sum(n_of_1bins > 0)) {
    warning(paste("this break specification produces", 
                  sum(n_of_1bins), "bin(s) of width 1. The corresponding label(s) are:", 
                  paste(breaks[c(F, F, n_of_1bins)], collapse = ", "))) # + 2 to get right index because of (1) diff and (2) first diff dropped
  }

  # break / x interaction
  if(length(x) == 1 & length(breaks) == 1) stop("if x is a scalar, breaks must be given in intervals")
  
  if(length(breaks) == 1) {
    if(2 * breaks > max(x) - min(x) + 1) stop("range too small for the number of breaks specified")
    if(length(x) <= breaks) warning("breaks is a scalar not smaller than the length of x")
  }

  ############################################### assertive checks completed  ###############################################
  
  # if breaks are not specified (i.e. only the number of breaks is provided)
  if(length(breaks) == 1){
    
    numLabels <- breaks
    
    # should the breaks be "pretty"? (‘round’ values which cover the range of the values in x)
    # or based on quantiles?
    # or evenly spaced over the range of the data? ("default")
    if(breaks_mode == "pretty"){
      
      breakpoints <- pretty(x, breaks)
      
    } else if(breaks_mode == "default"){
      
      range <- max(x)-min(x)+1
      avg_bin_width <- floor(range/breaks)
      rem <- range %% breaks
      num <- breaks+1
      if(rem == 0){
        breakpoints <- seq(from=min(x)-1, by = avg_bin_width, length.out = num)
        breakpoints[1] <- min(x)
      } else if(rem != 0) {
        if(right == FALSE){
          breakpoints <- rev(seq(from=max(x), by = -avg_bin_width, length.out = num))
          breakpoints[1] <- min(x)
          for(i in 1:rem){
            breakpoints[i+1] <- min(x)-1+avg_bin_width*i+i
          }
        } else if(right == TRUE){
          breakpoints <- seq(from=min(x)-1, by = avg_bin_width, length.out = num)
          breakpoints[1] <- min(x)
          breakpoints[num] <- max(x)
          for(i in num:(num-rem+1)){
            breakpoints[i-1] <- max(x)-(avg_bin_width+1)*(num-i+1)
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
    recode_labels <- paste(head(breakpoints, -1) + floorInc, tail(breakpoints, -1) - ceilingDec, sep = label_sep) 
    # correct labels with binwidth 1, that is where to elements separated by label_sep are the same, i.e. the label "10-10"
    same <- head(breakpoints, -1) + floorInc == tail(breakpoints, -1) - ceilingDec
    recode_labels[same] <- (tail(breakpoints, -1) - ceilingDec)[same]
    
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
  output <- cut.default(x, breaks = breakpoints, labels = recode_labels, include.lowest = include.lowest,
              right = right, ordered_result = ordered_result, ...)
  
  if(anyNA(output)) {
    warning(paste(sum(is.na(output)), "missing values generated"))
  }
  output
}
