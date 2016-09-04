#' Convert Numeric to Factor
#'
#' cut divides the range of x into intervals and codes the values in x according to the interval they fall into.
#'
#' @param x A numeric vector which is to be converted to a factor by cutting.
#' @param breaks Either an integer vector of two or more unique cut points or a single integer (greater than or equal to 2) giving the
#' number of intervals into which x is to be cut.
#' @param labels Labels for the levels of the resulting category. By default, labels are constructed using "a-b c-d" interval notation.
#' If labels = FALSE, simple integer codes are returned instead of a factor.
#' @param include.lowest Logical, indicating if an ‘x[i]’ equal to the lowest (or highest, for right = FALSE) ‘breaks’ value should be
#' included.
#' @param right	Logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
#' @param ordered_result Logical: should the result be an ordered factor?
#' @return A factor is returned, unless labels = FALSE which results in an integer vector of level codes.
#' @examples Z <- sample(10)
#' cut(Z, breaks = c(0, 5, 10))
#' @export
cut.integer <- function(x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE, ordered_result = FALSE,
                        breaks_mode = "default", label_sep = "-", balance = "left", ...) {
  
  # check function arguments
  assert_class(x, "integer")
  assert_class(breaks, "numeric")
  assert_class(include.lowest, "logical")
  assert_class(right, "logical")
  assert_class(ordered_result, "logical")
  assert_choice(breaks_mode, c("default", "pretty", "quantile"))
  assert_class(label_sep, "character")
  assert_choice(balance, c("left", "right"))
  
  # NAs in breaks
  if(anyNA(breaks)) {
    breaks <- na.omit(breaks)
    warning("missing values in breaks were removed")
  }
  
  # unsorted breaks
  if(is.unsorted(breaks)){
    breaks <- sort(breaks)
    warning(paste("breaks were unsorted and are now sorted in the following order:", paste0(breaks, collapse = " ")))
  }

  # break / x interaction
  if(length(x) %in% length(breaks) %in% 1) stop("if x is a scalar, breaks must be given in intervals")
  
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
      
    } else if(breaks_mode == "quantile"){
      
      # not yet implemented
      
    } else if(breaks_mode == "default"){
      
      range <- max(x)-min(x)+1
      avg_bin_width <- floor(range/breaks)
      rem <- range %% breaks
      num <- breaks+1
      if(rem == 0){
        breakpoints <- seq(from=min(x)-1, by = avg_bin_width, length.out = num)
        breakpoints[1] <- min(x)
      } else if(rem != 0) {
        if(balance == "left"){
          breakpoints <- rev(seq(from=max(x), by = -avg_bin_width, length.out = num))
          breakpoints[1] <- min(x)
          for(i in 1:rem){
            breakpoints[i+1] <- min(x)-1+avg_bin_width*i+i
          }
        } else if(balance == "right"){
          breakpoints <- seq(from=min(x)-1, by = avg_bin_width, length.out = num)
          breakpoints[1] <- min(x)
          breakpoints[num] <- max(x)
          for(i in num:(num-rem+1)){
            breakpoints[i-1] <- max(x)-(avg_bin_width+1)*(num-i+1)
          }
        }
      }
    }
    
    include.lowest <- TRUE
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
  } else if(length(labels) == 1){
    if(labels == FALSE) {
    recode_labels <- FALSE
    }
  } else if(!is.null(labels)) {
    if(length(labels) == length(breakpoints) - 1) {
      recode_labels <- labels
    } else if(length(labels) != length(breakpoints) -1) {
      stop("if labels not 'NULL' and not 'F', it must be the same length as the number of brackets resulting from 'breaks'")
    }
    
  }
  output <- cut.default(x, breaks = breakpoints, labels = recode_labels, include.lowest = include.lowest,
              right = right, ordered_result = ordered_result, ...)
  
  if(anyNA(output)) {
    warning(paste(sum(is.na(output)), "missing values generated"))
  }
  output
}
