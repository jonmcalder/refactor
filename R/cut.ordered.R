#' Create Bins for ordered Factors
#'
#' cut divides the range of \code{x} into intervals and codes the values in 
#'  \code{x} according to the interval they fall into.
#'
#' @param x A numeric vector which is to be converted to a factor by cutting.
#' @param breaks Either a numeric vector of two or more unique cut points or a 
#'  single number (greater than or equal to 2) giving the
#' number of intervals into which \code{x} is to be cut.
#' @param labels Labels for the levels of the resulting category. By default, 
#'  labels are constructed using \code{a-b, c-d} interval notation.
#'  If \code{labels = FALSE}, simple integer codes are returned instead of a 
#'  factor.
#' @param include.lowest Logical, indicating if an \code{x[i]} equal to the 
#'  lowest (or highest, for \code{right = FALSE}) breaks value should be 
#'  included.
#' @param right	Logical, indicating if the intervals should be closed on the 
#'  right (and open on the left) or vice versa.
#' @param ordered_result Logical: should the result be an ordered factor?
#' @param label_sep A single or short character string used to generate labels 
#'  for the intervals e.g. the default value of "-" will result in labels like 
#'  a-c d-g i-z etc.
#' @param ... Further arguments to be passed to or from other methods, 
#'  in particular to \code{\link{cut.default}}.
#' @details In deviation from \code{cut.default}, \code{cut.ordered} does not 
#'  have an argument \code{dig.lab}, but instead has an argument that does not 
#'  exist for \code{cut.default}: \code{label_sep}.
#'  Note that unlike \code{\link[base]{cut.default}}, here 
#'  \code{include.lowest} defaults to \code{TRUE}, since this is more intuitive 
#'  for integer intervals.
#' @return A factor is returned, unless \code{labels = FALSE} which results in 
#'  an integer vector of level codes.
#' @examples 
#'  some_letters <- cfactor(sample(letters, 100, replace = TRUE), ordered = TRUE)
#'  head(cut(some_letters, breaks = c("a", "q", "z"), 
#'           labels = c("beginning of the alphabet", "the rest of the alphabeth"), 
#'           right = TRUE, include.lowest = TRUE))
#' @importFrom utils head tail
#' @importFrom stats quantile
#' @export
cut.ordered <- function(x, breaks, labels = NULL, include.lowest = FALSE,
                        right = TRUE, ordered_result = FALSE, label_sep = "-", 
                        ...) {

  # simple input checking 
  assert_factor(x, ordered = T)
  
  # breaks are either numeric or integer
  assert(
    test_class(breaks, "numeric"),
    test_class(breaks, "integer"),
    test_class(breaks, "character")
  )
  
  # labels
  assert(
    test_null(labels),
    test_logical(labels),
    test_character(labels)
  )
  
  # include.lowest
  test_logical(include.lowest)
  
  # right
  test_logical(right)
  
  # ordered_result
  test_logical(ordered_result)
  
  ######################### assertive checks completed  ########################
  
  x_num <- as.numeric(x)
  x_lev <- levels(x)
  unique_x <- unique(x)
  
  ######## 
  # if breaks are not specified (i.e. only the number of breaks is provided)
  if(length(breaks) == 1){
    
    numLabels <- breaks
    breakpos <- quantile(x_num, seq(0, 1, 1/breaks))
    
    # (‘floor’ values which cover the range of the values in x_num)
    # or evenly spaced over the range of the data? ("default")
    
      
    range <- max(x_num)-min(x_num)+1
    avg_bin_width <- floor(range/breaks)
    rem <- range %% breaks
    num <- breaks+1
    if(rem == 0){
      breakpoints <- seq(from=min(x_num)-1, by = avg_bin_width, length.out = num)
      breakpoints[1] <- min(x_num)
    } else if(rem != 0) {
      if(right == FALSE){
        breakpoints <- rev(seq(from=max(x_num), by = - avg_bin_width, length.out = num))
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
    
    right <- TRUE
    
    
    # use breakpoints as is if provided  
  } else if(length(breaks > 1)){
    breakpos <- match(breaks, x_lev)
    if(anyNA(breakpos)){
      stop(paste("specified breakpoints inexistent in data: \n", 
                 paste(breaks[is.na(breakpos)], collapse = "\n")))
    }
    

    breakpos <- match(breaks, x_lev)
    # check for breakpoint existence
    if(anyNA(breakpos)){
      stop(paste("specified breakpoints inexistent in data: \n", 
                 paste(breaks[is.na(breakpos)], collapse = "\n")))
    }
    
    breakpoints <- breakpos

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
    if(length(breaks) < length(unique_x)) { # the standard case
      recode_labels <- paste(x_lev[head(breakpoints, -1) + floorInc], 
                             x_lev[tail(breakpoints, -1) - ceilingDec], 
                             sep = label_sep)
      
      # correct labels with binwidth 1, that is where to elements separated by 
      # label_sep are the same, i.e. the label "10-10"
      same <- head(breakpoints, -1) + floorInc == tail(breakpoints, -1) - ceilingDec
      recode_labels[same] <- x_lev[tail(breakpoints, -1) - ceilingDec][same] 
      
      
    } else if(length(breaks) == length(unique_x)){ # if we have the same number 
      # of breaks as unique x, each level is a break value itself
      return(cfactor(x, levels = breaks))
      
    }
    
    
  } else if(!is.null(labels)) {
    if(length(labels) == length(breakpoints) - 1) {
      recode_labels <- labels
    } else if(length(labels) != length(breakpoints) - 1) {
      if(length(labels) == 1) {
        if(labels == FALSE) {
          recode_labels <- labels
        } else if(labels != FALSE) {
          stop(paste("if labels not 'NULL' and not 'FALSE', it must be the same", 
                     "length as the number of bins resulting from 'breaks'"))
        }
      } else if(length(labels) != 1) {
        stop(paste("if labels not 'NULL' and not 'FALSE', it must be the same", 
                   "length as the number of bins resulting from 'breaks'"))
      }
      
    }
  }
  output <- cut.default(x_num, breaks = breakpoints, labels = recode_labels, 
                        include.lowest = include.lowest, right = right, 
                        ordered_result = ordered_result, ...)
  
  
  if(anyNA(output)) {
    warning(paste(sum(is.na(output)), "missing values generated"))
  }
  output
}
