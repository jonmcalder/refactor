#' Create Bins for Numeric vectors
#' 
#' cut divides the range of \code{x} into intervals and codes the values in \code{x} according
#' to the interval they fall into.
#' 
#' @param x A numeric vector which is to be converted to a factor by cutting.
#' @param breaks Either an integer vector of two or more unique cut points or a
#'   single integer (greater than or equal to 2) giving the number of intervals
#'   into which \code{x} is to be cut. Please note, however, that the resulting 
#'   number of intervals is not guaranteed to be \code{breaks} in the case of 
#'   \code{breaks_mode = 'pretty'}.
#' @param labels Labels for the levels of the resulting category. By default,
#'   labels are constructed using "a-b c-d" interval notation. If 
#'   \code{labels = FALSE}, simple integer codes are returned instead of a 
#'   factor.
#' @param include.lowest Logical, indicating if an "x[i]" equal to the lowest
#'   (or highest, for \code{right = FALSE)} "breaks" value should be included. 
#'   Note that unlike \code{\link[base]{cut.default}}, here 
#'   \code{include.lowest} defaults to \code{TRUE}, since this is more 
#'   intuitive for integer intervals.
#' @param right	Logical, indicating how to create the bins. This is utilized in 
#'   different ways based on the type of breaks argument. In the conventional 
#'   case, where a breaks vector is supplied, \code{right = TRUE}
#'   indicates that bins should be closed on the right (and open on the left) or
#'   vice versa (i.e. as in \code{\link[base]{cut.default}}). If a 
#'   single integer breaks value is provided, then the appropriate breakpoints 
#'   are determined based on the value of \code{breaks_mode}, and the value for 
#'   \code{right} is not utilized if \code{breaks_mode = 'pretty'} - see below 
#'   for details.
#' @param ordered_result Logical: should the result be an ordered factor?
#' @param breaks_mode A parameter indicating how to determine the intervals 
#'  when breaks is specified as a scalar (note that this argument has no effect 
#'  if breaks is specified as a vector). Can be 'default', 'spread' or 
#'  'pretty'. See 'Details' below.
#' @param label_sep A single or short character string used to generate labels
#'   for the intervals e.g. the default value of "-" will result in labels like
#'   1-10 11-20 21-30 etc.
#' @param ... Further arguments to be passed to or from other methods, 
#'  in particular to \code{\link{cut.default}}.
#' @details In deviation from \code{cut.default}, \code{cut.integer} does not 
#'  have an argument \code{dig.lab}, but instead has two arguments that do not 
#'  exist for \code{cut.default}: \code{breaks_mode} and \code{label_sep}. \cr
#'  Note that unlike \code{\link[base]{cut.default}}, here 
#'  \code{include.lowest} defaults to \code{TRUE}, since this is more intuitive 
#'  for the class \code{integer}. \cr
#'  If \code{breaks} is supplied as a scalar, the value of \code{breaks_mode} determines 
#'  how the breaks are constructed:
#'  \itemize{ 
#'    \item 'default' will produce intervals which are the (integer) equivalent 
#'    to those produced by \code{cut.default} i.e. the bins/groupings will be the 
#'    same - but the labels will be of the form int-int/2-4 instead of 
#'    (numeric, numeric]/(1.5,4.2].
#'    \item 'spread' will result in intervals spread as evenly as possible 
#'    over the exact range of \code{x}. If the intervals cannot all be equal, 
#'    then \code{right} determines whether the rightmost (\code{TRUE}) or 
#'    leftmost (\code{FALSE}) intervals are made slightly wider.
#'    \item 'pretty' will generate rounded breakpoints for the intervals based 
#'    on \code{\link[base]{pretty}}. Note that breaks here is treated as the 
#'    'desired' number of intervals and is not guaranteed. Note also that the 
#'    range of \code{x} can be exceeded slightly by the intervals in some 
#'    cases.}

#' @return A factor is returned, unless \code{labels = FALSE} which results in 
#' an integer vector of level codes.
#' @examples 
#'  random <- sample(10)
#'  cut(random, breaks = seq(0, 100, by = 10))[1:10]
#' @export

cut.integer <- function(x, breaks, labels = NULL, include.lowest = TRUE, 
                        right = TRUE, ordered_result = FALSE,
                        breaks_mode = "default", label_sep = "-", ...) {
  
############################## assertive checks ################################
  
  # check function arguments
  assert_class(x, "integer")
  
  assert(
    test_class(breaks, "numeric"),
    test_class(breaks, "integer")
  )
  
  if(!is.null(labels) && labels != FALSE){
    if(length(labels) != breaks && length(labels) != length(breaks)-1){
    
      stop(paste("if labels not 'NULL' and not 'FALSE', it must be the same", 
                 "length as the number of bins resulting from 'breaks'"))  
    }  
  }

  assert_class(include.lowest, "logical")
  assert_class(right, "logical")
  assert_class(ordered_result, "logical")
  assert_choice(breaks_mode, c("default", "spread", "pretty"))
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
    warning(paste("When coerced to integers, the following breaks were", 
                  "truncated (rounded down): \n ", 
            paste(paste(breaks[differ], "to", new_breaks[differ]), " \n ", 
                  collapse = " ")))
          
    breaks <- new_breaks
  }
  
  # unsorted breaks
  if(is.unsorted(breaks)){
    breaks <- sort(breaks)
    warning(paste(
      "breaks were unsorted and are now sorted in the following order:", 
      paste0(breaks, collapse = " ")))
  }
  
  # breaks that create bins of width 1 (warning only if breaks is supplied as
  # scalar). Since breaks need to be created first, this warning is not issued
  # in this section
  
  # break / x interaction
  if(length(x) == 1 & length(breaks) == 1) {
    stop("if x is a scalar, breaks must be given in intervals")
  }
  
  if(length(breaks) == 1) {
    if(breaks > max(x) - min(x) + 1) {
      stop("range too small for the number of breaks specified")
    }
    if(length(x) <= breaks) {
      warning("breaks is a scalar not smaller than the length of x")
    }
  }
  
############################## assertive checks completed ######################

############################## determine breakpoints ###########################
    
  # if breaks are not specified (i.e. only the number of breaks is provided)
  # create breakpoints based on breaks_mode
  if(length(breaks) == 1){
    
    # should breakpoints be the (integer) equivalent of cut.default?
    if(breaks_mode == "default"){
      
      # adapted from base::cut.default
      nb <- as.integer(breaks + 1) # one more than #{intervals}
      dx <- diff(rx <- range(x, na.rm = TRUE))
      
      if(right == TRUE) {
        breakpoints <- floor(seq.int(rx[1L], rx[2L], length.out = nb))  
      } else {
        breakpoints <- ceiling(seq.int(rx[1L], rx[2L], length.out = nb))
      }
        
      include.lowest <- TRUE
      
    # or "spread" over the range of the data?
    } else if(breaks_mode == "spread"){
      
      breaks_output <- cut_breakpoints(x, breaks, right, include.lowest)
      breakpoints <- breaks_output$breakpoints
      include.lowest <- breaks_output$include.lowest
    
    # or "pretty"? (‘round’ values which cover the range of x)
    } else if (breaks_mode == "pretty"){
      
      breakpoints <- pretty(x, breaks)
      
    }
    
    # Now that the breakpoints have been determined, set right = TRUE since this 
    # is (by convention) the required interpretation of these breakpoints for 
    # the purpose of the labeling which follows
    # However the default method needs to honor the right argument in order to 
    # keep consistency with cut.default
    if(breaks_mode != "default"){
    
        right <- TRUE
        
    }
    
  # use breakpoints as is if provided  
  } else if(length(breaks > 1)){
    
    breakpoints <- breaks
    
  }

  numLabels <- length(breakpoints) - 1
  
############################## breakpoints completed ###########################

############################## determine labels ################################
  
  adjust <- bin_adjust(right, include.lowest, numLabels)
  
  # create integer-based interval labels using label_sep
  if(is.null(labels)) {
    recode_labels <- paste(head(breakpoints, -1) + adjust$floorInc, 
                           tail(breakpoints, -1) - adjust$ceilingDec, 
                           sep = label_sep) 
    # correct labels with binwidth 1, that is where to elements separated by 
    # label_sep are the same, i.e. the label "10-10"
    same <- head(breakpoints, -1) + adjust$floorInc == tail(breakpoints, -1) - adjust$ceilingDec
    recode_labels[same] <- (tail(breakpoints, -1) - adjust$ceilingDec)[same]
    
    # warning: breaks that create bins of width 1 (warning only if breaks is 
    # supplied a scalar)
    if(length(breaks) == 1) {
      if(sum(same) > 0) {
        warning(paste("this break specification produces", sum(same), 
                      "bin(s) of width 1. The corresponding label(s) are:", 
                      paste(recode_labels[same], collapse = ", "))) 
      }
    }

    
  } else {
    
    recode_labels <- labels
    
  }
  
############################## labels completed ################################
  
  output <- cut.default(x, breaks = breakpoints, labels = recode_labels, 
                        include.lowest = include.lowest, right = right, 
                        ordered_result = ordered_result, ...)
  
  if(anyNA(output)) {
    warning(paste(sum(is.na(output)), "missing values generated"))
  }
  output
}
