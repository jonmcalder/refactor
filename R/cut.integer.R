#' Create Bins for Numeric vectors
#' 
#' cut divides the range of \code{x} into intervals and codes the values in \code{x} according
#' to the interval they fall into.
#' 
#' @param x A numeric vector which is to be converted to a factor by cutting.
#' @param breaks Either an integer vector of two or more unique cut points or a
#'   single integer (greater than or equal to 2) giving the number of intervals
#'   into which \code{x} is to be cut.
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
#'   two different ways based on the type of breaks argument. In the
#'   conventional case, where a breaks vector is supplied, \code{right = TRUE}
#'   indicates that bins should be closed on the right (and open on the left) or
#'   vice versa. If a single integer breaks value is provided, then 
#'   \code{right = TRUE} indicates that bins will be determined such that those 
#'   on the right are larger (if it is not possible for all bins to be evenly 
#'   sized).
#' @param ordered_result Logical: should the result be an ordered factor?
#' @param breaks_mode A parameter indicating how to determine the intervals 
#'  when breaks is specified as a scalar. 
#'  \itemize{ 
#'    \item 'default' will result in intervals spread as evenly as possible 
#'    over the exact range of \code{x}. 
#'    \item 'pretty' will generate rounded breakpoints for the intervals (often
#'    extending slightly beyond the range of \code{x}) based on 
#'    \link[base]{pretty}.}
#' @param label_sep A single or short character string used to generate labels
#'   for the intervals e.g. the default value of "-" will result in labels like
#'   1-10 11-20 21-30 etc.
#' @param ... Further arguments to be passed to or from other methods, 
#'  in particular to \code{\link{cut.default}}.
#' @details In deviation from \code{cut.default}, \code{cut.integer} does not 
#'  have an argument \code{dig.lab}, but instead has two arguments that do not 
#'  exist for \code{cut.default}: \code{breaks_mode} and \code{label_sep}.
#'  Note that unlike \code{\link[base]{cut.default}}, here 
#'  \code{include.lowest} defaults to \code{TRUE}, since this is more intuitive 
#'  for integer intervals.
#' @return A factor is returned, unless \code{labels = FALSE} which results in 
#' an integer vector of level codes.
#' @examples random <- sample(10)
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
  
  # breaks that create bins of width 1
  n_of_1bins <- diff(breaks)[-1] == 1
  if(sum(n_of_1bins > 0)) {
    warning(paste("this break specification produces", sum(n_of_1bins), 
                  "bin(s) of width 1. The corresponding label(s) are:", 
                  paste(breaks[c(F, F, n_of_1bins)], collapse = ", "))) 
  }

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
  if(length(breaks) == 1){
    
    numLabels <- breaks
    
    # create breakpoints based on breaks_mode
    # should the breaks be "pretty"? (‘round’ values which cover the range of x)
    # or evenly spaced over the range of the data? ("default")
    if(breaks_mode == "pretty"){
      
      breakpoints <- pretty(x, breaks)
      
    } else if(breaks_mode == "default"){
      
      breaks_output <- cut_breakpoints(x, breaks, right, include.lowest)
      breakpoints <- breaks_output$breakpoints
      include.lowest <- breaks_output$include.lowest
      
    }
    
    # Now that the breakpoints have been determined, set right = TRUE since this 
    # is (by convention) the required interpretation of these breakpoints for 
    # the purpose of the labeling which follows
    right <- TRUE
    
  # use breakpoints as is if provided  
  } else if(length(breaks > 1)){
    
    breakpoints <- breaks

    numLabels <- length(breakpoints) - 1
    
  }
  
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
  
############################## labels completed ################################
  
  output <- cut.default(x, breaks = breakpoints, labels = recode_labels, 
                        include.lowest = include.lowest, right = right, 
                        ordered_result = ordered_result, ...)
  
  if(anyNA(output)) {
    warning(paste(sum(is.na(output)), "missing values generated"))
  }
  output
}
