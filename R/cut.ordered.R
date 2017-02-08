#' Create Bins for ordered Factors
#'
#' cut divides the range of \code{x} into intervals and codes the values in 
#'  \code{x} according to the interval they fall into.
#'
#' @param x An ordered factor which is to be cut into ordered bins.
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
#' @param right	Logical, indicating how to create the bins. This is utilized in
#'   two different ways based on the type of breaks argument. In the
#'   conventional case, where a breaks vector is supplied, \code{right = TRUE}
#'   indicates that bins should be closed on the right (and open on the left) or
#'   vice versa. If a single integer breaks value is provided, then 
#'   \code{right = TRUE} indicates that bins will be determined such that those 
#'   on the right are larger (if it is not possible for all bins to be evenly 
#'   sized).
#' @param ordered_result Logical: should the result be an ordered factor? Note 
#'  that since the input data is ordered this argument is \code{TRUE} by 
#'  default.
#' @param label_sep A single or short character string used to generate labels 
#'  for the intervals e.g. the default value of "-" will result in labels like 
#'  a-c d-g i-z etc.
#' @param ... Further arguments to be passed to or from other methods, 
#'  in particular to \code{\link{cut.default}}.
#' @details Note that the \code{dig.lab} argument from \code{cut.default}, is 
#'  replaced for \code{cut.ordered} by a new argument: \code{label_sep}. Also 
#'  note that unlike \code{\link[base]{cut.default}}, here \code{include.lowest} 
#'  defaults to \code{TRUE}, since this is more intuitive for the class 
#'  \code{ordered}. Finally, since the input for \code{cut.ordered} is of class 
#'  \code{ordered}, this is the default output as well. This contrasts with 
#'  \code{cut.default}, which produces an unordered output by default.
#' @return A factor is returned, unless \code{labels = FALSE} which results in 
#'  an integer vector of level codes.
#' @examples 
#'  some_letters <- cfactor(letters, ordered = TRUE)
#'  # bin letters into two groups
#'  cut(some_letters, breaks = c("a", "q", "z"), 
#'      labels = c("beginning of the alphabet", "the rest of the alphabet"), 
#'      right = TRUE, include.lowest = TRUE)
#'  # alter separator
#'  cut(some_letters, breaks = 20, label_sep = " to ")
#'  \dontrun{
#'  # warnings
#'  ## missing values created
#'  cut(some_letters, breaks = c("a", "g"), label_sep = " to ")
#'  ## bins of width 1
#'  cut(some_letters, breaks = 20)
#'      }

#' @importFrom utils head tail
#' @importFrom stats quantile
#' @export
cut.ordered <- function(x, breaks, labels = NULL, include.lowest = TRUE,
                        right = TRUE, ordered_result = TRUE, label_sep = "-", 
                        ...) {


#   ____________________________________________________________________________
#   assertive tests                                                         ####

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
  
  if(!is.null(labels) && labels != FALSE){
    if(length(labels) != breaks && length(labels) != length(breaks)-1){
      
      stop(paste("if labels not 'NULL' and not 'FALSE', it must be the same", 
                 "length as the number of bins resulting from 'breaks'"))  
    }  
  }
  
  # include.lowest
  test_logical(include.lowest)
  
  # right
  test_logical(right)
  
  # ordered_result
  test_logical(ordered_result)
  


#   ____________________________________________________________________________
#   determine breakpoints                                                   ####

  x_num <- as.numeric(x)
  x_lev <- levels(x)
  unique_x <- unique(x)
  
  ### 
  # if breaks are not specified (i.e. only the number of breaks is provided)
  if(length(breaks) == 1){
    
    numLabels <- breaks
    breaks_output <- cut_breakpoints(x_num, breaks, right, include.lowest)
    breakpoints <- breaks_output$breakpoints
    include.lowest <- breaks_output$include.lowest
    
  # use breakpoints as is if provided  
  } else if(length(breaks > 1)){
    breakpoints <- match(breaks, x_lev)
    if(anyNA(breakpoints)){
      stop(paste("specified breakpoints inexistent in data: \n", 
                 paste(breaks[is.na(breakpoints)], collapse = "\n")))
    }

    numLabels <- length(breakpoints) - 1
    
  }
  


#   ____________________________________________________________________________
#   determine labels                                                        ####

  adjust <- bin_adjust(right, include.lowest, numLabels)
  
  # create integer-based interval labels using label_sep
  if(is.null(labels)) {
    recode_labels <- paste(x_lev[head(breakpoints, -1) + adjust$floorInc], 
                           x_lev[tail(breakpoints, -1) - adjust$ceilingDec], 
                           sep = label_sep)
    
    # correct labels with binwidth 1, that is where to elements separated by 
    # label_sep are the same, i.e. the label "10-10"
    same <- head(breakpoints, -1) + adjust$floorInc == tail(breakpoints, -1) - adjust$ceilingDec
    recode_labels[same] <- x_lev[tail(breakpoints, -1) - adjust$ceilingDec][same] 
    
    # warning: breaks that create bins of width 1 (warning only if breaks is 
    # supplied a scalar)
    if(length(breaks) == 1) {
      if(sum(same) > 0) {
        warning(paste("this break specification produces", sum(same), 
                      "bin(s) of width 1. The corresponding label(s) are:", 
                      paste(recode_labels[same], collapse = ", "))) 
      }
    }
    
  } else if(!is.null(labels)) {
      
    recode_labels <- labels
    
  }
  

#   ____________________________________________________________________________

  output <- cut.default(x_num, breaks = breakpoints, labels = recode_labels, 
                        include.lowest = include.lowest, right = right, 
                        ordered_result = ordered_result, ...)
  
  
  if(anyNA(output)) {
    warning(paste(sum(is.na(output)), "missing values generated"))
  }
  output
}
