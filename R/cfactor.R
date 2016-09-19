#' Factor
#'
#' A wrapper for \code{factor} with enhanced control.
#' @inheritParams base::factor
#' @param sep A character vector giving all strings that are used to separate the lower numerical boundary of a range from the upper 
#'  numerical boundary within \code{x}. 
#' @return An object of class "factor" which has a set of integer codes the length of \code{x} with a "levels" attribute of mode character
#'  and unique (!anyDuplicated(.)) entries. If argument ordered is true (or ordered() is used) the result has class c("ordered", "factor").
#' @details \code{cfactor} wraps \code{\link{factor}} but provides enhanced control. \cr
#'  The order of the levels is determined by sorting the numerical values preceeding the separators indicated in \code{sep}. If 
#'  \code{sep} is set to \code{NULL} or if not every value of \code{x} contains numbers, the same ordering as in \code{factor} is applied. 
#' @examples cfactor(c("a", "c", "b", "c", "d"))
#' @export
cfactor <- function(x, levels, labels = levels, exclude = NA, ordered = is.ordered(x), nmax = NA, sep = c("-", "to")) {

  `%w/o%` <- function(x, y) x[!x %in% y] # opposite of %in%

  if(missing(levels)){ # detect factor levels if not given
    uniq_x <- unique(na.omit(x))
    has_numbers <- all(grepl("[[:digit:]]", uniq_x))
    if(!is.null(sep) && has_numbers){ # use regular expression algorithm
      sep.ready <- paste0(sep, collapse = "|")
      sep <- regexec(sep.ready, uniq_x)
      start <- vapply(sep, "[", 1, FUN.VALUE = numeric(1)) # extract start of sep
      start <- ifelse(start == -1, nchar(uniq_x) + 1 , start)
      before <- substr(uniq_x, 1, start - 1)
      before <- gsub("[[:space:]]", "", before)
      # remove all non-digit characters and return the order of the numbers
      rmpattern <- paste0("[^[:digit:]\\", options()$OutDec, "]") # get the systems decimal separator
      finalorder <- order(as.numeric(gsub(rmpattern, "", before)))
      
      levels <- uniq_x[finalorder]
    } else { # use factor algorithm
      levels <- sort(unique(as.character(x)))
    }
    
  }
  
  # create the factor
  output <- factor(x, levels = levels, labels = labels, exclude = exclude, ordered = ordered, nmax = nmax) # only x should never be looked up in .GlobalEnv
  prior <- as.character(unique(x))
  posterior <- ifelse(levels == labels, levels(output), levels)
  
  # check if new levels differ from old unique character strings
  if(!setequal(prior, posterior)) {
    # levels that are not current names
    if(!all(posterior %in% prior)) {
      warning(paste("the following levels were empty: \n", paste(c(posterior %w/o% prior), collapse = "\n")),
              call. = F)
    }

    # current names that don't become levels
    if(!all(prior %in% posterior)) {
      warning(paste("the following levels were removed: \n", paste(prior[!(prior %in% posterior)], collapse = "\n")),
              call. = F)
    }
  }
  output
}
