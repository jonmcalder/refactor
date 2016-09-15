#' Factor
#'
#' A wrapper for factor with enhanced control.
#'
#' @param x A vector of data, usually taking a small number of distinct values.
#' @param levels An optional vector of the values (as character strings) that x might have taken. The default is the unique set
#' of values taken by as.character(x), sorted into increasing order of x. Note that this set can be specified as smaller than
#' sort(unique(x)).
#' @param labels Either an optional character vector of labels for the levels (in the same order as levels after removing those in exclude),
#' or a character string of length 1.
#' @param sep A character vector giving all strings that are used to separate the lower boundary of a range from the upper boundary.
#' @return An object of class "factor" which has a set of integer codes the length of x with a "levels" attribute of mode character
#' and unique (!anyDuplicated(.)) entries. If argument ordered is true (or ordered() is used) the result has class c("ordered", "factor").
#' @examples cfactor(c("a", "c", "b", "c", "d"))
#' @export
cfactor <- function(x, levels, labels = levels, exclude = NA, ordered = is.ordered(x), nmax = NA, sep = c("-", "to")) {

  `%w/o%` <- function(x, y) x[!x %in% y] # opposite of %in%

  if(missing(levels)){ # detect factor levels if not given
    uniq_x <- unique(x)
    has_numbers <- any(grepl("[[:digit:]]", uniq_x))
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
  posterior <- levels(output)
  
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
