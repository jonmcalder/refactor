#' Factor
#'
#' A wrapper for \code{factor} with enhanced control.
#' @inheritParams base::factor
#' @param levels an optional vector of the values (as character strings) that 
#'  \code{x} might have taken. If missing (the default), the levels are 
#'  determined by sorting the numerical elements in \code{x}. This is not as in
#'  \code{factor}. See 'Details'.
#' @param sep A character vector giving all strings that are used to separate 
#'  the lower numerical boundary of a range from the upper numerical boundary 
#'  within \code{x}. See 'Details' for more information.
#' @return See \code{\link{factor}}.
#' @details \code{cfactor} wraps \code{\link{factor}} but provides enhanced 
#'  control. The enhanced control has the following elements:
#'  \itemize{
#'    \item more warnings: \code{NA}s are never produced silently. Empty levels
#'    also produce a warning. If \code{levels} and \code{labels} intersect, a
#'    warning is produced too.
#'    \item flexible order detection: By default, the order of the levels is 
#'    given by the order of the numerical elements in \code{x}. See below.
#'  }
#'  Levels of the factor or determined by the \code{levels} argument. If 
#'  \code{levels} is missing, the order of the levels is determined as 
#'  follows: 
#'  \itemize{
#'    \item If \code{sep} is set to \code{NULL}: Levels are determined by 
#'    sorting the unique values in \code{x}, which is the same behaviour as in 
#'    \code{factor}.
#'    \item If \code{sep} is set to \code{NA}, the order is given by first
#'    appearance in \code{x}. 
#'    \item Otherwise, 
#'      \itemize{
#'        \item If every value in \code{x} contains numbers: Levels are determined
#'        by sorting the numerical values preceeding the separators indicated in 
#'        \code{sep}.
#'        \item If not every value in \code{x} contains numbers: Levels are 
#'        determined by sorting the unique values in \code{x}, which is the same 
#'        behaviour as in \code{factor} and corresponds to setting \code{sep}
#'        to \code{NULL}.
#'      }
#'    }
#'  Apart from the newly introduced argument \code{sep}, \code{cfactor} has 
#'  the same arguments and defaults as \code{factor}.
#'  \cr
#'  Where levels is not supplied, \code{\link{unique}} is called. Since factors typically 
#'  have quite a small number of levels, for large vectors x it is helpful to 
#'  supply \code{nmax} as an upper bound on the number of unique values.
#' @examples \dontrun{
#' ## create warnings with unused and removed levels
#' string <- c("a", "b", "c")
#' cfactor(string, levels = c("b", "c", "d"))}
#' 
#' ## detecting levels: compare factor and cfactor
#' hard_to_detect <- c("EUR 21 - EUR 22", "EUR 100 - 101", 
#' "EUR 1 - EUR 10", "EUR 11 - EUR 20")
#' factor(hard_to_detect, ordered = TRUE)
#' cfactor(hard_to_detect, ordered = TRUE)
#' @importFrom stats na.omit 
#' @export
cfactor <- function(x, levels, labels = levels, exclude = NA,
                    ordered = is.ordered(x), nmax = NA, sep = c("-", "to")) {

  ############################ assertive tests #################################
  ## simple checks
  
  # x
  assert(
    # use check within assert
    check_character(x),
    check_factor(x),
    check_numeric(x)
  )
  
  # levels 
  if(!missing(levels)) {
    assert(
      check_character(levels),
      check_integer(levels),
      check_numeric(levels))
      
  }
  # labels
  if(!missing(labels)){
    assert(
      check_character(labels),
      check_numeric(labels),
      check_factor(labels)
      )
  }
  
  # exclude
  assert(
    check_scalar_na(exclude, null.ok = TRUE),
    check_class(exclude, class(x))
  )
  
  # ordered
  assert_class(ordered, "logical")
  
  # nmax
  check_int(nmax, na.ok = TRUE)
  
  ## duplicate levels
  if(!missing(levels) && any(duplicated(levels))) {
    
    warning(paste("the following duplicated levels were removed: \n", 
                  paste(unique(levels[duplicated(levels)]), collapse = " \n")))
    
    levels <- unique(levels)
  }
  
  
  
  ######################### assertive tests completed ##########################
  `%w/o%` <- function(x, y) x[!x %in% y] # opposite of %in%
  uniq_x <- unique(na.omit(x), nmax = nmax)
  
  if(missing(levels)){ # detect factor levels if not given
    has_numbers <- all(grepl("[[:digit:]]", uniq_x))
    if(!is.null(sep) && has_numbers){ # use regular expression algorithm
      sep.ready <- paste0(sep, collapse = "|")
      sep <- regexec(sep.ready, uniq_x)
      start <- vapply(sep, "[", 1, FUN.VALUE = numeric(1)) # extract start of sep
      start <- ifelse(start == -1, nchar(uniq_x) + 1 , start)
      before <- substr(uniq_x, 1, start - 1)
      # remove all non-digit characters and return the order of the numbers
      before <- gsub("[[:space:]]", "", before)
      # get the systems decimal separator
      rmpattern <- paste0("[^[:digit:]\\", options()$OutDec, "]") 
      finalorder <- order(as.numeric(gsub(rmpattern, "", before)))
      
      levels <- uniq_x[finalorder]
    } else if(!is.null(sep) && is.na(sep)) { # order by appearance
      levels <- uniq_x
    } else { # use factor algorithm if sep == NULL or no numbers
      levels <- sort(unique(as.character(x)))
    }
    
  }
  
  # create the factor
  ## only x should never be looked up in .GlobalEnv
  output <- factor(x, levels = levels, labels = labels, exclude = exclude, 
                   ordered = ordered, nmax = nmax) 
  prior <- as.character(unique(x))
  posterior <- ifelse(levels == labels, levels(output), levels)
  
  
  # check whether any value in x occurs now in labels that and it not the same
  # value
  
  if(any(levels %in% labels) && !all(levels %in% labels)){
    # find duplicates
    same_represent <- levels == labels
    duplicates <- levels[same_represent] 
    
    no_duplicate_level <- levels[!same_represent]
    no_duplicate_label <- labels[!same_represent]
    if(any(no_duplicate_level %in% no_duplicate_label) || any(same_represent)){
      warning(paste("Some values now used for the labels existed in the data", 
                    "vector 'x' already: \n ", 
                    if(any(no_duplicate_level %in% no_duplicate_label)) {
                      paste("'", no_duplicate_level[no_duplicate_level %in% no_duplicate_label], "' is now represented with '", 
                            no_duplicate_label[no_duplicate_level %in% no_duplicate_label], "', ",
                            "'", no_duplicate_level[no_duplicate_level %in% no_duplicate_label], "' now represents '",
                            no_duplicate_level[no_duplicate_label %in% no_duplicate_level], "' \n ",
                            sep = "", collapse = " \n")
                    }, 
                    if(any(same_represent)) {
                      paste("'", duplicates, "' still represents '", duplicates, "'",
                            sep = "", collapse = " \n")
                    },
                    sep = " "))
    }
    
  }
  
  # check if new levels differ from old unique character strings
  if(!setequal(prior, posterior)) {
    # levels that are not current names
    if(!all(posterior %in% prior)) {
      warning(paste("the following levels were empty: \n", 
                    paste(c(posterior %w/o% prior), collapse = "\n")), 
              call. = FALSE)
    }

    # current names that don't become levels
    if(!all(prior %in% posterior)) {
      warning(paste("the following levels were removed: \n", 
                    paste(prior[!(prior %in% posterior)], collapse = "\n")), 
              call. = FALSE)
    }
  }
  output
}
