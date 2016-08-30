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
#' @return An object of class "factor" which has a set of integer codes the length of x with a "levels" attribute of mode character
#' and unique (!anyDuplicated(.)) entries. If argument ordered is true (or ordered() is used) the result has class c("ordered", "factor").
#' @examples cfactor(c("a", "c", "b", "c", "d"))
#' @export
cfactor <- function(x, levels = NULL, labels = levels, connector = c("to", "bis", "-"),
                    cleaning = list(punct = c(from = ",'.", to = "."),
                                    connector = list(from = c("to", "-", "bis"), to = " - "),
                                    case = "upper",
                                    numbersAsStrings = F),
                    ...) {

  `%w/o%` <- function(x, y) x[!x %in% y]

  # detect factor levels
  if(is.null(levels)){
    connector.ready <- paste0(connector, collapse = "|")
    sep <- regexec(connector.ready, x)
    start <- sapply(sep, "[[", 1) # extract start
    before <- substr(x, 1, start - 1)
    before <- gsub("[[:space:]]", "", before)
    finalorder <- order(as.numeric(gsub("[^[:digit:]]", "", before)))

    finallevels <- unique(x)[finalorder]
  } else {
    finallevels <- levels
  }

  #  clean levels
  if(!is.null(cleaning[["punct"]])) {
    regex <- paste("[", cleaning[["punct"]][["from"]], "]", sep = "")
  }


  # check for empty / removed
  prior <- as.character(unique(x))
  output<-factor(x, levels = finallevels, ...)
  if(!setequal(prior, levels(output))) {
    # levels that are not current names
    if(!all(levels(output) %in% prior)) {
      warning(paste("the following label(s) is / are empty: \n", paste(c(levels(output) %w/o% prior), collapse = "\n")),
              call. = F)
    }

    # current names that don't become levels
    if(!all(prior %in% levels(output))) {
      warning(paste("the following label(s) is / are removed: \n", paste(prior[!(prior %in% levels(output))], collapse = "\n")),
              call. = F)
    }
  }
  output
}
