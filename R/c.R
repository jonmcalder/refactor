#' Combine Values into a Factor
#' 
#' @inheritParams base::c
#' @examples
#' char1 <- as.factor(sample(letters, 5))
#' char2 <- as.factor(sample(letters, 5))
#' c(char1, char2)
#' @export
c.factor <- function(..., recursive = FALSE){
  dots = list(...)
  dots_chars <- lapply(dots, FUN = as.character)
  return(cfactor(unlist(dots_chars)))
}

#' Combine Values into an Ordered Factor
#' 
#' @inheritParams base::c
#' @examples
#' group1 <- cfactor(c("US$51 - US$75", "US$101 - US$125", "US$1 - US$25"), 
#'                  ordered = TRUE)
#' group2 <- cfactor(c("US$76 - US$100", "US$26 - US$50", "US$126 - US$150"), 
#'                  ordered = TRUE)
#' c(group1, group2)
#' @export
c.ordered <- function(..., recursive = FALSE){
  dots = list(...)
  dots_chars <- lapply(dots, FUN = as.character)
  return(cfactor(unlist(dots_chars), ordered = TRUE))
}