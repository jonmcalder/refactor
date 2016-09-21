#' index_cfactor
#'
#' Decode numerical data into (ordered) factors given the encoding
#'
#' @param data A data frame containing integers to decode.
#' @param index A data frame containing the encoding for \code{data}.
#' @param variable The name of the column in \code{index} that indicates the variable.
#' @param encoding The name of the column in \code{index} that indicates the encoding.
#' @param label The name of the column in \code{index} that indicates the label to be assigned.
#' @param ... Further arguments to pass to \code{\link{cfactor}}. 
#' @examples 
#' data <- data.frame(var1 = sample(x = 1:10, size = 20, replace = TRUE),
#'                    var2 = rep(1:2, 20),
#'                    var3 = sample(20),
#'                    var4 = 2, 
#'                    var5 = sample(row.names(USArrests), size = 20),
#'                    stringsAsFactors = FALSE)
#' 
#' index <- data.frame(var = rep(paste0("var", 1:3), c(10, 2, 20)),
#'                    encoding = c(1:10, 1:2, 1:20),
#'                    label = c(letters[1:10], c("male", "female"), LETTERS[1:20]))
#'                    
#' index_cfactor(data = data, index = index, variable = "var", ordered = c(TRUE, TRUE, FALSE))
#' @export

index_cfactor <- function(data, index, variable = "variable", encoding = "encoding", label = "label", ...){
  
  further_args <- list(...)
  pos_enc <- match(encoding, names(index))
  pos_lab <- match(label, names(index))
  
  # only apply index to variables in index
  pos_fact <- which(names(data) %in% unique(as.character(index[[variable]])))
  
  sp_index <- split(index, index[[variable]]) # create three separate indices
  sp_index_enc <- lapply(sp_index, "[[", pos_enc) # extract the encoding columns
  sp_index_lab <- lapply(sp_index, "[[", pos_lab) # extract the label columns
  
  lapply(seq_along(further_args), function(i) if(!is.null(further_args[[i]]) && length(sp_index_enc) %% length(further_args[[i]])){
    stop(paste0("argument '", names(further_args)[[i]], "' is not recycled fully. Number of columns to be decoded should match the length of '", names(further_args)[[i]],"' or mulitiple thereof."))
  })
    
  data[, pos_fact] <- as.data.frame(Map(cfactor, data[, pos_fact], sp_index_enc, sp_index_lab, ...)) # apply cfactor to the original data, each column with its 
  data
}