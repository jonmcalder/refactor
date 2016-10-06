#' Decode numerical into categorical data
#'
#' Decode numerical columns in a data frame into (ordered) factors given the 
#'  encoding in another data frame.
#'
#' @param data A data frame containing at least one integer column to decode.
#' @param index A data frame containing the names of the variable to encode, 
#'  the encoding for \code{data} and labels to assign.
#' @param variable The name of the column in \code{index} that indicates the 
#'  variable.
#' @param encoding The name of the column in \code{index} that indicates the 
#'  encoding.
#' @param label The name of the column in \code{index} that indicates the label 
#'  that will be given.
#' @param ... Further arguments to be passed to or from other methods, 
#'  in particular to \code{\link{cfactor}}.
#' @details Arguments passed via \code{...} to \code{cfactor} are only recycled 
#'  if of length 1. Otherwise, an error is thrown. 
#'  All arguments passed via \code{...} are applied in the order of the data 
#'  columns but columns not to convert are skipped (see example).
#' @return The original data frame is returned whereas the variables for which 
#'  an encoding was provided are turned into (ordered) factors. All other 
#'  columns are returned unmodified.
#' @examples data <- data.frame(var1 = sample(x = 1:10, size = 20, replace = TRUE),
#'                    var2 = rep(1:2, 20),
#'                    var3 = sample(20),
#'                    var4 = 2, 
#'                    var5 = sample(row.names(USArrests), size = 20),
#'                    stringsAsFactors = FALSE)
#' 
#'  index <- data.frame(var = rep(paste0("var", 1:3), c(10, 2, 20)),
#'                    encoding = c(1:10, 1:2, 1:20),
#'                    label = c(letters[1:10], c("male", "female"), LETTERS[1:20]))
#'                    
#'  index_cfactor(data = data, index = index, variable = "var", ordered = c(TRUE, TRUE, FALSE))
#' @export

index_cfactor <- function(data, index, variable = "variable", 
                          encoding = "encoding", label = "label", ...){
  
  
  further_args <- list(...)
  # make sure variable, encoding and label are actual columns in the data frame
  pos_var <- match(variable, names(index))
  pos_enc <- match(encoding, names(index))
  pos_lab <- match(label, names(index))
  check <- list(variable = pos_var, 
                encoding = pos_enc, 
                label = pos_lab)
  lapply(names(check), function(g) 
    if(is.na(check[[g]])) {
      stop(paste("argument '", g, "' specified incorrectly. ",
                 "There is no such column in '", quote(index), "'", sep = ""))
    }) 
      

  
  # only apply index to variables in index
  var_in_index <- unique(as.character(index[[variable]]))
  # find variable from the data that exist in the index
  pos_fact <- which(names(data) %in% var_in_index) 
  
  # find the variables from the index that exist in the data. Assume all
  used_from_index <- var_in_index %in% names(data) 
  if(!all(used_from_index)) { # if some do not match
    warning(paste("The following variables from 'index' are not found in 'data': \n", 
               paste(var_in_index[!used_from_index], sep = "", collapse = " \n ")))
  }
  index <- subset(index, get(variable) %in% var_in_index[used_from_index])
  sp_index <- split(index, index[[variable]], drop = T) # create separate indices
  sp_index_enc <- lapply(sp_index, "[[", pos_enc) # extract the encoding columns
  sp_index_lab <- lapply(sp_index, "[[", pos_lab) # extract the label columns
  
  # check whether argument can be recycled fully, otherwise stop
  lapply(seq_along(further_args), 
         function(i) 
           if(!is.null(further_args[[i]]) && 
              !(length(further_args[[i]]) %in% c(1, length(sp_index_enc)))){
             stop(paste0("argument '", names(further_args)[[i]], 
                "' has unexpected length. Only arguments of length 1 are ",
                "recycled. Hence, the arguement should either be of length ", 
                length(sp_index_enc), " or 1"))
  })
  
  # check whether all variables in pos_fact have numeric counterparts in data, 
  # otherwise stop
  ## get variables of interest
  rel_var <- names(data)[pos_fact]
  ## run test for those
  integer <- vapply(data[rel_var], inherits, what = c("numeric", "integer"), 
                    FUN.VALUE = logical(1))
  
  if(!all(integer)){
    stop(paste("The following columns in 'data' cannot be decoded since they", 
               "do not inherit from class numeric or integer: \n", 
               paste(names(integer[!integer]), sep = "", collapse = " \n ")))
  }
  
  # apply cfactor to the original data, each column with its specification
  # if there are multiple columns to decode
  if(length(sp_index_lab) > 1){
    data[, pos_fact] <- as.data.frame(Map(cfactor, data[, pos_fact], 
                                          sp_index_enc, sp_index_lab, ...)) 
  } else if(length(sp_index_lab) == 1){
    data[, pos_fact] <- cfactor(data[, pos_fact], 
                                levels = sp_index_enc[[1]], 
                                labels = sp_index_lab[[1]], ...)

  }
  
  data
}