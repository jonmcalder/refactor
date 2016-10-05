# Helper function for both cut.integer and cut.ordered
# Takes numeric x input along with a scalar breaks argument, and produces 
# evenly spaced integer bins

cut_breakpoints <- function(x, breaks, right, include.lowest){

############################## assertive checks ################################

  assert(
    test_class(x, "numeric"),
    test_class(x, "integer")
  )
  assert(
    test_class(breaks, "numeric"),
    test_class(breaks, "integer")
  )
  assert_class(right, "logical")
  assert_class(include.lowest, "logical")
  
############################## assertive checks completed ######################
  
  range <- max(x)-min(x)+1
  avg_bin_width <- floor(range/breaks)
  rem <- range %% breaks
  num <- breaks+1
  
  # if data range can be evenly split into desired number of breaks
  if(rem == 0){
    breakpoints <- seq(from=min(x)-1, by = avg_bin_width, length.out = num)
    breakpoints[1] <- min(x)
    
    # if data range can't be evenly split into desired number of breaks
  } else if(rem != 0) {
    
    # make leftmost bins slightly larger
    if(right == FALSE){
      
      # start from the right and work backwards creating the breakpoints
      # then reverse the vector to get it in the correct direction
      breakpoints <- rev(seq(from=max(x), by = -avg_bin_width, length.out = num))
      # set correct point (lowest value) for first breakpoint
      breakpoints[1] <- min(x)
      
      # allocate remaning 'bin space' by widening intervals starting from 
      # the left
      for(i in 1:rem){
        breakpoints[i+1] <- min(x)-1+avg_bin_width*i+i
      }
      
      # make rightmost bins slightly larger
    } else if(right == TRUE){
      
      # start from the left and work forwards creating the breakpoints
      breakpoints <- seq(from=min(x)-1, by = avg_bin_width, length.out = num)
      # set correct point (highest value) for last breakpoint
      breakpoints[num] <- max(x)
      
      # allocate remaning 'bin space' by widening intervals starting from 
      # the right
      for(i in num:(num-rem+1)){
        breakpoints[i-1] <- max(x)-(avg_bin_width+1)*(num-i+1)
      }
      
      # handle edge case of bin width 1 with lowest breakpoint equal to 
      # min(x)
      if (breakpoints[2] == min(x)){
        # start from 1 below the min value for x and override include.lowest 
        # so that label will be correct for (expected) bin width of 1
        include.lowest = FALSE
      } else {
        breakpoints[1] <- min(x)
      }
      
    }
  
  }
  
  return(list(breakpoints = breakpoints, include.lowest = include.lowest))
  
}

# handle break offsets for 'right' and 'left' intervals
# and also handle include.lowest = TRUE
# returns adjustment vectors for bins (which impact labels)
bin_adjust <- function(right, include.lowest, numLabels){
  
  if(right == TRUE){
    floorInc    <- rep(1, numLabels)
    ceilingDec  <- rep(0, numLabels)
    if(include.lowest == TRUE){
      floorInc[1] <- 0
    }
  } else if(right == FALSE) {
    floorInc    <- rep(0, numLabels)
    ceilingDec  <- rep(1, numLabels)
    if(include.lowest == TRUE){
      ceilingDec[numLabels] <- 0
    }
  }
  
  return(list(floorInc = floorInc, ceilingDec = ceilingDec))
  
}