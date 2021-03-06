% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cc.R
\name{cc}
\alias{cc}
\title{Enhanced control for combining values}
\usage{
cc(..., recursive = FALSE)
}
\arguments{
\item{...}{objects to be concatenated.}

\item{recursive}{logical.  If \code{recursive = TRUE}, the function
    recursively descends through lists (and pairlists) combining all
    their elements into a vector.}
}
\description{
Enhanced control for combining values
}
\details{
This function returns the same as base \code{\link{c}}, but if all
 elements of \code{...} are of class factor or ordered, it returns the lowest
 common denominator, i.e. an object of class ordered or factor. 
 Ordering of the levels (also for unordered factors) is only preserved if it 
 is identical accross all element in \code{...}. Otherwise, it is determined 
 by \code{\link{cfactor}}, which is called internarlly. Refer to the section 
'Details' in the \code{\link{cfactor}} documentation for further details.
}
\examples{
char1 <- as.factor(sample(letters, 5))
char2 <- as.factor(sample(letters, 5))
cc(char1, char2) # probably more reasonable than
c(char1, char2) 
\dontrun{
## warnings
# no warning since levels identical
a_to_d <- cfactor(letters[1:2], levels = letters[1:5])
b_to_d <- cfactor(letters[1:5])
cc(a_to_d, b_to_d)

# warning for differing order of levels
a_to_d <- cfactor(letters[1:2], levels = letters[1:5])
d_to_a <- cfactor(letters[1:2], levels = rev(letters[1:5]))
cc(a_to_d, d_to_a)

# warning for different levels
some_levels <- cfactor(letters[1:2], levels = letters[1:5])
all_levels <- cfactor(letters[1:2], levels = letters)
cc(all_levels, some_levels, some_levels)
}
}

