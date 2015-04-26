## This R script contains two functions used to calculate the inverse or a matrix. It allows to 
## cache the calculated inverse matrix and getting it back to avoid recalculating it, and 
## therefore save on computation times.

## makeCacheMatrix() function creates a vector/list of functions that can be applied on a matrix
## The functions are as follows:
##    - set_mt(): allows the user to set the matrix for which the inverse will be calculated
##    - get_mt(): gets the matrix specified by the user
##    - set_invmt(): sets the calculated inverse matrix
##    - get_invmt(): gets the calculated inverse matrix (for example to check its existence)

makeCacheMatrix <- function(x = matrix()) {
  
        invmat <- NULL
        
        set_mt <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get_mt <- function () x
        
        set_invmt <- function(solve) invmat <<- solve
        
        get_invmt <- function() invmat
        
        list(set_mt=set_mt,
             get_mt=get_mt,
             set_invmt = set_invmt,
             get_invmt =get_invmt)
}


## cacheSolve() function calculates the inverse of a matrix, or return the cached matrix if it has already been calculated

cacheSolve <- function(x, ...) {
        invmat <- x$get_invmt()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get_mt()
        invmat <- solve(data, ...)
        x$set_invmt(invmat)
        invmat
}
