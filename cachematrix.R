## These functions serve to create and manipulate a matrix
## with cachable inverse data.

## This function creates some basic functionality for matrix 
## inverse caching and packs it all together in a list.
## 
## Input:
##    A matrix to set up to be cacheable. Expected to be a square, solvable matrix.
##
## Return:
##    A "matrix object" (list with the set, get, setInv, and getInv functions). 
makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInv <- function(inverse) inv <<- inverse
    
    getInv <- function() inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Function to get the inverse of the matrix.
## The function will first attempt to retrieve the cached inverse value. If it 
## does not exist, the value will be calculated and cached.
##
## Input:
##    The "matrix object" (created via a "makeCacheMatrix" call) that we are to
##    determine the inverse of.
##
## Return:
##    The inverse of the input (assuming it is possible to calculate).
cacheSolve <- function(x, ...) 
{
    inv <- x$getInv()
    if (!is.null(inv))
    {
        message("Getting cached data.")
        return (inv)
    }
    
    mtx <- x$get()
    inv <- solve(mtx)
    x$setInv(inv)
    inv
}
