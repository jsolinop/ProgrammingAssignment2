## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## param x is a numeric matrix - assumed that it's an invertible-square matrix -
## The function makeCacheMatrix returns a list of 4 functions:
## - set: stores de matrix and sets the inverse (volve) at NULL
## - get: recalls the value of the Matrix (data)
## - setsolve: calculates inverse of the matrix and stores it on inv
## - getsolve: recalls the inverse of the matrix
## finally creates a list with all the functions


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  
}


## A short comment describing this function: 
## Param X is a list with the result of the execution of makeCacheMatrix
## cacheSolve checks if the inverse matrix is previous calculated. 
## If not, it calculates it and stores it in the cache ($Setsolve method)
## If a inverse was previously calculated, it gets de data from de cache
## via the "getsolve" method

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
  
}
