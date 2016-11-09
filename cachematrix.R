## ProgrammingAssignment2
##
## Two functions are created in this assignment, with the purpose of 
## calculating the inverse of a matrix and storing it in Cache for future use.
## Provided that the matrix has not changed, the inverse is not
## calculated again, but recalled from Cache

## NOTE: It is assumed that the matrix supplied is a square invertable matrix

## The first function, makeCacheMatrix, creates a special "matrix", which is 
## a list containing 4 functions to: 
## (1) set the value of the matrix 
## (2) get the value of the matrix 
## (3) set the value of the inverse
## (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          # NOTE: `<<-` is used to assign a value to an object in an environment 
          # different from the current environment
          x <<- y
          inv <<- NULL
         }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The second function, cacheSolve, calculates the inverse of the special matrix
## created by the makeCacheMatrix function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the 
## data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        # get the value of the inverse in the cache with the getinv function.
        minv <- x$getinv()
        
        # if the inverse has already been calculated, i.e. minv is not NULL
        if (!is.null(minv)){
           # no computation required, use the matrix in the cache, 
           # give a suitable message and return the matrix 
           message("using cached data")
           return(minv)
        }
        
        # otherwise, calculate the inverse, i.e. minv was NULL 
        data <- x$get()
        minv <- solve(data, ...)
        
        # set the value of the inverse in the cache with the setinv function
        # and return the inverse matrix
        x$setinv(minv)
        
        minv
}

