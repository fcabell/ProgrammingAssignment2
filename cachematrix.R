## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function will return a list of four functions to operate with the input matrix and the inverse one.
## This will create four functions to set and get the original matrix and set and get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL   ## m will be the inverse matrix
## Create four functions to set and get the original matrix and set and get the inverse matrix
        set <- function(y) { ## this will set the matrix with the argument of the function
                x <<- y
                m <<- NULL
        }
        get <- function() x ## return the cached value
        setinv <- function(inv) m <<- inv ## output will be same as input
        getinv <- function() m ## return the value of the inverse matrix
        list(set = set, get = get, ## create the list of functions
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

## We'll read the value of the inverse matrix
## If we already have a value for the inverse matrix, this will be returned.
## Otherwise, we'll read the original matrix, calculate the inverse using 
## the solve function and return that value using the functions defined in the previous function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
        m <- x$getinv() ## Get inverse value
        if(!is.null(m)) {
                message("getting cached data")
                return(m) ## Return cached value if it exists
        }
        data <- x$get()  ## Otherwise get the original value and calculate inverse
        m <- solve(data, ...)
        x$setinv(m) ## Cache inverse value
        m

}