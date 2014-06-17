## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## We set in the makeCacheMatrix four functions to set and get the matrix and also to set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                setmatrix <- function(inverse) i <<- solve
                getmatrix <- function() i
                list(set = set, get = get,
                     setmatrix = setmatrix,
                     getmatrix = getmatrix)
        }


## Write a short comment describing this function
## This function gets the matrix and then checks if the inverse is already calculated, if not, then 
## it caches it.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getmatrix()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setmatrix(i)
        i
}
