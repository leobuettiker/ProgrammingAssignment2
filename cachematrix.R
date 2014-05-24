## Functions for cached calculation of the inverse of a matrix.
## Computing the inverse of a matrix can be costly, therefore we cache the result.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseParam) inverse <<- inverseParam
        getinverse <- function() inverse
        
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
## Internaly the solve function of R is used, which might not work for every matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

##########################################################################
## Bellow some code for testing the above functionality                 ##
##########################################################################

## function for creating a hilbert matrix as seen in the helppage of solve
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

# create a hilbert matrix and output it
h8 <- hilbert(8); h8

# create a special hilbert matrix 
specialMatrix <- makeCacheMatrix(h8);
# compute the inverse
cacheSolve(specialMatrix)
# compute it again, now the message "getting cached data" should appear
cacheSolve(specialMatrix)
# set the matrix again
specialMatrix$set(h8)
# the inverse should be computed without caching, no message should appear
cacheSolve(specialMatrix)

