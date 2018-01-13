## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# first I copied the statements of the 'Caching the Mean of a Vector' example
# then I adjusted these statements to get the inverse - using 'solve' - instead 
# the mean
# the function makeCacheMatrix creates a list, which includes four functions:
# set the value of the matrix, get the value of the matrix, set the inverse
# of the matrix and get the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
# this function first checks whether the Inverse is already calculated. If so, 
# the inverse is printed
# if not, the inverse of the matrix will be calculated and printed.

# to be honest: without the 'Caching the Mean of a Vector' example it would be
# very hard for me to solve this assignment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
