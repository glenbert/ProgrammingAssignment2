## Objective: To create a pair of functions that cache the inverse of a matrix
##
## make sure to provide an invertible matrix
## makeCachematrix will create a list of the following function
            ## set = set
            ## get = get 
            ## setinverse = setinverse
            ## getinverse = getinverse
## and cache the inverse of the given sqaure matrix

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                        x <<- y
                        inv <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) inv <<- inverse
            getinverse <- function() inv
            list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cachesolve function will check if the given matrix has already been 
## calculated or already has its inverse and return its inverse.  
## If the inverse has already been calculated the function will get the
## the inverse from the cache. If not, the function will calculate the inverse
## of the matrix in the cache thru setinverse function.
cacheSolve <- function(x, ...) {
            
            inv <- x$getinverse()
            if(!is.null(i)) {
                        message("getting cached data")
                        return(inv)
            }
            m <- x$get()
            inv <- solve(m, ...)
            x$setinverse(inv)
            inv
}
