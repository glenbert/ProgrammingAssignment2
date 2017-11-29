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

## Sample

## df <- matrix(c(2,2,3,4,4,3,1,3,9), c(3,3))


## df1 <- makeCacheMatrix(df)

##     [,1] [,2] [,3]
##[1,]    2    4    1
##[2,]    2    4    3
##[3,]    3    3    9

## cacheSolve(df1)

##      [,1]  [,2]       [,3]
##[1,]  2.25 -2.75  0.6666667
##[2,] -0.75  1.25 -0.3333333
##[3,] -0.50  0.50  0.0000000
