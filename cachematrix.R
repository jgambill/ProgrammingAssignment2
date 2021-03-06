## This R script contains a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    # set: set the matrix
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    # get: get the matrix
    get <- function() x
    # setinv: set the inverse of the matrix
    setinv <- function(inv) i <<- inv
    # getinv: get the inverse of the matrix
    getinv <- function() i
    # define a list of functions for later use
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve: Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the function retrieves the inverse 
## from the cache.

cacheSolve <- function(x) {
    # get the inverse of matrix x
    i <- x$getinv()
    # if the inverse isn't null, return the cached copy
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }
    # otherwise, calculate the inverse and return it
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}

# example usage of the above functions

# define an invertible matrix
c1 <- c(1,0,5)
c2 <- c(2,1,6)
c3 <- c(3,4,0)
mat <- cbind(c1,c2,c3)
print(mat)

# cache this matrix
a <- makeCacheMatrix(mat)

# calculate the inverse of the matrix
print(cacheSolve(a))

# calculate the inverse again to see that it uses the cached copy
print(cacheSolve(a))