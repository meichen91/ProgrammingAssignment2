## This package contains function to precompute the inverse of a matrix and
## store the inverse in the cache. Therefore it does not require repetitive
## computation when we need the inverse again.

## This function creates a special "matrix" object that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.
## Example: 
## A <- matrix(rnorm(9), 3, 3)
## A2 <- makeCacheMatrix(A)
## A_inv <- cacheSolve(A2)
## round(A %*% A_inv, 3) # to check the results
## A_inv2 <- cacheSolve(A2) # getting cached data

cacheSolve <- function(x, ...) { 
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
