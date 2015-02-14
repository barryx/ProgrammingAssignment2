## R Programming Assignment 2
## The following two functions demonstrate how the result of a computation can 
## be cached to avoid unnecessary duplication of work. In this case, we cache 
## the inverse of a matrix.

## makecacheMatrix creates a special "matrix" object that can cache its inverse.
## It has functions to:
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse matrix
##   4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    get <- function() x
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    getInverse <- function() inv

    setInverse <- function(inverse) inv <<- inverse
    
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## cacheSolve computes the inverse of a special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## cachesolve retrieves the inverse from the cache. 
## NOTE: This function assumes the matrix can be inverted.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}


## testFunc creates a special matrix with makeCacheMatrix and then makes two calls 
## to cacheSolve. It should print out:
##   1. the base matrix
##   2. the inverse matrix
##   3. a message indicating the cached inverse was retrieved
##   4. the identity matrix (all 0's except a line of 1's from top left to bottom right)
##   5. Repeat 1-4 on a different matrix

test <- function() {
    m1 <- matrix(c(0, 1, 2, 
                   1, 0, 2, 
                   2, 1, 1), 3, 3)
    Mat <- makeCacheMatrix(m1)
    
    
    print(Mat$get()) 
    print(cacheSolve(Mat)) 
    Identity <- Mat$get() %*% cacheSolve(Mat) ## calculate Identity Matrix using cached inverse
    print(round(Identity, 2)) ## round is there to clean up floating point issues

    message()
    message("Resetting Matrix")
    m2 <- matrix(c(10, 10, 20, 10, 
                   10,  0, 30,  0, 
                   20, 20,  0, 10,
                   0,  10,  0,  0), 4, 4)
    Mat$set(m2)
 
    print(Mat$get()) 
    print(cacheSolve(Mat)) 
    Identity <- Mat$get() %*% cacheSolve(Mat) ## calculate Identity Matrix using cached inverse
    print(round(Identity, 2)) ## round is there to clean up floating point issues
    
}

