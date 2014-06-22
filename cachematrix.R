## This functions calculate the inverse of a matrix. The functions save time 
## consuming calculations by checking if the inverse matrix has already been 
## calculated.

## makeCacheMatrix creates a list of functions to
## 1. set the value of an invertible matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y  
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv 
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix created with the above function.
## It checks to see if the inverse has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
