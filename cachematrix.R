## Put comments here that give an overall description of what your
## functions do

## I will do it for the next assignment... I promise!! :-)

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
  

}


## I hope this too...

cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
        ## Return a matrix that is the inverse of 'x'
}

mtrx1 <- matrix(1:4, 2)
mCMx1 <- makeCacheMatrix(mtrx1) 
inv1 <- cacheSolve(mCMx1)
test1 <- mtrx1 %*% inv1
mtrx2 <- matrix(8:11, 2)
mCMx2 <- makeCacheMatrix(mtrx2)
inv2 <- cacheSolve(mCMx2)
test2 <- mtrx2 %*% inv2
