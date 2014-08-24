# makeCacheMatrix is a function that take as argument a square matrix, in this 
# function:
# set is a function that stores in the cache the matrix (argument of makeCacheMatrix) 
# and initialize in the cache the value s of solve(matrix);
# get get the matrix; 
# setSolve apply the function solve (inverse of the square matrix) and stores 
# in the cache the value s of solve(matrix);
# getSolve get the value s of solve(matrix);
# finally the MakeCachematrix create a list with 4 elements containing the 
# functions: set, get, setSolve and getSolve.

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


## cacheSolve is a function that take as argument the value of makeCacheMatrix,
## to s is assigned the value stored in getSolve,
## the if loop check if the value is null, if not it print the message 
## "getting cached data" and return the value s stored inth cache by getSolve 
## and end the caheSolve function;
## if s is null to data is passed the matrix makeCacheMatrix$get, to s is 
## assigned the value of solve(matrix) and then is passed the value s to setSolve 
## that will store it in the cache, finally s is returned
## 

cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s     ## Return a matrix that is the inverse of 'x'
}

mtrx1 <- matrix(1:4, 2)                 # make a square matrix
mCMx1 <- makeCacheMatrix(mtrx1)         # assign the makeCaheMatrix value
inv1 <- cacheSolve(mCMx1)               # assign to inv1 the cahesolve valu 
test1 <- mtrx1 %*% inv1                 # check if the value of inv1 is the inverse of mtrx1

mtrx2 <- matrix(8:11, 2)
mCMx2 <- makeCacheMatrix(mtrx2)
inv2 <- cacheSolve(mCMx2)
test2 <- mtrx2 %*% inv2
