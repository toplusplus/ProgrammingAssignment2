## The functions below, taking advantage of the scoping rules of R, allows build
## special matrices whose inverses are calculated just one time and cached to 
## avoid recompute them again. 


## Makes a special matrix and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)  
}


## Calculates the inverse of an 'special' matrix or retrieves it if it was 
## computed before.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## T E S T I N G  
## The code below proves the 'makeCacheMatrix' and 'cacheSolve' functions

## 'm1' is an invertible matrix 
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

## 'm1inv' is the inverse of 'm1' 
m1inv <- solve(m1)
m1inv

## Creates a cached matrix named 'myMatrix'
myMatrix <- makeCacheMatrix(m1)

## Calculates and caches the inverse of 'myMatrix'
myMatrixInv <- cacheSolve(myMatrix)
myMatrixInv

## If 'cacheSolve' is doing it good, the line below has to return TRUE
identical(m1inv, myMatrixInv)

## And now 'cacheSolve' has to return the inverse of 'myMatrix' cached
cacheSolve(myMatrix)

## Lets probe the set function of makeCacheMatrix
m2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
m2

## 'm2inv' is the inverse of 'm2' 
m2inv <- solve(m2)
m2inv

## Sets 'm2' as the new matrix belong to 'myMatrix'
myMatrix$set(m2)

## Calculates and caches the inverse of 'myMatrix' again
myMatrixInv <- cacheSolve(myMatrix)
myMatrixInv

## If 'cacheSolve' is doing it good, the line below has to return TRUE, as before
identical(m2inv, myMatrixInv)

## And now 'cacheSolve' has to return the inverse of the new 'myMatrix' cached
cacheSolve(myMatrix)