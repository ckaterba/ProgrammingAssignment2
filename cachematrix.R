## The following functions allow one to create an (invertible)
## matrix and cache its inverse so that the inverse can be 
## called from memory rather than recalculating it. 

## makeChacheMatrix returns a list of four functions
## 1.  set(y) defines the (invertible) matrix.  If redefining, 
##            set(y) sets the inverse to NULL. 
## 2.  get() returns the invertible matrix
## 3.  setinv() defines the inverse of your matrix.
## 4.  getinv() returns the inverse of your matrix. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function(){x}
    setinv <- function(inverse){inv <<- inverse}
    getinv <- function(){inv}
    list( set = set, get = get, 
          setinv = setinv, 
          getinv = getinv)
}

## This function checks to see if the inverse of the 
## stored matrix is chached.  If so, it returns this
## inverse.  If not, it calculates,caches, and returns 
## the inverse. 

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
