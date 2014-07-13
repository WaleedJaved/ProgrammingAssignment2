## These functions take a matrix as an input and see if the inverse has already
## been created or not. If it is, and the contents of the matrix have not changed,
## then the inverse is returned from cache. 
## Otherwise, it is calculated and saved in cache.

## This function takes a matrix as an input and creates an object that can cache
## the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function (y){
     x <<- y
     inv <<- NULL
   }
   get <- function () x
   setinv <- function(inverse) inv <<- inverse
   getinv <- function() inv
   list(set = set, get = get, setinv = setinv,
        getinv = getinv)
}


## this function takes the class created through previous function, checks for
## its inverse, computes the inverse if it is a new matrix and returns 
## the inverse from cache if otherwise. 

cacheSolve <- function(z, ...) {
      
          invr <- z$getinv()
          if (is.null(invr)) {
            matrix <- z$get()
            invr <- solve(matrix)
            z$setinv(invr)
            invr
          }
          else{
          invr <- z$getinv()
       print("Getting Cached Data")
          invr}
}
      