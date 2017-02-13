## This is the solution to Programming Assignment 2. The code contains two
## functions. 
## The first one is makeCacheMatrix, which takes as an input a 
## matrix and gives as output a list. This list contains:
## - a function that sets the matrix (whenever we want to test a new matrix)
## - a function that gets/prints the matrix that we have stored
## - a function that sets the inverse of the matrix
## - a function that gets/prints the inverse of the matrix
##
## The second function is cacheSolve. This function does the following:
## - retrieves the value stored as the inverse in the previous function
## - if the inverse has been already stored in the previous function it prints 
##   it and then it stops
## - if the inverse is missing from the previous function then it calculates it
##   and stores it in the list of the previous function

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      
      inv <- matrix(, nrow = nrow(x), ncol = ncol(x))
      set <- function(y) {
            x <<- y
            inv <<- matrix(, nrow = nrow(x), ncol = ncol(x))
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## Cachematrix: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      my_inv <- x$getinv()
      
      if (!anyNA(my_inv)){
            
            message("getting cached data")
            return(my_inv)
      }
      data <- x$get()
      
      my_inv <- solve(data, ...)
      
      x$setinv(my_inv)
      
      my_inv
}
