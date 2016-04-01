## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Input: x -  NxN matrix must be invertible
makeCacheMatrix <- function(x = matrix()) {
            m <- NULL ## m - this is where inverse matrix stored, empty at the start
            
            set <- function(y) { 
              x <<- y
              m <<- NULL ##clears cache
            }
            
            get <- function() x
            
            setinv <- function(inv) m <<- inv #
            
            getinv <- function() m 
            
            list(set = set,       get = get,
                 setinv = setinv, getinv = getinv) #returns 'special' matrix
            ##set - sets the matrix x to the given value y
            ##get - returns the matrix x
            ##setinv - sets the value of m in the cache to the given value
            ##getinv - returns the value of m from the cache

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 ##Check if inverse is stored in the cache
   m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
 ##If cache is empty, calculate the inverse  
  data <- x$get() ##assigne matrix with the data to data
  m <- solve(data, ...) ##invert matrix
  x$setinv(m) 
  m
}
