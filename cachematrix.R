## Calculate the inverse of a matrix, retaining a copy of the
## calculate inverse value.  Subsequent calls with the same
## origial matrix will return the cached copy of the inverted
## matrix instead of re-computing it each time.

## makeCacheMatrix: get/set the value of a matrix and the inverse of
## a vector.  The calculated inverse can be saved so subsequent calls
## will retrieve the cached value rather than it being recomputed
## each time.
##
## Returns an object created from matrix x with the following
## available methods:  
##     $set - Set the value of the matrix
##     $get - Get the value of the matrix
##     $setinv - Set the inverse of the matrix
##     $getinv - Get the [cached] inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL      ## matrix inverse value, initially unknown
    set <- function(y) {  ## New matrix, clear cached inverse matrix
      x <<- y
      minv <<- NULL
    }
    get <- function() { x }  ## Return the original matrix
    setinv <- function(inv) {minv <<- inv}  ## Set the inverse
    getinv <- function() { minv }  ## Return the saved inverse
    list (set = set,
          get = get,
          setinv = setinv,
          getinv = getinv )
}


## cacheSolve: caluclate the inverse value of the matrix returned
## by makeCacheMatrix().  The calculated inverse is cached.
## Subsequent calls to cacheSolve() with the same (unchanged)
## matrix will return the cached value, rather than recalculating it.
##
## Arguments:  x is a matrix created by makeCacheMatrix()
##
## Returns:  inverse of matrix (x)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  minv <- x$getinv()
  if ( !is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data,...)
  x$setinv(minv)
  minv
}

## End of File ##