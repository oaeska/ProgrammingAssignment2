## The following R script provides two functions to cache the inverse of a matrix 
##If the inverse of the matrix is already calculated and cached, then it can be returned immediately
## without redundant computation
## This first function returns a special vector that contains:
## set - set matrix 
## get - retrieve matrix
## setinv - set the inverse of the matrix
## getinv - retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## Function makeCacheMatrix is already defined
## If the inverse of the matrix is already calculated and cached 
## then it is returned immediately without redundant computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv=x$getinv();
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
        
}
