# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
 i<-NULL
  set<-function(y){
  x<<-y
  z<<-NULL
}
get<-function() x
setmatrix<-function(solve) i<<- solve
getmatrix<-function() i
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getmatrix()
    if(!is.null(i)){
      message("getting cached data")
      return(i)
    }
    matrix<-x$get()
    i<-solve(matrix, ...)
    x$setmatrix(i)
    i
}
## Sample run:
## > x = rbind(c(1, -1/2), c(-1/2, 1))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,] 1.00 -0.5
## [2,] -0.5 1.00
## No cache in the first run
## > cacheSolve(m)
## [,1] [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
## [,1] [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
## > 
