## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 z<-NULL
  set<-function(y){
  x<<-y
  z<<-NULL
}
get<-function() x
setmatrix<-function(solve) z<<- solve
getmatrix<-function() z
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        k<-x$getmatrix()
    if(!is.null(k)){
      message("getting cached data")
      return(k)
    }
    matrix<-x$get()
    k<-solve(matrix, ...)
    x$setmatrix(k)
    k
}
