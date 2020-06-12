## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  z<- NULL
  set <-function(y){
    x<<-y
    z<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse)z<<-inverse
  getInverse<-function()z
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## This function calculates the inverse of the special vector 'x'. 
##It first checks if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips further computation. 
##Else the inverse is calculated 
##and sets it to the cache through the solve function 

cacheSolve <- function(x, ...) {
  ## Returning a matrix that is the inverse of the invertible matrix 'x'
  z<-x$getInverse()
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  mat<-x$get()
  z<-solve(mat,...)
  x$getInverse(z)
  z
}
