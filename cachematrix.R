##these two functions are used to cache the value of the inverse of a matrix.
## the formatting and function are very similar to the example set of makevector and cachemean

## creates a list containing function set/get the value of the matrix/inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <- function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) i<<- inverse
  getinverse<-function() i 
  list(set = set, get = get,
       setinverse= setinverse,
       getinverse = getinverse)
}


## checks to see if there is a cached value for the inverse of the matrix, if there is,
## returns the cached value, if not, it calculates the inverse and returns it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data)
  x$setinverse(i)
  i
}
