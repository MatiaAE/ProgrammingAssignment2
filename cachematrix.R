## File contains two functions, makeCacheMatrix(), and cacheSolve()
## makeCacheMatrix() creates a special matrix capable of storing its inverse.
## cacheSolve() calculates inverse of makeCacheMatrix().

  #Function creates special matrix that can store its inverse.
  #Function assumes provided matrix is invertible.


makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x<<- y
    inv<<- NULL
  }
  setInv <- function(inverse) {inv<<- inverse}
  getInv <- function() inv
  get <- function() {x}
  list(set = set, get = get,setInv=setInv, getInv=getInv)
}


  #cacheSolve takes a matrix of type makeCacheMatrix() and returns the inverse.
  #cacheSolve assumes matrix is invertible.
  
cacheSolve <- function(x,..){
  inv<- x$getInv()
  if(!is.null(inv)){
      message("Retrieving cached inverse.")
      return(inv)
  }
  matrix<- x$get()
  message("Computing inverse.")
  inv<- solve(matrix)
  x$setInv(inv)
  inv
}
