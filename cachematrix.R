## This function is create a matrix and cache matix values ans created a inverse
## of the matrix
## This function create a matrix and cache of the matrix

makeCacheMatrix <- function(a = matrix()) { 
  inv  <- NULL 
  set  <- function(b){ 
  a <<- b 
  inv <<- NULL  
  } 
  get  <- function() a 
  setinverse  <- function(inverse) inv  <<- inverse 
  getinverse  <- function() inv  
  list(set= set, get = get,  
       setinverse = setinverse,  
       getinverse = getinverse)   
} 
## This function calculate inverse of the matrix
cacheSolve <- function(a, ...) {  
  inv  <- a$getinverse()  
  if (!is.null(inv)){  
                  message("getting cached data")  
                  return(inv)  
                  } 
  data  <- a$get()  
  inv  <- solve(data, ...)  
  a$setinverse(inv)  
  inv 
} 

