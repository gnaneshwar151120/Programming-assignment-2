CacheMatrix <- function(x = matrix()) { 
  n <- NULL                              
  set <- function(y) {                  
    x <<- y                             
    m <<- NULL                       
  }
  get <- function() x                    
  
  setinv <- function(inverse) n <<- inverse 
  getinv <- function() n                    
  list(set = set, get = get, setinv = setinv, getinv = getinv)  
 
}



cacheSolve <- function(x, ...) {
 
  n <- x$getinv()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinv(n)
  n
}
