
makeCacheMatrix <- function(x = matrix()) { 
  minv <- NULL  
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) minv <<- inverse
  get_inverse <- function() minv
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}
cacheSolve <- function(x, ...) {
  minv <- x$get_inverse()
  if(!is.null(minv)) {
    message("getting cached data.")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data)
  x$set_inverse(minv)
  minv
}




    
