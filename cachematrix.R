## makeCacheMatrix will create a list of functions to get/set and 
## calculate the inverse of a matrix
## cacheSolve function return cached inverse matrix if possible 
## if not it will calculated it


## makeCacheMatrix creates a list containing functions to:
## set/get  the value of the matrix
## set/get  the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inv  <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve will verify if the matrix inverse is already cached
## If so, it will pop up a message "getting caching data" 
## and return the inverse of the matrix, other wise calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
