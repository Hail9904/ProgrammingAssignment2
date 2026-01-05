
# Below is the makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL          # this will store the inverse
  
  # set a new matrix and reset stored inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the matrix
  get <- function() x
  
  # store the calculated inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # get the stored inverse
  getinverse <- function() inv
  
  # return a list of functions
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


# Below is the cacheSolve function


cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  # if already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # otherwise compute inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # cache it
  x$setinverse(inv)
  
  inv
}

