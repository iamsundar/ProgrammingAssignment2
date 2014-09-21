
## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  #set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get function
  get <- function() x
  
  #setinverse function
  setinverse <- function(solve) m <<- solve
  
  #getinverse function
  getinverse <- function() m
  
  #return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, then cacheSolve returns the cached inverse

cacheSolve <- function(x = matrix(), ...) {
  
  #check if inverse is cached already and return
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #compute inverse if not in cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

