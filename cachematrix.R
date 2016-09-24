## Creates invertable matrix and caches the inverse
## 

## Creates an intertable matrix that can be cached

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmsolve <- function(solve) m <<- solve
  getmsolve <- function() m
  list(set = set, get = get,
       setmsolve = setmsolve,
       getmsolve = getmsolve)
}


## Inverts a matrix and caches the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmsolve(m)
  m
  }
