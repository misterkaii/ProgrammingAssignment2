

makeCacheMatrix <- function(x = matrix(sample(1:50,25),5,5)) {
  p <- NULL
  set <- function(y) {
    x <<- y
    p <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) p <<- solve
  getsolve <- function() p
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

Part 2
cacheSolve <- function(x, ...) {
  p <- x$getsolve()
  if(!is.null(p)) {
    message("Inverse Matrix")
    return(p)
  }
  data <- x$get()
  p <- solve(data, ...)
  x$setsolve(p)
  p
}