## Put comments here that give an overall description of what your
## functions do

## The following funtion creates a numeric vector that stores the matrix

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set<- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) m <<-inverse
  getmatrix <- function() m
  list(set=set, get=get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}

## The following function caculates the invese of the above vector/matrix

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data, ...)
  x$setmatrix(m)
  m
}





