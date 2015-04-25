## Put comments here that give an overall description of what your
## functions do


## The makeCacheMatrix function creates a special "vector", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix


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

## The following function calculates the inverse of the the above function. 
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setmatrix function.

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



## Test
## x = rbind(c(10, 3), c(10, 5))
## m = makeCacheMatrix(x)

## cacheSolve(m)

## solve(x)
