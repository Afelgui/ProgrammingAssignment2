## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function create a object that contains  2 variables (x and m) 
## and 4 functions, x is our matrix, m is inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <<- matrix(,nrow(x),ncol(x))
  set <- function(y) {
    x <<- y
    m <<- matrix(,nrow(x),ncol(x))
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## cachesolve function works with special matrix object, it inverse matrix x if
## inversed matrix m doesn't exist, 
## and just return m and write message if inverse already exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.na(m[1,1])) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}
