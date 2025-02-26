## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#R Programming Assignment 2
#Make Vector Function
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

#Cache Mean Function
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

#Creating A Matrix for the Cache
#makeCacheMat is a function which creates a special "matrix" object that can cache its inverse for the input (which is an invertible square matrix)
makeCacheMat <- function(x=matrix(sample(1:125,16),4,4)){
  z <- NULL
  set <- function(y){
    x <-y
    z <- NULL
  }
  get <- function()x
  setsolve <- function(solve) z <<- solve
  getsolve <- function() z
  list(set=set, get = get, setsolve=setsolve, getsolve=getsolve)
}

#Cache Solving Function
#cacheSolve is a function which computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x,...){
  z <- x$getsolve()
  if(!is.null(z)){
    message("inverse matrix")
    return(z)
  }
## Return a matrix that is the inverse of 'x'
  data <- x$get()
  z<- solve(data,...)
  x$setsolve(z)
  z
}

