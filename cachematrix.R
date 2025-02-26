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

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
}
