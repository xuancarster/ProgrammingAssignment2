## Put comments here that give an overall description of what your
## functions do

## The first function takes a parameter, in this case, a matrix and
## create a list of functions to call them when we need
## The second function calculates the inverse of the matrix if it is the
## first time we do, but if we already calculate it, through a global 
## variable "<<" we can call it to returns the inverse
## Write a short comment describing this function

## The makeCacheMatrix function has a parameter which is a matrix
## This fuction inicializate a variable m in the current enviroment
## The set function inicializate the input parameter of the main function
## to a x variable, and inicializate a NULL the variable m, this variable
## is in the global enviroment, so, it could be call at any time
## The get function returns the value of matrix x
## The setinverse function has de inverse matrix if it was already calculated
## and save this inverse matrix in the m variable which it could be seen in
## any enviroment
## THe getinverse returns this inverse matrix
## In the end we keep in a list every function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <-function(y) {
    x <<- y
    m <<- NULL
  }
  get <-function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## Write a short comment describing this function

## The cacheSolve function calculate the inverse matrix of the the initial, 
## x, with the solve function only, if it´s the first time. 
## If this inverse has already 
## calculated, because of we keep this inverse in a global enviroment,
## we can return its value, instead of, calculate again. If this happen, 
## it´s show in the screen the message "getting cached data"
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    m
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}