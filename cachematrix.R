## Put comments here that give an overall description of what your
## functions do

## Creates the primary matrix object along with get/set functions

makeCacheMatrix <- function(x = matrix()) {
  #preliminary checks and caching the object
  #validation checks for input data
  if(!is.matrix(x)){
    stop ('Data Type Mismatch: Matrix expected')
  }
  
  if(nrow(x)!= ncol(x)){
    stop('Matrix not invertible. Structure n x n expected')
  }
  
  if(length(na.omit(x)) < length(x)){
    stop('NA Values encountered in data')
  }
  
  c_mtrx <<- NULL
  
  #set fn: used to modify contents after instantiation
  set <- function(ip_mtrx) {
    x <<- ip_mtrx
    c_mtrx <<- NULL
  }
  
  #get fn: returns the source data used for computation
  get <- function() x
  
  #set fn: directly assigns matrix as inverse matrix to the cached object
  setinv <- function(inv_mtrx) c_mtrx <<- inv_mtrx
  
  #get fn: returns calculated and cached object
  getinv <- function() c_mtrx
  
  #return: list vector with function names
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Uses the makeCacheMatrix function in order to check and retrieve accordingly

cacheSolve <- function(x, ...) {
  #get the inverse
  inv_mtrx <- x$getinv()

  #check for null and populate inverse
  if (is.null(inv_mtrx) | ){
    message("Populating inverse")
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    return(m)
  }
  
  # inverse retrieved directly
  message("Fetching from cache")
  return(inv_mtrx)
}
