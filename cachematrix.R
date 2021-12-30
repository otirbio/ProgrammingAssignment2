## Create the inverse of a matrix


##The makecacheMatrix function save cahce data to use in the Cachesolve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##The cacheSolve function calculate the inverse of the makecacheMatrix data


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
  #check whether inverse of matrix has been already calculated
  if(!is.null(m)) {
    message("getting cached data") 
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #calculate inverse of a matrix
  x$setinv(m)
  m
}
