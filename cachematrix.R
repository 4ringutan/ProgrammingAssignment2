## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## Initializes object x and m 
  set <- function(y) {
    x <<- y ## Sets input x into the parent environment with '<<-'
    m <<- NULL ## Sets input m into the parent environment with '<<-' and clears any previous values of m
  }
  get <- function() x ## Retrieves x from parent environment of makeCacheMatrix
  setinverse <- function(inverse) m <<- inverse ## Sets inverse of the matrix and m is assigned to parent environment
  getinverse <- function() m ## Gets inverse of matrix
  list(set = set, get = get, ## set() and get() are given the names 'set and 'get' respectively so they can be retrieved using '$'
       setinverse = setinverse, ## setinverse() is given the name 'setinverse' for future retrieval with '$'
       getinverse = getinverse) ## getinverse() is given the name 'getinverse' for future retrieval with '$'
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- x$getinverse() ##Attempts to retrieve inverse from makeCacheMatrix
    if(!is.null(m)) {
      message("getting cached data")
      return(m) ## Checks to see if m is NULL, if it's not then returns the value
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m) ## If m was NULL then it calculates the inverse using solv() and then sets it 
    m ## Return a matrix that is the inverse of 'x'
}
