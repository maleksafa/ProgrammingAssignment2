## Lexical Scoping
## this script will solve a possibly invertible matrix using the solve() function
## the first time the matrix will be solved, the second time the script will search if the matrix
## is still the same (in the list) and will retreive the result (invertible matrix) from the list
## without solving it again.

## the makeCacheMatrix will initialize a new matrix and will eventually contain the matrix 
## and the invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolvematrix <- function(solve) m <<- solve
  getsolvematrix <- function() m
  list(set = set, get = get,
       setsolvematrix = setsolvematrix,
       getsolvematrix = getsolvematrix)
}


## the cacheSolve function will solve the matrix and store the result (invertible matrix)
## in the list of the makeCacheMatrix using the setsolvematrix() property
## the next time the cacheSolve is called it will first try to check if the matrix was 
## previously evaluated using the getsolvematrix() property, if yes then the result will be
## retreived from the cached data else it will solve it and apply it to the list

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolvematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolvematrix(m)
  m
}