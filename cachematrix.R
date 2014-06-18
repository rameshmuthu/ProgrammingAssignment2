## MakeCacheMatrix 
# parameter : any invertible matrix
# set & get : value of input matrix & its inverted Matrix

makeCacheMatrix <- function(x = matrix()) {
  mat_inversed <- NULL
  
  set <- function(y) {
    x <<- y
    mat_inversed <<- NULL
  }  
  get <- function() x
  
  setinverse <- function(inverse) mat_inversed <<- inverse
  getinverse <- function() mat_inversed
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve
# stores matrix & its inverted matrix.
# returns inverted matrix from cache if same matrix inputed otherwise inverts new matrix

cacheSolve <- function(x, ...) {
  
  mat_inversed <- x$getinverse()
  if(!is.null(mat_inversed)) {
    message("-- Getting from cache --")
    return(mat_inversed)
  }
  
  data <- x$get()
  mat_inversed <- solve(data, ...)
  
  x$setinverse(mat_inversed)
  
  mat_inversed
}
