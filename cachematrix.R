##  cache the inverse of a matrix
##  model command that can be used to test its function
#> a<-makeCacheMatrix()
#> cacheSolve(a,matrix(c(1,2,2,1),2))

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix


makeCacheMatrix <- function() {
  m <- NULL
  x<-matrix()
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  get <- function() x
  setInverseMatrix <- function(invert) m <<- invert
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix )
  
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above


cacheSolve <- function(x, y=matrix(), ...) {
  
  ##test if the matrix has changed
  ##if changed, cache the new matrix and calculate and cache the inverse of the new matrix
  ##otherwise, retrieve the inverse from the cache
  
  
  if(!identical(x$get(),y)) x$set(y)
  else {m <- x$getInverseMatrix()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }}
  
  m <- solve(y, ...)
  x$setInverseMatrix(m)
  m
}
