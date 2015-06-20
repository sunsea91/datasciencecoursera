## Assignment: Caching the Inverse of a Matrix
## Pair of functions that cache the inverse of a matrix
## Assumption: matrix supplied is always invertible

##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      #set up blank space for inverse matrix
      v <- NULL
      # substitutes x (input of main function) for y (input for this function) & v restores null value to the inverse matrix
      set <- function(y) {
        x <<- y
        v <<- NULL
      }
      #prints x
      get <- function() x
      #stores inverse matrix in main function
      setinverse <- function(inverse) v <<- inverse
      #prints inverse matrix
      getinverse <- function() v
      #list to store all the functions defined in this function
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

 


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      #determine whether v has been stored previously (and print "getting cached data)--> IF statement
      v <- x$getinverse()
      if(!is.null(v)) {
        message("getting cached data")
        return(v)
      }
      #else statement: store data from original input into a matrix
      data <- x$get()
      #calculate the inverse
      v <- solve(data, ...)
      #using cached inverse (retrieval)
      x$setinverse(v)
      #print it
      v #matrixinverse
}









