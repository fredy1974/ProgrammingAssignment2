# ProgrammingAssignment2
# Put comments here that give an overall description of what your
# functions do

# Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
  }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# The following function returns the inverse of the matrix. It first checks if the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv)) {
      return(inv)
  }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv)
      inv
}

source("ProgrammingAssignment2-master/cachematrix.R")
creat_matrix <- makeCacheMatrix(matrix(1:4, 2))
creat_matrix$get()  

creat_matrix$getInverse()
NULL
cacheSolve(creat_matrix)

creat_matrix$getInverse()

creat_matrix$set(matrix(c(1, 3, 5, 6), 2))
creat_matrix$get()

creat_matrix$getInverse()
NULL
cacheSolve(creat_matrix)

creat_matrix$getInverse()
