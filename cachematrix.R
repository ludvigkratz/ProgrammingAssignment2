#Creates special matrix object that are capable of caching the inverse of the matrix
#so that we only have to compute it ones for each matrix


#Creates a special matrix that is capable of caching the inverse of it
#Input: An inversiable squared matrix
#Output: A list with special function for that matrix
makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(inv) inverse <<- inv
      getInverse <- function() inverse
      list(set = set, 
           get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}

#Computes the inverse of a squared matrix
#If it has been computed before the inverse if obtained from the cache
#Input: special matrix creted by makeCacheMatrix
#Output: The inverse of that matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getInverse()
      if(!is.null(inverse)){
            message("Getting cahced data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setInverse(inverse)
      inverse
}
