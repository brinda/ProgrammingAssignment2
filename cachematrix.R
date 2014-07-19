## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The makeCacheMatrix function returns a list of functions which include:
# 1. get: returns the value of the matrix
# 2. set: sets the value of the matrix for whom the inverse is to be computed.
# 3. getInverse : returns the inverse of the matrix
# 4. setInverse: sets the inverse of the matrix.

makeCacheMatrix <- function(mtrx = matrix()) {
  inverse <- NULL # initialize the inverse to NULL
  
  set <- function (y) {  # sets the matrix
    mtrx <<- y   # set the value of the matrix
    inverse <<- NULL # initialize the inverse to NULL
  }  
  get <- function(){   # returns the matrix
    mtrx
  }  
  setInverse <- function(inv) { # sets the value of inverse 
    inverse <<- inv
  }  
  getInverse <- function() { # returns the value of the inverse 
    inverse
  }
  
  list(get= get, set=set, getInverse = getInverse, setInverse= setInverse) 
  # returns a list of functions that can be called on the special Matrix.

}


## Write a short comment describing this function

# this function uses the matrix created in the above function, 
#and gets the inverse of that matrix - 
# if the inverse is already computed, it just returns it (from the cache)
# if it is not already computed(not in the cache), it is computed, and the value is stored in the cache and also returned.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse() ## Return a matrix that is the inverse of the matrix in 'x'
  if(!is.null(inverse)) { # check if already computed
    inverse 
  }
  
  matrix <- x$get() # fetch the matrix from the special Matrix 
  inverse <- solve(matrix) # compute the inverse of the matrix
  x$setInverse(inverse) # store the value in the cache ( for future use)
  inverse
  
}
