
##  overall description of what your
## functions do

## This program has 2 functions implemented.
#
# 1. makeCacheMatrix creates a special matrix object out of a given matrix.
# 2. cacheSolve computes and returns inverse of this special matrix object.

# TESTED THE PROGRAM AS FOLLOWS, with actual commands on console:

# > A = matrix( c(2,4,3,1,5,7,4,2,7), nrow=3, ncol=3, byrow=TRUE)
# > A1 = matrix( c(2,4,3,0,5,7,4,2,6), nrow=3, ncol=3, byrow=TRUE)

# > aInv <- solve(A)
# > aInv
# [,1]        [,2]        [,3]
# [1,]  0.2916667 -0.30555556  0.18055556
# [2,]  0.2916667  0.02777778 -0.15277778
# [3,] -0.2500000  0.16666667  0.08333333
#> solve(A1)
# [,1]       [,2]       [,3]
# [1,]  0.1904762 -0.2142857  0.1547619
# [2,]  0.3333333  0.0000000 -0.1666667
# [3,] -0.2380952  0.1428571  0.1190476
# > 
# USE THIS PROGRAM: 
# myCacheA <- makeCacheMatrix(A)
# > cacheSolve(myCacheA)
# [,1]        [,2]        [,3]
# [1,]  0.2916667 -0.30555556  0.18055556
# [2,]  0.2916667  0.02777778 -0.15277778
# [3,] -0.2500000  0.16666667  0.08333333
# > cacheSolve(myCacheA)
# getting cached data
# [,1]        [,2]        [,3]
# [1,]  0.2916667 -0.30555556  0.18055556
# [2,]  0.2916667  0.02777778 -0.15277778
# [3,] -0.2500000  0.16666667  0.08333333
# > cacheSolve(myCacheA)
# getting cached data
# [,1]        [,2]        [,3]
# [1,]  0.2916667 -0.30555556  0.18055556
# [2,]  0.2916667  0.02777778 -0.15277778
# [3,] -0.2500000  0.16666667  0.08333333
# > myCacheA1 <- makeCacheMatrix(A1)
# > cacheSolve(myCacheA1)
# [,1]       [,2]       [,3]
# [1,]  0.1904762 -0.2142857  0.1547619
# [2,]  0.3333333  0.0000000 -0.1666667
# [3,] -0.2380952  0.1428571  0.1190476
# > cacheSolve(myCacheA1)
# getting cached data
# [,1]       [,2]       [,3]
# [1,]  0.1904762 -0.2142857  0.1547619
# [2,]  0.3333333  0.0000000 -0.1666667
# [3,] -0.2380952  0.1428571  0.1190476
# >

## Write a short comment describing this function

# 1.  `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.

# 
# Computing the inverse of a square matrix is being done with the `solve`
# function in R, and this in this program is done in next function 'cacheSolve'

# methods implemented in makeCacheMatrix
# set
# get
# setInverse
# getInverse

makeCacheMatrix <- function(x,.. ) {
  cacheInverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    cacheInverseMatrix <<- NULL
  }
  get <- function() {
    x
  }  
  setInverse <- function(inverseMatrix) {
    cacheInverseMatrix <<- inverseMatrix
  }
  getInverse <- function() {
#     if (is.null(cacheInverseMatrix)) {
#       cacheInverseMatrix <- solve(x)
#     }
    cacheInverseMatrix  
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function

# 2.  `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` would retrieve the inverse from the cache.

cacheSolve <- function(cacheObj,..) {
  
  cacheInverseMatrix <- NULL
  
  ## Return a matrix that is the inverse of 'x'
  # cacheObj <- makeCacheMatrix(x)
  # assuming that argument passed is the special matrix object
  
#   if ( !is.object(cacheObj)) {
#     # need to run   x <- makeCacheMatrix(y) first..
#     return(cacheInverseMatrix)
#   }
  cacheInverseMatrix <- cacheObj$getInverse()
  if(!is.null(cacheInverseMatrix)) {
    message("getting cached data")
    return(cacheInverseMatrix)
  }
  data <- cacheObj$get()
  cacheInverseMatrix <- solve(data)
  cacheObj$setInverse(cacheInverseMatrix)
  cacheInverseMatrix
}


