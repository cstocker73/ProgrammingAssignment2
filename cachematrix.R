## This file consistes of two files, makeCacheMatrix and cacheSolve.
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
##changed), then the cachesolve should retrieve the inverse from the cache.
##  
## Usage example:
## > source("cachematrix.R")
## > m <- matrix(c(1,1,3,2,5,8,9,11,2), nrow=3, ncol=3)
## > mat <- makeCacheMatrix(m)
## > mat$getinv()
## NULL
## > mat$get()
## [,1] [,2] [,3]
## [1,]    1    2    9
## [2,]    1    5   11
## [3,]    3    8    2
## > mat$setinv(cacheSolve(mat))
## No cached data availabe: calculating inverse...
## > mat$getinv()
## [,1]        [,2]        [,3]
## [1,]  0.98734177 -0.86075949  0.29113924
## [2,] -0.39240506  0.31645570  0.02531646
## [3,]  0.08860759  0.02531646 -0.03797468
## > mat$setinv(cacheSolve(mat))
## Getting cached data...
## > mat$getinv()
## [,1]        [,2]        [,3]
## [1,]  0.98734177 -0.86075949  0.29113924
## [2,] -0.39240506  0.31645570  0.02531646
## [3,]  0.08860759  0.02531646 -0.03797468
## > 


makeCacheMatrix <- function(x = matrix()) {
  ## Description: create a list containing a function to set and get the value 
  ##              of the matrix and the inverse of the matrix
  ## Input: 
  ##  - Matrix
  ## Output: 
  ##  - List containing functions to:
  ##    - set the value of the matrix
  ##    - get the value of the matrix
  ##    - set the inverse of the matrix  
  ##    - get the inverse of the matrix 
  ## 
  ## Author: C.S.
  ## Date: 2015-07-25
  ## Version: Beta
  ## Observations: based on the following example:
  ## https://class.coursera.org/rprog-030/human_grading/view/courses/972581/assessments/3/submissions
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 
  get <- function() x
  setinv <- function (solve) m <<- solve
  getinv <- function() m
  
  # Outout
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  ## Description: calculate and cache the inverse of a matrix. If the calculation 
  ##              has been executed previouly, it returns the cached value. 
  ##              According to the assignment's text it is assumed that the matrix 
  ##              supplied is always invertible.
  ## Input: 
  ##  - CacheMatrix
  ## Output: 
  ##  - Inverse of CacheMatrix$get():
  ## 
  ## Author: C.S.
  ## Date: 2015-07-25
  ## Version: Beta
  ## Observations: based on the following example:
  ## https://class.coursera.org/rprog-030/human_grading/view/courses/972581/assessments/3/submissions
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting cached data...")
    return(m)
  } 
  message("No cached data availabe: calculating inverse...")
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
