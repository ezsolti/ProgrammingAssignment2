##the functions below are caching the inverse of a matrix. For this purpose the first function caches the matrix, and defines 
##functions to set and get the inverse of the matrix. The second function (cacheSolve) actually computes the inverse but only if it
##has not been computed previously.

# example input/outputs
# > B<-makeCacheMatrix(matrix(c(22,322,323,114,215,632,217,228,329),3,3))
# > B$get()
# [,1] [,2] [,3]
# [1,]   22  114  217
# [2,]  322  215  228
# [3,]  323  632  329
# > B$getinv()
# NULL
# > cacheSolve(B)
# [,1]          [,2]          [,3]
# [1,] -0.003082998  0.0041872896 -0.0008683631
# [2,] -0.001357156 -0.0026413990  0.0027256592
# [3,]  0.005633833  0.0009631296 -0.0013438763
# > B$getinv()
# [,1]          [,2]          [,3]
# [1,] -0.003082998  0.0041872896 -0.0008683631
# [2,] -0.001357156 -0.0026413990  0.0027256592
# [3,]  0.005633833  0.0009631296 -0.0013438763
# > B$set(matrix(c(222,3252,3243,1114,2115,6232,2137,2228,329),3,3))
# > B$get()
# [,1] [,2] [,3]
# [1,]  222 1114 2137
# [2,] 3252 2115 2228
# [3,] 3243 6232  329
# > B$getinv()
# NULL
# > cacheSolve(B)
# [,1]          [,2]          [,3]
# [1,] -0.0004048069  3.975087e-04 -6.254429e-05
# [2,]  0.0001889283 -2.104671e-04  1.981181e-04
# [3,]  0.0004115119  6.841993e-05 -9.677992e-05
# > cacheSolve(B)
# getting cached data
# [,1]          [,2]          [,3]
# [1,] -0.0004048069  3.975087e-04 -6.254429e-05
# [2,]  0.0001889283 -2.104671e-04  1.981181e-04
# [3,]  0.0004115119  6.841993e-05 -9.677992e-05

## makeCacheMatrix creates a special "matrix", which is really a list containing functions to
## set and get its value, and set and get its inverse (setinv and getinv)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  

}


## cacheSolve receives a matrix as an input, it first checks whether the inverse of the matrix is already available
##if yes, then it returns that
##if not, it computes the inverse of the matrix, and sets the result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
