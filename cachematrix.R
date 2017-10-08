## This combination of functions can be used to calculate the inverse a matrix, and
## is especially useful for large matrices used in multiple iterations of a loop, for
## example, since this would be resource intensive to calculate every time. Rather, these
## functions can store the inverse in the cache and just retrieve it each iteration. 

## The first function, `makeCacheMatrix` creates a special "vector", or list, 
## which contains named functions to 
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the matrix inverse result
## 4.  get the matrix inverse result

makeCacheMatrix <- function(x = matrix()) {
  invmat<-NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
    }
  get<-function() x
  setinvmat<-function(inversed_matrix) invmat <<- inversed_matrix
  getinvmat<-function() invmat
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}

## The second function uses as input a makeCacheMatrix type "vector" or list. It then
## checks if the matrix inverse is stored in the cahce, and if so retrieves it using getinvmat. If not,
## it calculates the inverse and stores it in the cache using the 'setinvmat' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinvmat()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }  
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinvmat(invmat)
  invmat
}

## Example:
## m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## myMatrix_object <- makeCacheMatrix(m1)
## cacheSolve(myMatrix_object)
## [,1] [,2]
## [1,]    3    7
## [2,]    1    5
## cacheSolve(myMatrix_object)
## getting cached data
## [,1] [,2]
## [1,]    3    7
## [2,]    1    5