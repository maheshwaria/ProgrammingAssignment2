## Put comments here that give an overall description of what your
## functions do
## Arvind Maheshwari
## These functions provide support for caching inverse of matrix. Matrix inverse is quite  
## resource intensive operation, if there is a scenario where we have to calculate inverse 
## of same matrix many times we should try to cache the inverse value.
## Methods in this file provide caching support for matrix inverse where matrix  inverse 
## can be stored along with matrix

## Here is one flow and sample code on how you can use these function 
## 1. First create a square matrix - remember you can't have inverse of non-square matrix.
## 2. Pass the matrix to method makeCacheMatrix and save return value in a parameter that
## you will use to access the matrix and inverse later on.
## 3. When you need inverse on matrix call cacheSolve (*NOT* the solve method provided by R)  
## Pass the variable that you got from step 2 above.
## 4. When you call cacheSolve first time - as expected matrix inverse will be calculated 
## but subsequent calls to cacheSolve will return cached value. 
## 5. cacheSolve program will display a message to point out when a cached value is used

## sample code
## myM<-matrix(c(1,9,3,7,6,5,64,5,6,8,9,4,5,2,4,8),nrow=4,ncol=4) ## you create a 4X4 matrix
## myCM<-makeCacheMatrix(myM) ## you create an object that can store your matrix and it's inverse
## cacheSolve(myCM) ## first time you won't see a message you'll just get inverse
## cacheSolve(myCM) ## next time you won't see a message "getting cached data" along with inverse

## Write a short comment describing this function
## Please refer avove this function takes a matrix as input and returns an object that contains
## matrix and placeholder for inverse. It also provides getters and setter functions for 
## matrix and inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invVal) inv <<- invVal
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## Please refer avove this function takes object returned by makeCacheMatrix as input and 
## returns inverse of matrix contained in the object.
## If the inverse saved with object is null it calculates and saves the inverse with object. 
## else it returns the cached value of inverse from the object

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
