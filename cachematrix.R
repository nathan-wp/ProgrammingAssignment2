## Programming Assignment 2 - R Programming

## The makeCacheMatrix creates a list containing 4 functions: 
## Set to set the value of the matrix
## Get the value of the matrix
## setinv to set the inverse results of the matrix
## getinv to get the inverse of the matrix.
## When executing the makeCacheMatrix, the function is looking for a user inputted matrix 
## therefore there must be a matrix tied to an object prior to execution.


makeCacheMatrix <- function(x = matrix()) {

inv <- NULL 
##initially sets the value to NULL prior to user setting a value. 
##This is where the inverse of the matrix is stored.
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function ()x ##gets the matrix that the user inputs
setinv <- function(inverse) inv <<- inverse ##set the inverse matrix
getinv <- function () inv ##get the inverse of the matrix
list (set = set, get = get, setinv = setinv, getinv = getinv) ##tie everything into a list
}
  
## The cacheSolve Function returns the inverse of the user input matrix. However, it first checks 
## if the inverse results has already been calculated (ie. tests if inv is not null). 
## If the inversed result is present, it skips the computation, prints a message, and then returns the result.
## If there is no result, it computes the calculation (solve is the function to calculate the inverse of a matrix).
## Finally, it returns the inverse calculation.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()##gets the inversed results from x
  if(!is.null(inv)){ ##checks if the object already has inversion results
    message("getting cahced data") ##if it does, display message
    return (inv) ##return inversion results
  }
  data <- x$get()##If not, sets data as the matrix the user input
  inv <- solve(data, ...) ##solve function calculates inverse of matrix
  x$setinv(inv)##ties the solved matrix to the object
  inv ##returns inversion results
  }