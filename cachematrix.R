## Date: 05-16
## Author: Cuby-DS

## Week 3 Assignment of R Programming Class
## This assignment has the goal to practice all concepts learned so far in the 
## class, including lexical scoping, matrix operation and functions.
## In this assignment we create an object that stores matrix data and
## we add functionality to cache the value of the inverse of the matrix.


## This function stores an object that represents a "matrix". It stores its 
## contents and also caches the value of the inverse of the matrix.
## Input: x is a matrix
## Output: object with private functions that allow to get the content and to
##  get/set the value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize cache of inverse to NULL
        my_inverse <- NULL
        ## Auxiliary functions: get, setinverse, getinverse
        ## Return the matrix value
        get <- function() {
                x
        }
        ## Sets the cache value
        setinverse <- function(inverse_value) {
                my_inverse <<- inverse_value
        }
        ## Returns the cache value, NULL if it hasn't been computed yet
        getinverse <- function() {
                my_inverse
        }
        list(get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This functions returns the inverse of a matrix
## If the value has been computed before it returns the cached value
##  otherwise it computes the inverse and caches the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        my_inverse <- x$getinverse()
        if(!is.null(my_inverse)) {
                message("getting cached data")
                return(my_inverse)
        }
        # Since there was cached value, it gets the matrix data and computes 
        # the inverse
        data <- x$get()
        my_inverse <- solve(data, ...)
        ## store the value to avoid re-computing the inverse for this matrix
        x$setinverse(my_inverse)
        my_inverse
}


## To test, use the following example taken from the Discussion Forums
## Link: https://www.coursera.org/learn/r-programming/discussions/weeks/3/
## threads/ePlO1eMdEeahzg7_4P4Vvg
##> m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
##> myMatrix_object <-makeCacheMatrix(m1)
##> cacheSolve(myMatrix_object)
##[,1] [,2]
##[1,]    6    8
##[2,]    2    4
