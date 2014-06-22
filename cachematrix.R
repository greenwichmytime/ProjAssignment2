## Caching the Inverse of a Matrix
## Assignment by GreenwichMyTime
## This functions create a special "matrix" object in order to store a matrix, and cache its is inverse.
## This behaviour can help to improve performance if the inverse of a matrix has to be calculated 
## repetitively (e.g. in a for loop)


## makeCacheMatrix <- function(x = matrix()) 
## The first function, makeVector creates a special "matrix", 
## which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the Inverse
## 4. get the value of the Inverse
## Input:          x -> data of a matrix
## output:	Special "matrix" object with its list of four functions. 


## Getting the Cached values in order to input into cacheSolve() function
## via creating the cacheable matrix:

makeCacheMatrix <- function(original.matrix = matrix()) {
        
        # Check the validity of input:
        
        if (!is.matrix(original.matrix)) {
                stop("Input a matrix")
        }
        
        inverted.matrix <- NULL
        
        set <- function(y) {
                original.matrix <<- y
                inverted.matrix <<- NULL
        }
        
        # Creating cached inverted matrix value:
        get <- function() original.matrix
        
        # Inverting stage by using solve() function:
        set.inverse <- function(solve) inverted.matrix <<- solve
        get.inverse <- function() inverted.matrix
        
        list(
                set = set,
                get = get,
                set.inverse = set.inverse,
                get.inverse = get.inverse)
        
}


## Here the makeCacheMatrix() gives the computed inverse of cacheable matrix.
## So the cached inverse is returned by the cacheSolve() if the inverse computed
## and the matrix is unchanged

cacheSolve <- function(cacheable.matrix, ...) {
        inverted.matrix <- cacheable.matrix$get.inverse()
        # Is the cached matrix available?
        if(!is.null(inverted.matrix)) {
                message("Get the cached inverse matrix!")
                return(inverted.matrix)
        }
        # Create the inverted matrix when the cached matrix is not available.
        matrix.to.inverse <- cacheable.matrix$get()
        inverted.matrix <- solve(matrix.to.inverse)
        cacheable.matrix$set.inverse(inverted.matrix)
        inverted.matrix
        
}
