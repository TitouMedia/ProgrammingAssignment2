## Put comments here that give an overall description of what your
## functions do

## 2 functions in order to cache the inverse of a matrix

## The function makeCacheMatrix" generates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
        # store inverse value
        inverse <- NULL
        # set the original matrix and reset inverse
        set <- function(y) {
                matrix <<- y
                inverse <<- NULL
                }
        ## get the original matrix
        get <- function () matrix
        ## set the inverse value
        set_inverse <- function(inv) inverse <<- inv
        ##get the inverse value
        get_inverse <- function () inverse
        
        # Return a list of the 4 functions = the special "matrix"
        list (set = set, get = get,
              set_inverse = set_inverse,
              get_inverse = get_inverse)
        
}


## This function computes the inverse of the special "matrix" (smatrix) returned by 
## makeCacheMatrix above


cacheSolve <- function(s_matrix, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inverse <- s_matrix$get_inverse ()
        
        if(!isnull(inverse)) {
                message ("getting cached data")
                return(inverse)
                }
        data <- s_matrix$get()
        inverse <- solve(data,...)
        s_matrix$set_inverse (inverse)
        inverse
       
}
