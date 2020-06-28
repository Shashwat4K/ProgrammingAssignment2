
# Auxiliary function which calculates and returns inverse of a matrix
calculateInverse <- function(mat) {
    # Using solve() to calculate matrix inverse
    # First check if the matrix is square or not
    if(nrow(mat) != ncol(mat)) {
        return(NaN) # Return NaN if the matrix is not square.
    }
    # Then check whether its determinant is 0 or not 
    else if(det(mat) == 0) {
        return(NaN)  # Return NaN if determinant is 0. 
                     # As the matrix is not invertible
    }
    # If the matrix is square and invertible, calculate the inverse
    solve(mat)
}

# By referring the example, I've written an equivalent code
# for making a cache matrix. Here, the only difference was,
# that we have to store 'matrix inverse' rather than 'mean'
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # inverse of the matrix 'x'
    
    # setter method to assign values to 'x' and 'inv'
    # We can change the value of 'x' here
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # getter method
    # Returns the matrix 'x' stored
    get <- function() x
    
    # sets the 'inv' variable the value: 'myinv'
    setInverse <- function(myinv) inv <<- myinv 
    
    # gets (returns) the computed inverse 'inv'
    getInverse <- function() inv
    
    # bind all these functions to a list
    # Equivalent to creation on a Java object
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    # Check whether inverse is precomputed,
    # if yes, then directly return that
    if(is.null(inverse) == FALSE) {
        message("Inverse is already calculated, returning the data")
        return(inverse)
    }
    # Otherwise...
    else {
        # since the inverse wasn't already cached, 
        # we need to calculate it from scratch
        mat <- x$get()
        inverse <- calculateInverse(mat)
        x$setInverse(inverse)
        return(inverse)
    }
}