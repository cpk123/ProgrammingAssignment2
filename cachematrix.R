## This function creates a special "matrix" object
## that can cache its inverse.
##
## Usage:
##      somematrix <- matrix(1:4,2,2)
##      cm <- makeCacheMatrix(somematrix)
##      cacheSolve(cm)  # computes inverse matrix and stores in cm
##      # both of following matrix multiplies now returns identity matrix
##      cm$get() %*% cm$getinv()
##      cm$get() %*% cacheSolve(cm) # also prints "getting cached data"
##                                  # (does NOT recalculate inverse)

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    # define methods for the special matrix object
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inv) inverse <<- inv
    
    getinv <- function() inverse
    
    # return a list of accessor methods defined for object
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


##  `cacheSolve`: This function computes the inverse of the special
##  "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then
##  `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getinv()
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data)
    x$setinv(x_inv)
    x_inv
}
