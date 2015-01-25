
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    
    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    
    ## get the value of the matrix 
    get <- function() x
    
    ## set the value of the solve
    setsolve <- function(solve) invMat <<- solve
    
    ## get the value of the mean
    getsolve <- function() invMat
    
    ## a list contains 4 functions
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    invMat <- x$getsolve()
    
    if(!is.null(invMat)) { ## Check if exists already calculated inverse matrix 
        
        message("getting cached data")
        
        return(invMat)
        ## Return a cached matrix that is the inverse of x without calculation
    
    } else {
        
        message("getting calculate data")
        
        data <- x$get()
        invMat <- solve(data, ...)
        x$setsolve(invMat)
        invMat

        ## Return a matrix that is the inverse of 'x' with calculation
    }
}
