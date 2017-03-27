## This functions will cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {     #if already in the cache, return it directly. 
        message("getting cached inverse data")    
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)   # solve() returns the inverse of the input matrix  
    x$setinverse(inv)   # save the inverse data to the special matrix
    inv #return the inversed data
}

## To test
#m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
#myMatrix_object <- makeCacheMatrix(m1)
#cacheSolve(myMatrix_object)
#cacheSolve(myMatrix_object)
#
#testmtx2 <- matrix(c(9,1,9,7,2,3,6,7,8,0,3,1,4,1,5,9), 4,4)
#cachedtestmtx2 <- makeCacheMatrix(testmtx2)
#cacheSolve(cachedtestmtx2)
#cacheSolve(cachedtestmtx2)
