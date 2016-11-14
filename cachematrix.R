## functions provide caching of previously-computed matrix inversion operations

## This function creates a special "matrix" object / container that can cache its inverse.
makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL
    get <- function() {
        matrix
    }
    set <- function(matrix = matrix()) {
        inverse <<- NULL
        matrix <<- matrix
    }
    getInverse <- function() {
        inverse
    }
    setInverse <- function(inv) {
        inverse <<- inv
        inverse
    }
    list(set = set,
         get = get,
         getInverse = getInverse,
         setInverse = setInverse)
}


## function either returns previously cached matrix inversion or performs fresh inversion
##
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
##   then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(m, ...) {

    ## lookup inverse from cached matrix
    inv <- m$getInverse()
    if( !is.null(inv) ) {
        message("inverse obtained from cache")
        return( inv )
    }
    
    ## if not cached, then compute inverse of square matrix
    inv <- solve( m$get() )
    m$setInverse( inv )
    return( inv )
}
