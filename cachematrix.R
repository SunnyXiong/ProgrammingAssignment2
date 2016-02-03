## For some of the costly computation such as matrix inversion and there may be 
## some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. Below are a pair of functions that are used to create a special
## object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversionmatrix <- NULL
        set <- function(y) {
                x <<- y
                inversionmatrix <<- NULL
        }
        get <- function()
                x
        setInverse <- function(inverse)
                inversionmatrix <<- inverse
        getInverse <- function()
                inversionmatrix
        list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversionmatrix <- x$getInverse()
        if (!is.null(inversionmatrix)) {
                message("getting cached data")
                return(inversionmatrix)
        }
        mat <- x$get()
        inversionmatrix <- solve(mat, ...)
        x$setInverse(inversionmatrix)
        inversionmatrix
}