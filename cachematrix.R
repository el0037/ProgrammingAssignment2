# makeCacheMatrix is a function that is used to do the following tasks:
# 1. Setting the matrix
# 2. Getting the matrix 
# 3. Setting the inverse 
# 4. Getting the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# The second function i.e. the cacheSolve() function is used to calculate the
# inverse of the matrix, which is returned by the first function. 
# It checks whether the inverse has been previously
# calculated or no. If calculated, the caced inverse data is returned and if
# no, then inverse of the matrix is calculated and the value is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}