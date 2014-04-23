## This program is made to solve matrixes
##
## In simple cases the best option to solve a matrix is the function solve() but in some cases this could be a costly computation
## Some times we need to reatedly solve a matrix. The ideia of this program is to  cache the results of the calculation
## so that if you have to solve a matrix more than once, you don't need to do the whole operation again.
##
## This program transorms a regular matrix into a "special" matrix, if you solve this "special" matrix
## with the fuction cacheSolve() it will solve the original matrix, just like the solve() function, and
## the result will be stored. If you solve the same "special" matrix again it will not have to calculate de inverse again



## This function transforms a matrix into a Special matrix to be used in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {

       m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Calculates de inverse of the "Special" matrix criated by the makeCacheMatrix function. If it has already done this operation, it will get the result from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
