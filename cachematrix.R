
## Put comments here that give an overall description of what your
## functions do

## This function is used to cache a vector and a matrix that will be used later to evaluate inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrixinverse <- function(solve) m <<- solve
        getmatrixinverse <- function() m
        list(set = set, get = get, setmatrixinverse = setmatrixinverse,getmatrixinverse = getmatrixinverse)
}


## This function is used to create the inverse of a matrix that has been cached using the cacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrixinverse()
        if(!is.null(m)){
                message("getting cached data")
                return (m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrixinverse(m)
        m
}