## Caches inverse of a matrix and provides operations to get and set matrix
## and get and set matrix inverse.

## Returns vector of functions "get", "set", "setinverse" and "getinverse"
## set - sets value of matrix
## get - gets value of matrix
## setinverse - sets matrix inverse
## gets matrix inverseS
makeCacheMatrix <- function(x = matrix()) {
        #1. Create special matrix object with operation inverse
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Takes special function vector, obtains cached matrix.
## If inverse is not NULL ( already calculated), inverse is returned.
## Else, it solves for inverse.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
                
        # get inverse
        # if it isn't null, get the cached data and return
        # set inverse and return
        ## Return a matrix that is the inverse of 'x'
}
