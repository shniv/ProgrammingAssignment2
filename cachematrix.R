## Caches inverse of a matrix and provides operations to get and set matrix
## and get and set matrix inverse.

## Caches matrix and returns list of functions 
## to get and set matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        ## set matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get matrix
        get <- function() x
        
        ## set inverse of matrix
        setInverse <- function(inverse) m <<- inverse
        
        ## get inverse of matrix
        getInverse <- function() m
        
        ## return list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Takes special function vector, obtains cached matrix.
## If inverse is not NULL (already calculated), inverse is returned.
## Else, it solves for inverse.
cacheSolve <- function(x, ...) {
        
        ## Gets stored inverse
        m <- x$getInverse()
        
        ## check if inverse null
        if(!is.null(m)) {
                ## if not null, inverse has been set so return cached matrix
                message("getting cached data")
                return(m)
        }
        
        ## get original matrix
        data <- x$get()
        
        ## compute inverse matrix
        m <- solve(data, ...)
        
        ## set inverse matrix 
        x$setInverse(m)
        
        ## return inverse matrix
        m
}
