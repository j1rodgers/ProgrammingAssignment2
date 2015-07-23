## These are two functions which will be used to return the inverse of a matrix.
## If the inverse of the matrix has not already been calculated, than the inverse will be
## calculated, stored in the cache, and returned.  If the inverse of the matrix
## was previously calculated and cached, the cached inverse will be returned.  
## This saves time by eliminating the intenisve calculation of the inverse each time
## it is used.



## The first function, makeCacheMatrix, provides the tools to create (set) the matrix, retrieve
## (get) the matrix, store the inverse of the function (setinverse), and retrieve the inverse (getinverse).
## It uses <<- to ensure the values are available outside the function environment.

makeCacheMatrix <- function(x = matrix()) {
        
        minverse <- NULL
        
        set <- function(y) {
                x <<- y
                minverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) minverse <<- inv
        getinverse <- function() minverse
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, cacheSolve, uses makeCacheMatrix to set, get, setinverse, and
## getInverse of a matrix.  It checks to see if the cache contains a stored inverse
## of the matrix.  If so, it returns the cached inverse.  If not, it calcualtes the inverse
## of the matrix, stores the inverse in cache, and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        ## Checks to see if the inverse is already cached
        if(!is.null(inv)) {
                message("Getting cached inverse")
                return (inv)        
        }
        
        ## If inverse is not cached, calculate and cache the inverse
        data <- x$get()
        inv  <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}

