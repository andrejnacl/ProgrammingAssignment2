#This code is a functionality of a matrix with its cached inverse.
#Function makeCacheMatrix below creates a 'special' matrix that caches its inverse.
#The inverse is calculated when the the function cacheSolve 
##is called on matrix created by makeCacheMatrix.

## Creates a matrix with cacheable inverse.
## Be aware, actually creates a list of functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x                   #this function returns the matrix
        setinv <- function(inv) m <<- inv     #this function sets the m, which is cached inverse storage
        getinv <- function() m                #access to cached inverse
        list(set = set, get = get,  
             setinv = setinv,
             getinv = getinv)                 #output is a list of functions
}


## This is a substitute for normal SOlve function.
## If the inverse was already calculated, it returns the cached  output.

cacheSolve <- function(x, ...) {
          m <- x$getinv()
          if(!is.null(m)) {           # we test if we can find a cache, if yes, we print it.
            message("getting cached data")
            return(m)
          }
          data <- x$get()
          m <- solve(data, ...)       # here we calculate the inverse if it is not cached
          x$setinv(m)                 #setting the inverse to cache
          m
}
