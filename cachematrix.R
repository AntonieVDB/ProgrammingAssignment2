# ### Caching the matrix inverse of a reversible square matrix
# ### calculated with the solve function
# 
# Below are two functions that are used to create a
# special object that stores a numeric matrix and caches its inverse calculated
# with the solve function.
# 
# The first function, `makeCacheMatrix` creates a special "vector", which is
# really a list containing a function to
#  
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse with solve
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)        
}


# The following function calculates the inverse of the special "vector"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setsolve`
# function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s  
}
