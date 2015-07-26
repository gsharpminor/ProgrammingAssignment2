## Put comments here that give an overall description of what your
## functions do
##These functions take a matrix as input from the global environment, solve the inverse of the matrix, cache the inverse in a separate R environment, and return it as needed so that it does not have to be solved again. 

## Write a short comment describing this function
## makeCacheMatrix takes an (invertible) matrix as argument and creates new variables and functions based on it. Since these all appear inside makeCacheMatrix, they will share the same environment and have access to the matrix. makeCacheMatrix then outputs a list of functions that can call up the needed variables and calculate the matrix inverse.
##Note: the "set" function seems unnecessary to calculate or cache the inverse, but I left it just in case. Also added in a message to show more clearly when it is calculating the inverse rather than retrieving from cache.

makeCacheMatrix <- function(x = matrix()) {
	   m <- NULL
	   set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setmat <- function(solve) {
       	      m <<- solve
              message("calculating inverse")
	       	}
       getmat <- function() m
       list(set = set, get = get,
            setmat = setmat,
            getmat = getmat)
}


## Write a short comment describing this function
## cacheSolve takes as input the list of functions output by makeCacheMatrix. It then looks whether the inverse of the matrix has already been calculated. If it has been, it retrieves it from cache (i.e., the environment). Otherwise cacheSolve uses the makeCacheMatrix functions to calculate it, then caches it and prints it out. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
               m <- x$getmat()
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setmat(m)
       m
}
