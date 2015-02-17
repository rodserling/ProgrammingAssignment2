##
## These 2 functions cache square functions and their inverse: 
##
##
## The 1st function makeCacheMatrx() creates a list of 4 functions which
## simply caches a matrix, retrieves the cached matrix, 
## computes/caches the inverse of the matrix or retrieves the cached inverse.
## More is discussed later about this function.
##
## The 2nd function cacheSolve() will only calculate the inverse of a matrix 
## if it isn't yet cached.  Either way the inverse is returned.
## More is discussed later about this function.
##
## Specifically makeCacheMatrix(mat1=matrix()) will cache 'mat1' ( e.g.mat2<-makeCacheMatrix(mat1) )
## and return a list with the following functions:
##  -cache a matrix mat1 ( mat2$set(mat1)), 
##  -return the cached matrix, if it exists ( mat2$get() ) 
##  -find the inverse of a matrix and cache it ( mat2$setSolve(solve(mat1) )
##  -return the cached matrix inverse if it exists ( mat2$getSolve() )
##
makeCacheMatrix <- function(x = matrix()) {
      m<- NULL
      set <- function(y) {
            m<<-NULL
            x <<- y
      }
      get <- function() x
      setSolve <- function(solve) m <<- solve
      getSolve <- function() m
      list(set = set, get = get, 
           setSolve = setSolve,
           getSolve = getSolve)
}
##
## cacheSolve(mat2=list()) shall first check if the inverse of the matrix is already cached
## in the list.  If so, the user is prompted that the inverse exists and the inverse matrix is 
## returned from the cache.  If the inverse doesn't exist, the inverse is calculated, 
## cached and returned to the user.
##
cacheSolve <- function(x=list(), ...) {
      m <- x$getSolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setSolve(m)
      m
}
