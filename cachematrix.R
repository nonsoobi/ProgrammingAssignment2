## The functions below are used to create a special object that stores a matrix and caches its inverse.

## This funstion creates a matrix objact that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv = NULL
         set = function(y) {
                 # use `<<-` to assign a value to an object in an environment 
                 # different from the current environment. 
                 x <<- y
                inv <<- NULL
         }
         get = function() x
         setinv = function(inverse) inv <<- inverse 
         getinv = function() inv
         list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function below calculates the inverse of the matrix created by makeCacheMatrix above. The function should retrieve the inverse from the cache, if the inverse has already been calculated (i.e if the matrix has not changed)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
         if (!is.null(inv)){
                 message("getting cached data")
                 return(inv)
         } 
         mat.data = x$get()
         inv = solve(mat.data, ...)
         x$setinv(inv)
         
         return(inv)

}
