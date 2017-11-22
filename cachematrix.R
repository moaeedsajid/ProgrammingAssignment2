## cachematrix.R Moaeed Sajid
## V1 - Orginal Version
##
## makeCacheMatrix will be used a cache to store the matrix and it's inverse.  cacheSolve 
## will look for the inverse from makeCacheMatrix(mcm).  On the first run this will not be available
## and so will need to be calculated by cacheSolve and returned for mcm storage.  On subsequent runs 
## the inverse will be in the mcm cache and not need to be calculated

## makeCacheMatrix (mcm) gets and sets the matrix and inverse values  
## makeCacheMatrix function must be called at least once to create the storage objects.  Subsequent
## calls can use get and set against the store
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL # As we are writng the matrix here only the inverse value is set to NULL
        setmatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        getmatrix <- function() x
        # setinverse <- function(csi) i <<- csi
        # csi is the inverse value as calculated from the cacheSolve function
        # Above setinverse line rewritten as below to understand better
        setinverse <- function(csi) {
                i <<- csi
        }
        getinverse <- function() i
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The cacheSolve function will look for the inverse from makeCacheMatrix. If this is the first time
# the request is made then getinverse will return a null value.  In this case we will solve the 
# inverse and send it to makeCacheMatrix for storage.  Subsequent requests for the same matrix will
# retrieve the inverse from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Data available in cache, retrieving")
                return(i) # No need to do any more work and function will end with this return
        }
        # If getinverse returns null then retrieve the matrix, solve inverse, store and print
        data <- x$getmatrix()
        i <- solve(data, ...)
        x$setinverse(i)
        i

}
