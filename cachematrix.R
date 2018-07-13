##This function sets the value of the matrix as an input and caches the matrix


makeCacheMatrix <- function(x = matrix()) {
        invertMatrix <- NULL
  
#set the value of the Matrix
        setMatrix <- function(y) {
                x <<- y
                invertMatrix <-- NULL
}
#similar to the makeValue of mean function; want to be able to get the matrix and its inverse
        getMatrix <- function() x
        setInverse <- function(inverse) invertMatrix <<- inverse
        getInverse <- function() invertMatrix
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse,
             getInverse = getInverse)
}
        
 ##this function is used to check to see if there is any cached Matrix data from the previous function
##if there is data, it returns the inverted matrix
cacheSolve <- function(x, ...) {
        invertMatrix <- x$getInverse()
        if(!is.null(invertMatrix)) {
                message("Getting Cached Inversed Matrix")
                return(invertMatrix)
        }
        data <- x$getMatrix()
        invertMatrix <- solve(data, ...)
        x$setInverse(invertMatrix)
        return(invertMatrix)
        ## Return a matrix that is the inverse of 'x'
}
