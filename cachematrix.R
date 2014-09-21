## Creates a special matrix that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
# get matrix value
get <- function() x
        # set matrix inverse
        setinverse<- function(inverse) inv <<-inverse
        getinverse <- function() inv
        # give matrix inverse value
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}



 ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
     z <- x$getinverse()                #inverse of the matrix
        # check matrix
        if (!is.null(z)) {
                message("getting cached inv_matrix")
        return(z)
        } else {
                z <- solve(x$get())
        # set matrix inverse
                x$setinverse(z)
        
        return(z)       # return the inverse of the matrix
        }

       
}
