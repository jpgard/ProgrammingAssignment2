## This function initializes the functions and variables needed for the 
##cacheSolve function including: get (which returns the matrix),
## setinverse(which assigns the inverse to i), getinverse, which returns
##the inverse, i, and then lists set, get, setinverse, and getinverse.


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        

}



## Returns a matrix that is the inverse of x. If this inverse has already
##been cached, the function messages "getting cached data" and returns
##the previously computed inverse; if not, it computes and returns
##the inverse of the given matrix.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
