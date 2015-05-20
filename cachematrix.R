# - Creating a special matrix that can cache it's own inverse matrix for easily data retrieval

# - This function creates a special "matrix", which is really a list containing functions to;
#   set the value of the matrix; get the value of the matrix, set the value of the inverted matrix;
#   get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <-function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# - This function checks if the inverse of the matrix has been stored, if so it will retrieve this value;
#   Else the function with calculate the inverse of the matrix (and print it) and also store/cache the value
#   using the structure set up by the previous function
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}






