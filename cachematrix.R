#
# The function, makeCacheMatrix creates a special "vector", 
# which is really a list containing the functions to
# 1. set the value of the matrix ( "set" is used within the function)
# 2. get the value of the matrix ( see example of the use of "get" below)
# 3. set the value of the inverse of the matrix (in variable "inverse")
# 4. get the value of the inverse of the matrix ( function "getinverse")
#
# Example 1
# 1. Define a matrix with: m <- matrix(c(-1, -2, 1, 1), 2,2)
# 2. Cache the matrix with:  x <- makeCacheMatrix(m) 
# 3. Now the cached matrix can be recovered with: x$get(), whose result is:
#          [,1] [,2]
#    [1,]   -1    1
#    [2,]   -2    1
# 4. Next, compute the inverse with: inv <- cacheSolve(x)
# 5. Visualize the inverse with: inv, whose result is:
#          [,1] [,2]
#    [1,]    1   -1
#    [2,]    2   -1
# 5. Verify that "inv" is the inverse with: m %*% inv, whose result is:
#           [,1] [,2]
#    [1,]    1     0
#    [2,]    0     1
# In summary, makeCacheMatrix stores a matrix and caches its inverse
#
makeCacheMatrix <- function(x = numeric()) {
    inverse <- NULL
    set <- function(y) {  # The function "set":
        x <<- y           # stores the original matrix in "x"
        inverse <<- NULL  # and its initial inverse (NULL)
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# The function "cacheSolve" calculates the inverse of the matrix 
# created with the above function. However, it first checks to see if 
# the inverse has already been calculated. If so, it gets the inverse 
# from the cache and skips the computation. Otherwise, it calculates 
# the inverse of the data and sets the value of the inverse in the 
# cache via the setinverse function.
#
# Example 2
# After running the instructions from "Example 1", run again: 
# inv <- cacheSolve(x)
# whose result is the inverse, in variable "inv", and the message:
# getting cached data
# The inverse can be printed with: inv, or with: x$getinverse()
#
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()    # recover the stored inverse
    if(!is.null(inverse)) {      # If the inverse is NULL then
        message("getting cached data")
        return(inverse)          # return the cached inverse
    }
    data <- x$get()      # Else, recover the original matrix
    inverse <- solve(data, ...)  # compute its inverse
    x$setinverse(inverse)        # keep the new inverse
    inverse                      # return the new inverse
}
