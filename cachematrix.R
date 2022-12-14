## makeCacheMatrix = creates special matrix, acts as an input to cacheSolve
## cacheSolve = solves for the inverse of the matrix created in makeCacheMatrix.
## if the inverse is already calculated, cacheSolve will retrieve the result from the cache

makeCacheMatrix <- function(x = matrix()) { ##makeCacheMatrix defines x as the matrix

    m <- NULL ##set to null for use later in the function
    set <- function(y) {
        x <<- y ##assigns y to x
        m <<- NULL ##assigns null to m
    }
    get <- function() x ##defines the getter for x
    setinverse <- function(inverse) m <<- inverse ##defines the setter for the inverse assigns the input to m
    getinverse <- function() m ##defines the getter for the inverse
    list(set = set, get = get, ##assigns names to the set and get functions using a list 
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve will solve for the inverse of the matrix created by makeCacheMatrix
## if the inverse is already solved, it will retrieve the results from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) { ##if the value is not null the function will return the message in the next line
        message("getting cached data")
        return(m)
    }
    data <- x$get() ##retrieves the value of x if the above value is null, assigns it to variable data
    m <- solve(data, ...) ##calculates the inverse
    x$setinverse(m)##sets the inverse
    m ##returns the inverse
    }
