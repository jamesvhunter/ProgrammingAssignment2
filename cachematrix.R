## These paired functions allow you to solve for an inverse of a matrix 
## and store the result special object that caches the inverse. 

## Create the "special matrix" list

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Takes input from above and returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setsolve(m)
    m
}

