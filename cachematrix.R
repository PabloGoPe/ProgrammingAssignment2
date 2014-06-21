## This functions can calculate the inverse of a matrix.
## The first time it is calculated the value is chached so the next time it is
## calculated it just uses the value already calculated.

## This function returns a list with the value of the matrix and that list will
## store the value of the inverse once it is calculated for the first time

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        ## The following line prevents for keeping an old inverse value
        ## when using a new vector
        inv <<- NULL
    }
    get <- function() x
    calcinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         calcinv = calcinv,
         getinv = getinv)
}


## This function checks if the inverse has been already calculated. 
## If there is a value in cache it just returns that value.
## Elsewhere, it calculates the inverse matrix as usual

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$calcinv(inv)
    inv
}
