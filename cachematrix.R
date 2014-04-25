## code location: https://github.com/saoconnell/ProgrammingAssignment2.git
## 
## makeCacheMatrix:
##    Create a special that sovles the inverse of a matrix, 
##      caching the result
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## 
## cacheSolve:
##    Invert a special matrix created with the makeCacheMatrix,
##      if it a new reference, it will calculate the inverse, if it has
##      been previously calculated it will use the cached value.
##
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}


##  NOT RUN:

## TEST FUNCTION
## create an invertible matrix
#hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
#h8 <- hilbert(8)

## make a special matrix with the makeCacheMatrix function
# iX <- makeCacheMatrix(h8)

## use the special matrix form of invertible matrix, and calculate
##    The inverse, use the cached value if presviously calculated and 
##    the matrix has not changed
# system.time(cacheSolve(iX))

