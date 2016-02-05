## these 2 functions solve the inverse of square matrix
## and add it to cache 

## Returns a list of functions:
## 1. set - sets value of matrix
## 2. get - gets value of matrix
## 3. setInv - adds matrix inverse to cached data
## 4. getInv - gets matrix inverse from cached data

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(new_inv) inv <<- new_inv
    getInv <- function() inv
    list(set = set, get = get, 
         setInv = setInv, getInv = getInv)
}

## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(x)
    x$setInv(inv)
    inv
}
