## In this second R assignment the task is to create a function which caches the result of
## the inverse of any given, invertible matrix.

## This function returns a list which behaves similar to an object in other languages such as C++
## The resulting object defines following in it's own enviornment
## - x - The matrix data
## - inverse - The inverse of the matrix
## - The method get() return x
## - The method set() setting x
## - The method setinverse() with which the inverse value of x can be remembered
## - The method getinverse() with which a cached inverse value can be returned
##
## When ever a new value is assigned to the matrix the cache will be cleared 
## as well.

# Creates a scope containing data of a matrix, it's inverse (if cached) and
# functions to access or modify the matrix and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
    # x in the current scope is defined via the parameter list
    # define inverse in the current scope
    inverse <- NULL
    set <- function(y) {
        # Assigns y to x in the upper scope (makeCacheMatrix).
        # x is defined via the parameter list.
        x <<- y 
        # Assigns NULL to inverse when ever matrix is changed as the previous
        # inverse will become invalid then as well.
        inverse <<- NULL 
    }
    # defines functions in the local scope
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    # return references to the just defined functions in the current scope
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cache solve function checks if a cached result of the inverse of given matrix x 
## is available. If so the cached value will be returned. Otherwise the inverse of
## the matrix will be calculated, stored in the object's cache and the just 
## created value returned.
## If the function is called a second time on the same object and the content has
## not been modified yet via set the previously cache result will be returned.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Test the functionality of our cache mechanism
c=makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1))) 
otherC=makeCacheMatrix(rbind(c(1, -1/2), c(-1/2, 1))) 
print("Original matrix:")
print(c$get())
print("Inverse matrix:")
inv_c <- cacheSolve(c)
print(inv_c)
print("Matrix inverted should result identity")
print(c$get() %*% inv_c)
print("Matrix inverted, using cacheSolve function:")
print(c$get() %*% cacheSolve(c))