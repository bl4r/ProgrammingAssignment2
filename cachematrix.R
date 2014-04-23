## This file contains two functions.
## The first one, makeCacheMatrix, stores a matrix, enabling the second function,
## cacheSolve, to find and display the matrix's inverse.

## makeCacheMatrix requires only a matrix, x.  
## It's primary purpose is to store the information required for cacheSolve to run properly.
## Note that the function does not test to make sure that the matrix provided is, in fact, invertible.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        set_solve <- function(solve) m <<- solve
        get_solve <- function() m
        
        list(set = set, get = get,
                set_solve = set_solve,
                get_solve = get_solve)
}


## cacheSolve searches for the inverse of the matrix x.
## If the inverse has already been calculated, cacheSolve indicates this to the user
## by printing a message and then displays the solution.
## If the inverse has not been stored already, the function finds it using the solve
## function and then prints the solution.

## cacheSolve requires makeCahceMatrix to already have been run.  This can be done by
## running the first function seperately and saving the results in memory or by nesting the functions.

## Like makeCacheMatrix, cacheSolve does not test if the matrix is invertible before trying to solve it.

cacheSolve <- function(x, ...) {
        m <- x$get_solve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_solve(m)
        m
}
