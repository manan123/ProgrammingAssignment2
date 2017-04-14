## Coursera Data Science: R Programming, Assignment Week 3, 
## Git Hub user: UMESH K

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inv_var <- NULL                                ## to hold value of matrix inverse 
    set_fun <- function(y) {                    ## defining the set_fun function to assign new 
        x <<- y                                 ## value of matrix in parent environment
        inv_var <<- NULL                        ## if there is a new matrix, reset inv_var to NULL
    }
    get_fun <- function() x                     ## define the get_fun fucntion - returns value of the matrix argument
    
    setinverse <- function(inverse) inv_var <<- inverse  ## assigns value of inv_var in parent environment
    getinverse <- function() inv_var                     ## gets the value of inv_var where called
    list(set_fun = set_fun, get_fun = get_fun, setinverse = setinverse, getinverse = getinverse)  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {         ## Return a matrix that is the inverse of 'x'
  inv_var <- x$getinverse()
     if(!is.null(inv_var)) {             ## Check if inv_var is already available in current environment
         message("getting cached data")
         return(inv_var)
     }
     data <- x$get_fun()
     inv_var <- solve(data, ...)
     x$setinverse(inv_var)
     inv_var
}
