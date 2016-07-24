##The assignment is to write a pair of functions that cache the inverse of a matrix.

## The first function creates a special "matrix" object that can cache its inverse.
## The second function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix will create a matrix x, and have four methods to set and get x and its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## initialize inverse with NULL
    cachedInv <- NULL
    
    ## set x
    set <- function(matr = matrix()) {
        x <<- matr 
        cachedInv <<- NULL
    }
    
    get <- function() x
    
    ##set inverse variable and return value for checking
    setInverse <- function(inv) {
        cachedInv <<- inv 
        return(cachedInv)
    }
    ##get Inverse
    getInverse  <- function() cachedInv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve first checks to see if there's already a cached inverse and return if there is one
## Otherwise solve its inverse and set/return it

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { 
    
    ## Already computed?
    calculatedInverse <- x$getInverse() 
    
    ##check if there is a cached value and it is a matrix?
    if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
        message("Already computed")
        return(calculatedInverse)
    }
    
    ## otherwise get the matrix
    matrixToSolve <- x$get()  
    
    ## solve the matrix, catch errors and warnings (went maybe wrong, if it is not invertible)
    calInv <- tryCatch({ 
        solve(matrixToSolve)
    }, warning=function(w) {
        message("Something went wrong with the data")
        message(w)
    }, error=function(e) {
        message("Something went wrong solving your matrix")
        message(e)
        message("\n")
    })
    
    ##set the value of the inverse (set it NULL if there was an error or warning)
    x$setInverse(calInv)
}