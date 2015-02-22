## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. 
## Below a pair of functions that cache the inverse of a matrix is defined.

## Below are two functions defined that are used to
## create a special object that stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "vector", which 
## is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <‐ NULL
    set <‐ function(y) {
        ## In the next two lines the <<- operator is used to assign a value to 
        ## an object in an environment that is different from the current 
        ## environment.
        x <<‐ y
        m <<‐ NULL
    }
    get <‐ function() x
    setsolve <‐ function(solve) m <<‐ solve
    getsolve <‐ function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## The following function calculates the inverse of the special "vector" created 
## with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <‐ x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## initialize matrix "A"  
    data <‐ x$get()
    ## create coresponding unity matrix E
    e <- diag(1,nrow(data))
    ## calculate the inverse of A by solving the equation AX=E
    m <‐ solve(data,e, ...)
    x$setsolve(m)
    m
}
## Tests for first calculating and afterwards retreving from cache for 
## testmatrices:
## A1 = 1 3 2 4 (2 x 2)             giving        
##          inv(A1) = -2 1.4 1 -0.5
## A2 = 1 0 1 2 4 0 3 5 6 (3 x 3)   giving 
##          inv(A2) = 1.091 0.227 -0.182 -0.545 0.136 0.091 -0.091 -0.227 0.182 
## A3 = 1 0 5 2 1 6 3 4 0 (3 x 3)   giving 
##          inv(A3) = -24 20 -5 18 -15 4 5 -4 1
## went according to expectations.