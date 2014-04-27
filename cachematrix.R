## The functions below create a special "matrix" object that can cache its inverse
## and show the use of this special "matrix" object


## The function contains the get and set methods for this "matrix" object and for the inverse of the matrix value
makeCacheMatrix <- function(x = matrix()) {
      
    ## set xInv - Inverse matrix variable to NULL
    xInv <- NULL
    
    ## set function will set the value of the matrix in the "matrix" object
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    
    ## get function will return the matrix value
    get <- function() x
    
    ## setMatrixInv function will cache the value of the matrix inverse after computing
    setMatrixInv <- function(matrixInv) xInv <<- matrixInv
    
    ## getMatrixInv function will return the cached value of the matrix inverse 
    getMatrixInv <- function() xInv
    
    ## list contatining all the functions for this special "matrix" object
    list(set = set, get = get,
         setMatrixInv = setMatrixInv,
         getMatrixInv = getMatrixInv)
}


## This function returns the inverse of a special "matrix" object
## If the inverse matrix is not already cached, it will be computed and cached for future use
cacheSolve <- function(x, ...) {
        
    ## Return a matrix that is the inverse of 'x' from cahce
    xInv <- x$getMatrixInv()
    
    ## If the value returned is NULL, compute the inverse
    if(!is.null(xInv)) {
        message("getting cached value for the inverse matrix")
        return(xInv)
    }
    
    ## Inverse matrix is not cached, hence compute the value
    
    ## get the value of the matrix
    matrix <- x$get()
    
    ## Use the solve() function to calculate the matrix inverse
    xInv <- solve(matrix)
    
    ## cache the matrixInv value
    x$setMatrixInv(xInv)
    
    ## retrun the inverse matrix
    xInv
}
