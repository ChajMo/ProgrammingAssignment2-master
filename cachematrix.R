## assignmentment: caching the inverse of a matrix
## For certain computations such as matrix inversions, it may
## sometimes prove useful to cache the result rather than repeating
## thecomputation. caching saves time. For this pair of functions,
## there purpose is to create a matrix and cache its inverse.

## This function creates an object, "matrix" that caches its inverse.

makeCacheMatrix <- function(x = matrix()){
        Inv <- NULL
        set <- function(y){
                x <- y
                Inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) Inv <<- inverse
        getInv <- function() Inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the matrix created by the
## first function. If the computation has already been done on the
## same matrix then the inverse is retrieved.

cacheSolve <- function(x, ...){
	## Return a matrix that is the inverse of 'x'
        Inv <- x$getinverse()
        if(!is.null(Inv)){
                meassage("Acquiring cached data.")
                return(Inv)
        }
        m <-x$get()
        Inv <- solve(m, ...)
        x$setinverse(Inv)
}
