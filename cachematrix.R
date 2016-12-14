## This code calculates the inverse of a matrix and stores it 
##in cache.
## It calculates the inverse or retrieves from cache if previousl 
##calcualted

## The following function creates a special matrix object
##that can cache its inverse

makeCacheMatrix <- function(M = matrix()) {
        i <- NULL
        set <- function(y) {
                M <<- y
                i <<- NULL
        }
        get <- function() M
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function calculates the inverse of the matrix from
##the above function, or retrieves from cache if already computed

cacheSolve <- function(L, ...) {
        i <- L$getinv()
        if(!is.null(i)) {
                message("getting stored inverse")
                return(i)
        }
        data <- L$get()
        i <- solve(data)
        L$setinv(i)
        i
}

## Enter matrix to be solved here
L <- makeCacheMatrix(matrix(2:5,2,2))


