## to create function that cache inverse matrix
##this creats a list of functions that sets and get invs of the matrix
makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setmean <- function(inverse) invs <<- inverse
        getmean <- function() invs
        list(set = set, get = get,
             setinvs = setinvs,
             getinvs = getinvs)

}
## this inverses and cache matrix. 'invs ' is returned
cacheSolve <- function(x, ...) {
        invs <- x$getinvs()
        if(!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }
        data <- x$get()
        invs <- solve(data, ...) ## this function performs the inverse of the matrix
        x$setinvs(invs)
        invs
}
