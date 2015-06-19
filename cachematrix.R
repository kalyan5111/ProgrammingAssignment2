## functions to cache and solve the inverse of a matrix to improve performance
## (to avoid redundant calculation)

## makeCacheMatrix is a function to set and get the input matrix and the inverse of it

makeCacheMatrix <- function(mat_std = matrix()) {
        mat_inv <- NULL                 # mat_inv is the matrix inverse: initialised here
                                        # mat_std is the input
        set <- function(input) {
                mat_std <<- input       # to set the input matrix & init mat_inv
                mat_inv <<- NULL
        }
        get <- function() mat_std       # to return the original matrix
        setinv <- function(inv) mat_inv <<- inv # to set the mat_inv
        getinv <- function() mat_inv    # to return the mat_inv
        list(set = set, get = get,      # names of functions as a list
             setinv = setinv,           # functions can be accessed by objectName$FUN (defined in next function)
             getinv = getinv)

}


# cacheSolve is a function which checks whether the inverse of the matrix
# was already calculated and cached, if not, it solves for the inverse and
# then stores/caches the result , then returns it.
cacheSolve <- function(mat_std, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- mat_std$getinv()
        if(!is.null(mat_inv)) {   # checking if inverse was already cached
                message("getting cached inverse of matrix")
                return(mat_inv)
                # return inverse directly without extra calculations since it is already cached .
        }
        data <- mat_std$get()       # these lines are executed when not cached/first time
        mat_inv <- solve(data)     # getting the input matrix and solving for inverse
        mat_std$setinv(mat_inv)
        mat_inv
}
