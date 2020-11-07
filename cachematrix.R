
##Create an objective store the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    I_M  <- NULL
    set <- function(matrix) {
        x <<- matrix
        I_M <<- NULL
    }
    get <- function() x
    set_IM <- function(IM) I_M <<- IM
    get_IM <- function() I_M
    list(set = set, get = get,
         set_IM = set_IM,
         get_IM = get_IM)
}


##Check whether the inverse has been calculated, if not calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I_M <- x$get_IM()
        if(!is.null(I_M)) {
                message("getting inversed matix")
                retrun(I_M)
        }
        matrix <- x$get()
        I_M <- solve(matrix)
        x$set_IM(I_M)
        I_M
}
