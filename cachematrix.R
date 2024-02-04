## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

#' Make Cache Matrix
#'
#' @name makeCacheMatrix
#' @usage makeCacheMatrix(x)
#' @description This function creates a special \code{\link{matrix}} object that
#'   can cache its inverse.
#' @param x A matrix (optional)
#'
#' @return The list functions to \code{set} and \code{get} the cached
#'   \code{\link{matrix}} and the its inverse (\code{setInv} and \code{getInv}).
#' @export makeCacheMatrix
#'
#' @examples
#' makeCacheMatrix()
#' makeCacheMatrix(matrix(rnorm(9), 3, 3))
makeCacheMatrix <- function(x = matrix()) {
    x.inv <- NULL
    # Set function
    set <- function(y) {
        x <<- y
        x.inv <<- NULL
    }

    # Get function
    get <- function()
        x

    # SetInv function
    setInv <- function(y.inv) {
        x.inv <<- y.inv
    }

    # GetInv function
    getInv <- function()
        x.inv

    # Return list of set and get functions
    list(
        set = set,
        get = get,
        setInv = setInv,
        getInv = getInv
    )
}

#' Cache Solve
#'
#' @name cacheSolve
#' @usage cacheSolve(x, ...)
#' @description This function computes the inverse of the special
#'   \code{\link{matrix}} returned by \code{\link{makeCacheMatrix}}. If the
#'   inverse has already been calculated (and the matrix has not changed), then
#'   the \code{cacheSolve} retrieves the inverse from the cache.
#' @param x A matrix
#' @param ... additional arguments to be passed to \code{\link{solve}}
#'
#' @return Returns a matrix that is the inverse of \code{x}
#' @export cacheSolve
#'
#' @examples
#' x <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
#' cacheSolve(x) #Not cached
#' cacheSolve(x) #Cached
cacheSolve <- function(x, ...) {
    x.inv <- x$getInv()
    if (is.null(x.inv)) {
        x.inv <- solve(x$get(), ...)
        x$setInv(x.inv)
    }
    else
        message("Getting cached data")
    x.inv
}
