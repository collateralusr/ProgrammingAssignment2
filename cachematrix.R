# this here function makes the thing out of the other thing

makeCacheMatrix <- function(A = matrix()) {

  A_inv <- NULL
  
  set <- function(B) {
    A <<- B
    A_inv <<- NULL
  }
  get <- function() A
  set_inv <- function(inv) A_inv <<- inv
  get_inv <- function() A_inv
  
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
  
}


# this function right here does the other part.

cacheSolve <- function(thing, ...) {

  A_inv <- thing$get_inv()
  
  if (!is.null(A_inv)) {
    message("gettin', boss")
    return(A_inv)
  }
  
  A <- thing$get()
  A_inv <- solve(A)
  thing$set_inv(A_inv)
  A_inv
  
}


# what the above code does:

# > source("cachematrix.R")
# > M <- matrix(1:4, 2, 2)
# > M
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > what <- makeCacheMatrix(M)
# > what$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > what$get_inv()
# NULL
# > cacheSolve(what)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > what$get_inv()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > solve(M)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
