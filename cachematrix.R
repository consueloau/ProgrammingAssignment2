## Put comments here that give an overall description of what your
## functions do
##
## Write a short comment describing this function
## creates an object matrix that saves this natrix inverse for later use. It is used to save resources when running lengthy programs
## where inverse of a matrix is requested several times.
makeCacheMatrix <- function(x = matrix()) {
 mix <- NULL
  set <- function(y) {
    x <<- y
    mix <<- NULL
  }
  get <- function() x
  setmix <- function(solve) mix <<- solve
  getmix <- function() mix
  list(set = set, get = get,
       setmix = setmix,
       getmix = getmix)
}
##
##
## Write a short comment describing this function
## This function check whether there exist a cache of a nonsingular square matrix inverse, it so output the save inverse
## if matrix have been changed or a it is a new matrix, calculates the inverse and save it
## in the makeCacheMatrix object for later use
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          ## Return a matrix that is the inverse of 'x'
  mix <- x$getmix()
  if(!is.null(mix)) {
    message("getting cached data")
    return(mix)
  }

  
  data <- x$get()
  mix <- solve(data)
  x$setmix(mix)
  mix
}
##
## do not run
##matx<-(t(matrix(1:16, 4, 4)[c(1,3), c(1,3)]) %*% matrix(1:16, 4, 4)[c(1,3), c(1,3)])
## (cacheSolve(makeCacheMatrix(matx)))
