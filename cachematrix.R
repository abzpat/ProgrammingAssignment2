## Write a short comment describing this function
## Matrix inversion is usually a costly computation. So as to reduce this overhead of 
## recomputation, it makes sense to cache. Below two functions are made to cache the 
## matrix and then find the inverse of matrix.

## Function     : makeCacheMatrix()
## Input args   : x (type : Matrix)
## Output args  : A list containing  output of function in following order - 
##                set (function), get (function), setSolve(function) ,getSolve (function)
## Description  : This function will take the arguments 
##                as a matrix and will cache that matrix in specific enviornment
makeCacheMatrix <- function(x = matrix()) {  
  # Initializing cachedInvMat to Null while making
  # the cached matrix for the first time
  cachedInvMat <- NULL
  
  # To Set the enviornmental variables x and cachedInvMat
  set <- function(locMat){
    cachedInvMat <<- NULL
    x <<- locMat
  }
  
  # To get the matrix x
  get <- function() {x}
  
  # To push the solvedMat to the enviornmental variable cachedInvMat
  setSolve <- function(solvedMat){cachedInvMat <<- solvedMat}
  
  # To retrieve the cachedInvMat back
  getSolve <- function(){cachedInvMat}
  
  # To create a list which will help to obtain the cached Matrix back
  l<-list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
  # A warning message is displayed for non-square matrix
  if(NCOL(x) != NROW(x)){
    warning("Special matrix is created. Inverse of this matrix is not possible as it is a non-square matrix")
  }
  #Print special matrix
  l
}

## Function     : cacheSolve()
## Input args   : x (type : Matrix, it is a list for cached matrix), 
##                ...(dot dot dot argument)
## Output args  : Inverse matrix 
## Description  : This function will accept a cached matrix prepared by 
##                makeCacheMatrix and returns the inverse of x
cacheSolve <- function(x, ...) {
  
  retuningMatrix = NULL
  # Retrieving the cached matrix 
  cachedInvMat <- x$getSolve()
  
  # Checking if the value of matrix is null or not. 
  # if yes then proceed with computing of inverse.
  if(!(is.null(cachedInvMat))) {
    message("getting cached data")
    retuningMatrix <- cachedInvMat
  }
  else{
    # check if the matrix is square
    if(NROW(x$get()) == NCOL(x$get())) {
      # if the matrix is square matrix then check if the 
      # value of determinant non-zero
      if(det(x$get()) != 0){
        # Get the matrix from the special matrix 
        data <- x$get()
        # Compute inverse of the matrix
        solvedMat <- solve(data, ...)
        # Store the computed matrix in the catched matrix
        x$setSolve(solvedMat)
        # Display the matrix
        retuningMatrix <- solvedMat
      }
      else{
        stop("The matrix is not singular. Hence, inverse cannot be taken.")
      }
    }
    else {
      stop("Inverse of non-square matrix does not exist.")
    } 
  }
  return(retuningMatrix) 
}#end of function
