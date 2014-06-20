## ####################################################################################
##
## makeCacheMatrix:
## ================
## it has 2 pairs of getter / setter functions which are returned to the caller
## the get/set function pair for the original matrix (set and get)
## and the get/set function pair for the inverse matrix (setInverse/getInverse)
## the function makeCacheMatrix returns a list of these 2 pairs of getter / setter
## functions to the 
## 
## ####################################################################################
makeCacheMatrix <- function(p_Matrix = matrix()) {
  
  m_invereMatrix <- NULL # local member variable hence no <<- operator
  
  ## ####################################################################################
  ##
  ##
  ##
  ## ####################################################################################
  set <- function(p_Matrix) {
    g_Matrix <<- p_Matrix ## store the content in the global environment 
    g_Inverse <<- NULL ## reset global inverse martix since it is not yet computed
  }
  
  ## ####################################################################################
  ##
  ##
  ##
  ## ####################################################################################
  get <- function() {
    g_Matrix ## this is the vector from the global environment
  }
  
  ## ####################################################################################
  ##
  ##
  ##
  ## ####################################################################################
  setInverse <- function(p_Inverse) {
    g_Inverse <<- p_Inverse
  }
  
  ## ####################################################################################
  ##
  ##
  ##
  ## ####################################################################################
  getInverse <- function() {
    return(g_inverse)
  }
  
  ## ####################################################################################
  ##
  ## create the list of getter/setter function pairs.
  ## the name of the element in the list (left side of assignment operation)
  ## is the same as the setter / getter funciton name of the makeCacheMatrix
  ## function (right side of assignment operation)
  ##
  ## ####################################################################################
  return_list <- list(set = set, get = get,  
                      setInverse = setInverse, 
                      getInverse = getInverse)
  
  invisible(return_list) ## return the getter/setter functions to caller
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

m <- matrix( c(3,4,4,8), 2, 2)

makeCacheMatrix(m)
m1 <- cacheSolve