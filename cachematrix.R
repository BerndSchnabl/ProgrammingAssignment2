## ####################################################################################
##
## makeCacheMatrix:
## has 2 pairs of getter / setter functions which are returned to the caller
## the get/set function pair for the original matrix and 
## the 
## 
## ####################################################################################
makeCacheMatrix <- function(p_Matrix = matrix()) {
  
  m_invereMatrix <- NULL # local member variable
  
  set <- function(p_Matrix) {
    g_Matrix <<- p_Matrix ## store the content in the global environment 
    g_Inverse <<- NULL ## reset global inverse martix since it is not yet computed
  }
  
  get <- function() {
    x ## this is the vector from the global environment and not the parameter x from the
  }
  
  setInverse <- function(mean) {
    m <<- mean
  }
  
  getInverse <- function() {
    return(m)
  }
  
  return_list <- list(set = set, get = get,  
                      setInverse = setInverse, 
                      getInverse = getInverse)
  
  invisible(return_list)
  
  list(set = set, get = get, setmean = setmean, getmean = getmean)
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