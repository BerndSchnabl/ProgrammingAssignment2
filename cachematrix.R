
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
  ## matix getter and setter function pair
  ## these 2 functions are used to access the Matrix stored in the global
  ## environment. When the matrix is set with a new value, the inverse
  ## version is deleted since it would not match the set one.
  ## the 'cacheSolve' function is testing this variable for NULL to determine
  ## if a cached version of the inversed variable is already stored or if the 
  ## time consuming task has to be performned
  ##
  ## ####################################################################################
  set <- function(p_Matrix) {
    g_Matrix <<- p_Matrix ## store the content in the global environment 
    g_Inverse <<- NULL ## reset global inverse martix since it is not yet computed
  }
  
 get <- function() {
    g_Matrix ## this is the vector from the global environment
  }
  
  ## ####################################################################################
  ##
  ## the inverse getter/setter pair functions provide access to the cached version
  ## of the matrix by the cacheSolve function. these 2 functions are passed in the 
  ## list object 'return_list' to the caller of the function 'makeCacheMatrix'.
  ##
  ## ####################################################################################
  setInverse <- function(p_Inverse) {
    g_Inverse <<- p_Inverse
  }
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

## ####################################################################################
##
## cacheSolve:   Return a matrix that is the inverse of 'mkcm'
## ===========
## The cacheSolve functions make use of the 4 getter/setter funcitons in the
## makeCacheMatrix function previously defined. the cacheFunction itself
## is passed an instance of the makeCacheMatrix return value.
## The makeCacheMatrix returns a list which in turn is passed to the 
## cache solve function to access the stored version of the inverse matrix
## Tha parameter which is passed to the cacheSolve function is the return value 
## of the makeCacheMatrix function and hence it is called mkcm 
## this parameter is a list of the 4 access functions from the makeCacheMatrix function.
## these functions allow the 'cacheSolve' function set and get access to the matrix
## and the inverse version of the matrix. 
##
## ####################################################################################
cacheSolve <- function(mkcm) {
  
  inv_mat <- mkcm$getInverse()      ## use makeCacheMatrix functions to access the inverse matrix 
  if(!is.null(inv_mat)) {           ## yes there is a cached version of the inverse matrix
    message("getting cached data") 
    return(inv_mat)                 ## pass the cached inverse matrix to the caller
  }
  mat  <- mkcm$get()                ## get the original matrix
  message("calculate inverse matrix")
  inv_mat <- solve(mat)             ## no additional parameters are passed to solve
  mkcm$setInverse(mat)              ## cache the inverse matrix
  inv_mat                           ## return the inverse matrix
}

## ####################################################################################
## 
## testfunction
## test cases are implemented in a fuction , so that the test funcation can
## be passed to the debug methid: debug(testMatrix)
## 
## ####################################################################################
testMatrix <- function() 
{
  ##debug(cachInverse)
  ##debug(makeCacheMatrix)
  
  mat1 <- matrix( rnorm(900), 30, 30 )                 ## can be inversed since |mat1|<>0 & nrow=ncol
  mcm1 <- makeCacheMatrix()                            ## there is no need to pass a mat1 as parameter
  mkm1$set( mat1 )                                     ## store the matrix

  mat2 <- mcm1$get()                                   ## get it back
  if (mat1 <> mat2) message("error: matrix changed")   ##  this should return the vector again which was passed 
  
  imat1 <- cacheInverse(mcm1)                          ## get the inverse matrix (this time calculated)
  imat2 <- cacheInverse(mcm1)                          ## return again this time from cache
  
}

## ####################################################################################
##
## main
##
## ####################################################################################
debug(testMatrix)
testMatrix()



