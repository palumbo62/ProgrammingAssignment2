##############################################################################
## Robert Palumbo
## Coursera - Data Scientist Track
## R Programming
## Assignment 2 - Lexical Scoping
## July 24/15
##
## Solution Notes
## 
##    This program snippet provides solutions to the required R functions
##        makeCacheMatrix()  
##        cacheSolve()
##
##    These functions work together to create and trivially manipulate a 
##    'matrix' object and its computed inverse. It is a performance 
##    optimization which utilizes a 'cache' to store a previously computed
##    matrix inverse.  For large matrices, calculation of an inverse can
##    be quite time consuming.  If a matrix has already had its inverse
##    calculated it is better to store the inverse result in a 'cache' or
##    alternate scoping environment -- achieved using the '<<-' operator.
##    This environment provides a mechanism that allows objects to have values
##    that can persist outside the scope of the local function environment.
##
##    In this context, for a given matrix object, if the inverse has already
##    been calculated and stored in the cache, the cached value can be used
##    thereby avoiding the overhead of having to recalculate it.  If no 
##    inverse exist for the object in the cache, then it will be formally
##    calculated, placed in the cache, and returned as the first result.
##    
##    The 'cache' concept is widely used in software development to improve
##    processing performance, but is also used in hardware development, for
##    example L1/L2 caches to quickly move heavily used data to main memory.
##
##
##    The general purpose of the assignment is more to understand 'scoping'
##    rules in the R language than it is about matrix inversion per se.
##
##    Sample Use:
##
##    > m <- matrix(c(1,3,5,7,9, 2, 4, 6, 8), 3, 3)
##    > m
##            [,1] [,2] [,3]
##      [1,]    1    7    4
##      [2,]    3    9    6
##      [3,]    5    2    8
##    >
##    > cm <- makeCacheMatrix(m)
##    > cm
##    $set
##    function (y) 
##    {
##        x <<- y
##        mat <<- NULL
##    }
##    <environment: 0x00000000190edb38>
##  
##    $get
##    function () 
##    x
##    <environment: 0x00000000190edb38>
##  
##    $setinverse
##    function (inv) 
##    mat <<- inv
##    <environment: 0x00000000190edb38>
##    
##    $getinverse
##    function () 
##    mat
##    <environment: 0x00000000190edb38>
##    
##    > cacheSolve(cm)
##        [,1]       [,2]       [,3]
##    [1,] -1.1111111  0.8888889 -0.1111111
##    [2,] -0.1111111  0.2222222 -0.1111111
##    [3,]  0.7222222 -0.6111111  0.2222222
##
##    > cacheSolve(cm)
##    getting cached inverse data for matrix
##        [,1]       [,2]       [,3]
##    [1,] -1.1111111  0.8888889 -0.1111111
##    [2,] -0.1111111  0.2222222 -0.1111111
##    [3,]  0.7222222 -0.6111111  0.2222222


##############################################################################
##  makeCacheMatrix() is used to create a 'matrix' object that contains a
##  list of functions to get/set the inverse of a specified 'cached' inverse
##  of the matrix.  
##
##  It takes one parameter which is actually defaulted to an empty matrix if
##  no formal matrix is specified as an argument but that is of little value
##  for this assignment as it were.  Per the assignment, it is assumed that
##  all matrices to be passed to this function are invertible.  Given that, 
##  the function creates several internal functions that are bound to the 
##  matrix object passed as the parameter.
##
##  get() is used to retrieve the 'matrix' object.
##
##  set() is used to update a given object with a new matrix to evalute.
##
##  setinverse() is a workhorse function. It is responible for taking the 
##  current calculated inverse and setting it in the 'cache' environment
##
##  getinverse() is used to gain access to the currently scoped matrix 
##  object.
##############################################################################
makeCacheMatrix <- function(x = matrix()) 
{
    mat <- NULL

    set <- function(y) {
      #print("resetting matrix cache")
      x <<- y
      mat <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inv) mat <<- inv
    getinverse <- function() mat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##############################################################################
##  cacheSolve() is used to compute the inverse of the specified 'matrix' 
##  object that is passed as the formal argument.  This object must have
##  been created using 'makeCacheMatrix()'.  
##
##  The general idea is that cacheSolve() will first look in the 'cache'
##  environment to see if the inverse has already been calculated and
##  if so will just return that value.  Otherwise, it will go ahead and
##  manually calculate it, place it in the cache for the next time, and
##  return the newly calculate value.
##############################################################################
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getinverse()
    
    if (!is.null(mat)) {
      message("getting cached inverse data for matrix")
      return(mat)
    }
  
    data <- x$get()
    mat <- solve(data, ...)
    x$setinverse(mat)
    mat
}
