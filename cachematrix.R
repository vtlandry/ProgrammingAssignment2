# This file contains two functions: makeCacheMatrix and CacheSolve for a group project.
# Some recommended test cases as well as sourcing commands will be listed at the bottom of
# this file.

## `makeCacheMatrix`: This function creates a special "matrix" object of 4 functions to be used:
##  set, get, setinverse and getinverse. These can be used to cache the inverse of an invertible matrix.

makeCacheMatrix <- function(x = matrix()) { #begin makeCacheMatrix. Creates cache matrix that can cache its inverse.


    inv <- NULL #initialize local copy
    set <- function(y) { #begin set function
      cat("Running function set", "\n")
      x <<- y #store y passed by set caller in global x
      inv <<- NULL #initialize global copy
    } #end set function
    get <- function() { #begin get
      cat("Running function get", "\n")
      x #PRINT X WHICH IS THE MATRIX TO BE INVERTED, updated global x
    } #end get
    setinverse <- function(inverse) { #begin setinverse
      inv <<- inverse #Store function parameter from caller in global inv
                      #invertible matrix to be inverted in inv global
      cat("Running function setinverse", "\n")
    } #end setinverse
    getinverse <- function() { #begin getinverse
      cat("Running function getinverse", "\n")
      inv #PRINT square matrix to be inverted 
          #Global inv is data to be used by caller of getinverse
    } #end getinverse
    list(set = set, get = get, #create matrix object with 4 functions used to cache inverse of invertible matrix
         setinverse = setinverse,
         getinverse = getinverse)
 

} #end makeCacheMatrix

## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) { #begin cacheSolve Which uses the solve function to invert a matrix
        ## Return a matrix that is the inverse of 'x'


    inv <- x$getinverse() #This will be NULL until cached. Find out if cached in next statement.
    if(!is.null(inv)) { #begin already cached so skip 3 function calls below, message and return(inv) 
      message("getting cached data") #use of message function for informational purposes in debug
      return(inv) #skip code below and return inv to caller
    } #end already cached so skip computations
    matrix_to_invert <- x$get() #Get the matrix to invert
    inv <- solve(matrix_to_invert, ...) #Call the solve function with matrix to invert
    x$setinverse(inv) #call setinverse
    inv #PRINT inv updated global variable inv
   
  
} #end cacheSolve

#INFO ON HOW TO RUN TEST CASES BELOW:

#source("cachematrix.R")
#4 INVERTIBLE TEST CASES TO RUN
#TEST CASE 1
#>mm <- makeCacheMatrix(matrix(1:4, 2, 2))
#>mm$get()
#Running function get 
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#>mm$getinverse()
#Running function getinverse
#NULL
#>cacheSolve(mm)
#Running function getinverse 
#Running function get 
#Running function setinverse 
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5


#> cacheSolve(mm)
#Running function getinverse 
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5 


#  TEST CASE 2
#>mm <- makeCacheMatrix(matrix(, 2, 2))

#>mm$set(matrix(c(2, 2, 1, 4), 2, 2))
#Running function set 
#>mm$get()
#Running function get 
#     [,1] [,2]
#[1,]    2    1
#[2,]    2    4


#>mm$getinverse()
#Running function getinverse 
#NULL

#>cacheSolve(mm)
#Running function getinverse 
#Running function get 
#Running function setinverse 
#          [,1]       [,2]
#[1,]  0.6666667 -0.1666667
#[2,] -0.3333333  0.3333333

#>cacheSolve(mm)
#>Running function getinverse 
#getting cached data
#           [,1]       [,2]
#[1,]  0.6666667 -0.1666667
#[2,] -0.3333333  0.3333333


#TEST CASE 3
#>mm <- makeCacheMatrix(matrix(, 2, 2))
#>mm$set(matrix(c(2, 2, 3, -2), 2, 2))
#Running function set 
#>mm$get()
#Running function get 
#     [,1] [,2]
#[1,]    2    3
#[2,]    2   -2

#>mm$getinverse()
#Running function getinverse
#NULL

#>cacheSolve(mm)
#Running function getinverse 
#Running function get 
#Running function setinverse 
#     [,1] [,2]
#[1,]  0.2  0.3
#[2,]  0.2 -0.2

#>cacheSolve(mm)
#Running function getinverse 
#getting cached data
#     [,1] [,2]
#[1,]  0.2  0.3
#[2,]  0.2 -0.2
#TEST CASE 4
#>mm <- makeCacheMatrix(matrix(, 2, 2))
#> mm$set(matrix(c(.2, .2, .3, -.2), 2, 2))
#Running function set
#>mm$get()
#Running function get 
#     [,1] [,2]
#[1,]  0.2  0.3
#[2,]  0.2 -0.2

#>mm$getinverse()
#>Running function getinverse
#NULL

#>cacheSolve(mm)
#Running function getinverse 
#Running function get 
#Running function setinverse 


#     [,1] [,2]
#[1,]    2    3
#[2,]    2   -2


#>cacheSolve(mm)
#Running function getinverse 
#getting cached data
#     [,1] [,2]
#[1,]    2    3
#[2,]    2   -2















