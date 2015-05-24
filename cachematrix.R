## The first function, makeCacheMatrix, creates a list that contains the four functions:
## 1. set, which stores the matrix 
## 2. get, which returns the matrix
## 3. setInverse, which stores the inverse of the matrix
## 4. getInverse, which returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {           
      x <<- y                   ## stores matrix as x, removes previous value of x if function had been called before
      inverse <<- NULL          ## Changes value of inverse variable to NULL in case inverse from previous matrix was still stored
  }
  
  get <- function() {           ## returns matrix
    x
  }
  
  setInverse <- function (x){
    inverse <<- solve(x)        ## stores inverse of matrix
  }
  
  getInverse <- function (){
    inverse                     ## returns inverse of matrix if function is called
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
  
}



## The cacheSolve function calculates the inverse of the matrix using the functions
## from the makeCacheMatrix function above. 
## It first checks to see if the inverse has been caculated using the getInverse function.
## If it has been calculated, it returns the inverse from the cache.
## If not, then it calculates the inverse and stores the data in the cache using setInverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<-x$getInverse()         ## Functions checks and returns inverse of matrix
  
  if(!is.null(inverse)) {         ## Returns inverse if inverse was previously calculated and stored
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()                 ## If no inverse was previously calculated, we get the matrix that we're currently working with
  inverse <- solve(data,...)      ## Solve for the inverse and store the inverse
  x$setInverse(inverse)
  inverse
} 
