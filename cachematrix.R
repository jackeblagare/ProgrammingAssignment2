## This file contains a set of functions for computing the inverse of a matrix. Furthermore, 
## it also contains a function to check whether the inverse has already been previously calculated.
## If the inverse has already been previously calculated, the cached result is returned.
##
## Jack Elendil B. Lagare

## This function initializes a new matrix object.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL; # This is the variable storing the calculated inverse of the matrix provided.
             # Resets to NULL whenever a new matrix is initialized.
  
  # Saves the input matrix and saves it. Resets the value of m into NULL. 
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  # Returns the value of the current input matrix
  get <- function(){
    x
  }
  
  # Saves the inverse of the input matrix
  setinverse <- function(inverse){
    m <<- inverse
  }
  
  # Retrieves the inverse of the input matrix
  getinverse <- function(){
    m
  }
  
  # Lists all the available operations you can perform on the created object
  list(get = get,setinverse = setinverse, getinverse = getinverse)
}

## This function checks whether a cached result of the inverse already exists. If not, calculate inverse.
cacheSolve <- function(x, ...) {
  
    m <- x$getinverse() # Retrieves the inverse
    
    # If result already exists, just return result
    if(!is.null(m)){
      message("getting cached data")
      return (m);
    }
    
    data <- x$get()  # Get the input matrix
    m <- solve(data) # Calculate the inverse
    x$setinverse(m)  # Store the result
    m                # Return the result
}
