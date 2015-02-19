##  Matrix inverse can be a time consuming operation especially for huge matrices. 
##  The functions below provide an example of how to cache the inverse of a matrix
##  instead of repeated computation, thus improving the code performance.


##  The function below returns a list of 4 get and set functions
##  that create a special "matrix" object and cache its inverse.

makeCacheMatrix <- function(x=matrix()){
  
  ##  Assign empty matrix
  Inv <- matrix()
 
  ##  Save the matrix
  ##  <<- operator makes the variables assessible outside setMatrix() function
  setMatrix <- function(y){
    
    x <<-y
    Inv <<- matrix()
  }
  
  ##  Retrive the matrix
  getMatrix <- function(){
    x
  }
  
  ##  Save Inverse of the matrix
  setInverse <- function(inverse){
    Inv <<- inverse
  }
  
  ##  Retrive Inverse of the matrix
  getInverse <- function(){
    Inv
  }
  
  ##  Returns the list of function names
  list(setMatrix=setMatrix,
       getMatrix=getMatrix,
       setInverse=setInverse,
       getInverse=getInverse)
}


##  The function below computes inverse of the matrix returned by makeCacheMatrix function above. 
##  If inverse already exists for the matrix, then display the cached inverse.
##  Else compute inverse and save the results.

cacheSolve <- function(x, ...){

  ##  Get the saved inverse for matrix x
  ##  If inverse is not computed yet then empty matrix is returned
  Inv <- x$getInverse() 
  
  ##  Display cached Inverse if exists and return
  if(sum(complete.cases(Inv))!=0){
    print("Caching Inverse")
    return(Inv)
  }
  

  ##  Compute and save Inverse
  Inv <- solve(x$getMatrix(),...)  
  
  x$setInverse(Inv) 
  
  Inv
  
}
