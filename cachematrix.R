##These functions cache the inverse of a matrix instead of repeated computation.
##This saves a lot of computation time when huge matrices are involved.

##  The function below returns a list of 4 get and set functions
##  that create a special "matrix" object and cache its inverse.

makeCacheMatrix <- function(x=matrix()){
  
  ##Assign empty matrix
  Inv <- matrix()
 
  ##Save the matrix
  ## <<- operator makes the variables assessible outside setMatrix() function
  setMatrix <- function(y){
    
    x <<-y
    Inv <<- matrix()
  }
  
  ##Retrive the matrix
  getMatrix <- function(){
    x
  }
  
  ##Save Inverse of the matrix
  setInverse <- function(inverse){
    Inv<<-inverse
  }
  
  ##Retrive Inverse of the matrix
  getInverse <- function(){
    Inv
  }
  
  ##Return the list of function names
  list(setMatrix=setMatrix,
       getMatrix=getMatrix,
       setInverse=setInverse,
       getInverse=getInverse)
}


##  The function below computes inverse of the matrix returned by 
##  makeCacheMatrix function above. If inverse already exists for the same matrix,
##  then cached inverse is displayed with message "Caching Inverse".

cacheSolve <- function(x, ...){

  ##Get the saved inverse for matrix x
  ##If inverse is not computed yet then empty matrix is returned
  Inv <- x$getInverse() 
  
  ##Display cached Inverse if exists and return
  if(sum(complete.cases(Inv))!=0){
    print("Caching Inverse")
    return(Inv)
  }
  

  ##Else Compute and save Inverse
  Inv <- solve(x$getMatrix(),...)  
  
  x$setInverse(Inv) 
  
  Inv
  
}
