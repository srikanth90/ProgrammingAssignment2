## Created two functions makeCacheMatrix and cacheSolve. 
## The first function is to pass a invertable matrix as an argument and also 
## store its inverse (once calculated) in the memory. 
## The second function is to calcuate the inverse or fetch it from the memory
##  and display the result.

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
## Below function resets the value for a new matrix
## As this function is called in a different environment from its parent function
## the operator '<<-' assigns a value to variable  in an environment that is 
## different from the current environment.
  set_matrix <- function(new_matrix){
    matrix_inv <<- NULL
    x <<- new_matrix
  }
  get_matrix <- function() x
## As this function is called in a different environment from its parent function
## the operator '<<-' assigns a value to variable  in an environment that is 
## different from the current environment.
  set_inv <- function(inv) matrix_inv <<- inv
  get_inv <- function() matrix_inv
  list(set_matrix = set_matrix, get_matrix = get_matrix, set_inv = set_inv,
       get_inv = get_inv)
}


cacheSolve <- function(x, ...) {
## returns the stored inverse if the matrix has not been changed  
  if(!is.null(x$get_inv())){
    print("Getting cached data")
    matrix_inv <- x$get_inv()
    return(matrix_inv)
  }
## calculate the inverse if a new matrix passed as an argument or for the first 
## iteration of the program.
  matrix <- x$get_matrix()
  matrix_inv <- solve(matrix)
## '<<-' operator helps assign value calculated within cacheSolve() function
## to a variable within makeCacheMatrix() function.
  x$set_inv(matrix_inv)
  matrix_inv
}
