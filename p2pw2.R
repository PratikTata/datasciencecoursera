cpymatrix <- function(inputMatrix = matrix()){
 inverted <- NULL
 setMatrix <- function(innerinput){
   x <<- y
   inverted <<- NULL
 }
 get <- function(){inputMatrix}
 assignInverse <- function(inverse){inverted <<- inverse}
 makeInverse <- function(){inv}
 list(setMatrix = setMatrix,  get = get, assignInverse = assignInverse, makeInverse = makeInverse)
}

solve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}