## Last modified: 21/03/2015
## Author: Monica Guerra
## Rutina para crear una matriz, que primero valida si la matriz inversa, ya fue invertida antes, y esta en cache

## Function call: cacheSolve(makeCacheMatrix(h8))

## Devuelve un vector donde cada elemento es una funcion
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve1) m <<- solve1
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## Esta función invierte una matriz, de un vector creado con la funcion "makeCacheMatrix"
## primero chequea si el calculo se hizo y luego si las matrices son las mismas
## Si ya fue calculada la matriz inversa y es la misma, entonces no gasta recursos de computo
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) 
    if(x$setsolve() == x$getsolve()) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

