## is the same as the example

## Crea la matriz "especial", con los objetos Set, get, setinverse y getinverse,
## primero inicializa m en null, 
## set permite asignar una nueva matriz que queda guardada en get
## y cuando se asigna una nuvea matriz en ese caso inicializa m a null
## setinverse salva la inversa y se almacena en m, cuyo valor toma getinverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse1) m <<- inverse1
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## toma la matriz "especial" y valida si tiene la matriz inversa guardada y la 
## regresa en caso de que sea nullo es decir que no tenga nada guardado 
## calcula la inversa y una vez calculada la guarda en la matriz "especial" 
## con la función setinverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
