# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has
# not changed), then cacheSolve should retrieve the inverse from the cache.
# solve(X) returns its inverse.


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL  # setting inverse matrix value to null incase a new matrix is created within the program
  
  set<-function(y){
    x<<-y  #setting values of new matrix and storing in cache
    m<<-NULL #setting value of inverse matrix to null in m (cache)
  }
  
  get<-function() x # returns matrix x
  
  setmatrix<-function(matrix) m <<- matrix #takes values of the inverted matrix and stores in m(cache)
  
  getmatrix<-function() m # returns matrix m
  
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) { #function takes matrix
  m <- x$getmatrix() # reads matrix x values in m
  
  if(!is.null(m)){ # check if matrix values are available return m which is the cached matrix
    message("getting cached data")
    return(m)
  }
  
  matrix<-x$get() #if conditon is false (matrix is null) read matrix
  
  m <- solve(matrix, ...) #then use solvce function to get inverse matrix
  
  x$setmatrix(m) #then use setmatrix to store new matrix
  m # return new inverse matrix
}

test_matrix <- matrix(1:4,2,2)
new_matrix <- makeCacheMatrix(test_matrix)
cache_matrix <- cacheSolve(new_matrix)
cache_matrix