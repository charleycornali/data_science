makeCacheMatrix <- function(x = matrix()) {
    #   This function creates a special "matrix" object that can cache its inverse
    #   computes the inverse of a square matrix
    #   
    inv_matrix <- NULL
    
    #  set the value of the matrix
    set <- function(y){
        x <<- y
        inv_matrix <<- NULL
    }
    
    #   get the value of the matrix
    get <- function() x
    
    #   set the value of inverse of the matrix
    set_inv_matrix <- function(inverse) inv_matrix <<- inverse
    
    #   get the value of inverse of the matrix
    get_inv_matrix <- function() inv_matrix
    
    #   returns a list containing functions
    list(set = set, 
         get = get,
         set_inv_matrix = set_inv_matrix,
         get_inv_matrix = get_inv_matrix)

}


cacheSolve <- function(x, ...) {
    #   This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
    #   If the inverse has already been calculated (and the matrix has not changed), 
    #   then the cachesolve should retrieve the inverse from the cache
    #   This function assumes that the matrix is always invertible.
    
    inv_matrix <- x$get_inv_matrix()
    
    if(!is.null(inv_matrix)) {
        message("getting cached data.")
        return(inv_matrix)
    }
    
    inv_matrix <- (solve(x$get()))
    x$set_inv_matrix()
    
    #   Return a matrix that is the inverse of 'x' 
    inv_matrix
}

#   Example output....
#   > source("cachematrix.R")
#   > my_mat <- rbind(c(1, 2), c(4, 6))
#   > m <- makeCacheMatrix(my_mat)
#   > m$get()
#   [,1] [,2]
#   [1,]    1    2
#   [2,]    4    6
#   > cacheSolve(m)
#   [,1] [,2]
#   [1,]   -3  1.0
#   [2,]    2 -0.5
#   > cacheSolve(m)
#   getting cached data.
#   [,1] [,2]
#   [1,]   -3  1.0
#   [2,]    2 -0.5
#   > my_mat <- rbind(c(1, -1/4), c(-1/4, 1))
#   > m <- makeCacheMatrix(my_mat)
#   > m$get()
#   [,1]  [,2]
#   [1,]  1.00 -0.25
#   [2,] -0.25  1.00
#   > cacheSolve(m)
#   [,1]      [,2]
#   [1,] 1.0666667 0.2666667
#   [2,] 0.2666667 1.0666667
#   > cacheSolve(m)
#   getting cached data.
#   [,1]      [,2]
#   [1,] 1.0666667 0.2666667
#   [2,] 0.2666667 1.0666667
#   > 
