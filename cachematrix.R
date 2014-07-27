## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## initialize as given in example
    inverse <- NULL;
    set<- function(y){
        y <<- x;
        inverse <<- NULL;
    }
    get <- function() { x; }
    get_matrix <- function() { x; }
    set_inverse <- function(inverse_) { inverse <<- inverse; }
    get_inverse <- function() { inverse; }
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse);
}


## This function first checks if the inverse exists in chache.
## If yes, it returns that else it computes the matrix inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## x is the list object
    ## This checks if the inverse already existed
    mat_i <- x$get_inverse();
    if(!is.null(mat_i)){
        message("getting cached inverse");
        return(mat_i);
    }
    ## if the inverse didn't exist
    mat <- x$get();
    mat_i <- solve(mat) ;
    x$set_inverse(mat_i);
    mat_i;
}
