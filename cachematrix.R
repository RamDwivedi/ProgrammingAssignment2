## Function takes a matrix as Input parameter and returns inverse of it

## makeCacheMatrix takes a matrix as input param and uses <<- operator to cache the values. 
## Returns a list with various operators on the matrix

makeCacheMatrix <- function(x = matrix()) {
        x <- NULL
        #renaming variable as invMatrix to increase readability
        invMatrix <- x
        
        set <- function(setMatrix) {
                inMatrix <<- setMatrix
                invMatrix <<- NULL
        }
        
        get <- function() inMatrix
        
        setInverse <- function(invertedMatrix) invMatrix <<- invertedMatrix
        
        getInverse <- function() invMatrix
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Takes a matrix as input and checks if cached. If yes, then return data from cache instead of recomputing
## Function also checks if the input matrix is inversible. If not, do a graceful exit

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # assign input x to a more appropriate variable
        inMatrix <- x
        invMatrix <- inMatrix$getInverse()
        
        # Check if inverse exists
        if(!is.null(invMatrix)) {
                message("Matrix exists. Returning cached data")
                return(invMatrix)
        }
        
        data <- inMatrix$get()
        
        #check if matrix is inversible, do a graceful exit otherwise compute the inverse
        if(det(data) == 0){
                message("Non invertible matrix, change elements and try again")
        } else {
                invMatrix <- solve(data)
                inMatrix$setInverse(invMatrix)
                invMatrix
        }
}
