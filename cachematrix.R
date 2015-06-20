
## makeCacheMatrix() is a function which calculates and put in cash the 
## value of the reverse of the input matrix
## input: a matrix
## output: a list of 4 objects "functions"
## setmatrix set the value the value of the matrix
## getmatrix get the value of the matrix
## setinversematrix set the value of the inverse matrix
## getinversematrix get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initialize vinv which holds the cashed value     
        vinv <- NULL
        
        setmatrix<- function(y){
                ## assign a new value y to matrix x 
                x<<- y 
                ## reset the vinv var to null in the parent environment 
                ## as a new value is assigned to matrix x
                vinv<<- NULL
                
        }
        ## get the value of the matrix
        getmatrix<- function() {
                x
        }
        ## create the inverse matrix in the cash
        setinversematrix <- function(inverse){
                vinv<<- inverse
        }
        
        ## get the value of the inverse stored in cash
        getinversematrix<- function(){
                vinv
        }
        ## return the list
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinversematrix = setinversematrix, 
             getinversematrix = getinversematrix)
        
}


## cacheSolve() takes a matrix and returns the inverse of the matrix
## if the inverse exists in cash then get it from cash else comput
## input: matrix 
## output: the inverse matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        vinv<- x$getinversematrix()
        
        ## if the data is stored in cash get it
        if(!is.null(vinv)) {
                message("getting cached data")
                return(vinv)
        }
        ## else get the matrix, calculate inverse 
        vmatrix<- x$getmatrix()
        vinv<- solve(vmatrix, ...)
        ## store it in cash via setinvesrematrix
        x$setinversematrix(vinv)
        ## print inverse
        vinv
}
