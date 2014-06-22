## The makeCacheMatrix creates a square matrix, sends the matrix
#to another function for inversion and then caches the returned 
#inverted matrix.

#create matrix object
makeCacheMatrix <- function(x = matrix()) { 

#set/get are for the matrix and 
#setmatrix/getmatrix are for inverse matrix
m <- NULL
set <- function(y) {
  x <<- y
  m <<- NULL
  }
get <- function() x
setmatrix <- function(matrix) m <<- matrix
getmatrix <- function() m

list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
  
#set x as 2x2 matrix filled with 0s and 4s
x <- x$set(matrix(c(0,4,4,0),2,2))  

##Retrieve matrix
x$get()  

##Call to look for inverted matrix in cache. 
cacheSolve(x) 

##Cache the matrix
x$setmatrix()         

##Retest for cached, inverted matrix. Message "getting cached matrix"
##let's us know that an inverted, unchanged matrix was found 
##and returned.
cacheSolve(x)    

}


##Called from makeCacheMatrix.  The cacheSolve function
##first checks the cache to see if a matrix already exists 
##and tests for changes. It performs matrix inversion if needed.

cacheSolve <- function(x, ...) {  

##Pull cached matrix object if any exists
    m<-x$getmatrix()  
    
##Test if cache is empty or differs from passed matrix

    if(is.null(m)) or (!all.equal(x,m)) {
              
    ##If either of these condition are true then invert the 
    ##passed matrix instead.                   
          
        m <- solve(x)          

    else
        message("getting cached matrix")
    
    }

  return(m)          #return a new or cached inverted matrix       

}
