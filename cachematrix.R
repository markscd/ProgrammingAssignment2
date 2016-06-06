# M. Farrell, 5 June 2016
# functions to store a matrix with values provided
# and then provide the inverse of that matrix.

# And if the inverse of the matrix has already been returned,
# do not re-calculate; simply provide the results and a message
# saying that it is being returned.

makeCacheMatrix <- function(x = matrix()) {
     # Create the matrix to be stored.
     # We can do this by simply creating a 2x2 matrix:
     # m <- matrix(1:4,2,2)
     # We can then see the expected results by entering:
     # solve(m)
     # Then, we can test our function with the matrix we created for 'm' in the variable 'm2':
     # m2 <- makeCacheMatrix(m)
     # Then, by entering our secondary function, we should notice that it's calculating the results,
     # and the results should be the same as the solve(m) function.
     # cacheSolve(m2)
     # But, when we enter the function again, we should get the message that it's obtaining the
     # cached results, with it skipping the other parameters.
     # cacheSolve(m2)
     # Thus, the message "Obtaining cached data" should appear each successive time we use 
     # the function ~ cacheSolve(m2)
     
     # Initially set the inverse "m" to null
     m <- NULL
     
     # Reset the matrix 'x' a new value via Set function; 
     set <- function(y)            {
          x <<- y
          
          # 'm' (inverse) is also reset to Null
          m <<- NULL
                                   }
     
     # Return the matrix
     get <- function() x
               {
          # After the inverse is calculated, the vector's inverse attribute value is set
          setInverse <- function(solve) {m <<- solve}
          
          # The inverse value is returned
          getInverse <- function() m
          
          # List the methods inside of makeCacheMatrix
          list(set = set,
               get = get,
               setInverse = setInverse,
               getInverse = getInverse)
               }
                                             }


cacheSolve <- function(x, ...) {
     # Calculate the matrix's inverse; if stored, skip the calculation.
     
     # Return the matrix that is x's inverse if already obtained.
     m <- x$getInverse()
     
     # Check to see if m is not NULL (i.e., if results already stored)
     if (!is.null(m)) {
          
          # If it is not null, provide the message that the cached data is being returned
          # and this will end further processing, with the value of 'm' returned at the end
          # of this procedure, skipping the 'else' procedure below.
          message("Obtaining cached data")
                      }
     
     # Else, if getInverse has not been used to store the data,
     # then 'get' matrix stored in the vector and put in 'data'
     else           {
          
          # get: "Return the Value of a Named Object" -- CRAN
          data <- x$get()
               {
               # Apply the solve function for the returned data.
               # solve: "This generic function solves the equation
               # a %*% x = b for x, where b can be either a vector or a matrix." -- CRAN
               m <- solve(data)
               
               # The results are stored in the special vector, which is the inverse
               x$setInverse(m)
               }
                    }
     
     # Return the inverse of the matrix whether already known (skipped 'else')
     # or if calculated (within 'else' structure).  1 less line of code.
     return(m)
                                   }
