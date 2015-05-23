#create function that stores functions that allows them to be called
makeCacheMatrix <- function(x = matrix()) #create function of functions
	{	##initial cache is null
		c <- NULL 
		##pass y into x in main function above
		setMatrix <- function(y) 
			{
				x<<- y
				#sets c to be null
				c <<- NULL
			}
			#return x matrix
		getMatrix <- function() x 
		#cache the inverse value
		setInverse <- function(inverse) 
			{	
				c <<- inverse
			}
			#return the cached inversed value
		getInverse <- function() c 
		#return a list of the values
		list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
	}

#next function calculates the inverse of a given matrix
#takes matrix as input parameter into function
cacheSolve <- function(x = matrix(),...)
	{	#calls getInverse function which returns c and will pass into m
		m <- x$getInverse()
		#checks if inverse already exists
		if (!is.null(m))
		{	#returns this message if it is not null
			message('getting cached data')
			#then returns m
			m
		}
		#get new matrix and passes into data
		data <-x$getMatrix()
		#calculates the inverse of data and passes into m
		m <- solve(data)
		#calls set inverse function which stores m into c
		x$setInverse(m)
		#prints m
		m
	}
