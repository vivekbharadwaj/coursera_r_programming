#Call  the makeVector() function and assign it's
#  return value ( a list of four functions) to a variable, v
#  v is now a list of four functions
v <- makeVector()

#use v's set function to create a vector 
#  containing the numbers 20 through to 40
v$set(20:40)

#use v's get function to retrieve the vector created 
v$get()

#pass the list v to the cachemean() function
#   the mean of the numeric vector 20:40 should be returned
cachemean(v)

#pass the list v to the cachemean() function a second time
#  the mean of the numeric vector 20:40 should be returned
#  also a message "retrieving value from cache" indicating that the mean
#  is not being calculated this time but is being retrieved from the cached
#  value
cachemean(v)

#use v's set function to create a new vector 
#  containing the numbers 23,23,34.6,654.35
v$set(c(23,23,34.6,654.35))

#pass the list v to the cachemean() function
#   the mean of the numeric vector 23,23,34.6,654.35 should be returned
cachemean(v)

#pass the list v to the cachemean() function a second time
#  the mean of the numeric vector 23,23,34.6,654.35 should be returned
#  also a message "retrieving value from cache" indicating that the mean
#  is not being calculated this time but is being retrieved from the cached
#  value
cachemean(v)