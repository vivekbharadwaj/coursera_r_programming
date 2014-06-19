makeVector <- function(x = numeric()) {
        global_mean <- NULL
        set <- function(y) {
                x <<- y
                global_mean <<- NULL
        }
        get <- function() x
        setmean <- function(mean) global_mean <- mean
        getmean <- function() global_mean
        list(var1 = set, var2 = get,
             var3 = setmean,
             var4 = getmean)
}

cachemean <- function(something, ...) {
        m <- something$var4()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- something$var2()
        m <- mean(data, ...)
        something$var3(m)
        data
}






