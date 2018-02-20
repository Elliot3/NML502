## Define the root mean square error function

rms_error <- function(D, y) {
    
    rms <- sqrt(sum((D - y)^2))
    return(rms)
    
}

rms_error_batch <- function(D, y) {
    
    rms <- sqrt(sum((D - y)^2)/length(y))
    return(rms)
    
}