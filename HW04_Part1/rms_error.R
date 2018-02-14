## Define the root mean square error function

rms_error <- function(D, y) {
    
    rms <- sqrt(sum((D - y)^2)/100)
    return(rms)
    
}