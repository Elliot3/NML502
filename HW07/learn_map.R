## Build the function that learns the mapping

learn_map <- function(SOM_lattice, X, num_iter, radius, ler_rate) {
    
    ## Save the initial parameters
    
    init_radius <- radius
    decay_constant <- num_iter / log(init_radius)
    
    init_ler_rate <- ler_rate
    
    ## Perform the iterated learning
    
    for (i in 1:num_iter) {
        
        ## Decay the learning rate
        
        ler_rate <- init_ler_rate * exp(-i / num_iter)
        
        ## Decay the radius
        
        radius <- init_radius * exp(-i / decay_constant)
        
        ## Randomly select an input vector
        
        rand_ind <- sample(x = 1:dim(X)[2], size = 1)
        x <- as.matrix(X[, rand_ind], ncol = 1)
        
    }
    
}