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
        
        ## Calculate the matrix of differences
        
        matrix_diffs <- list()
        matrix_diffs[[1]] <- x[1, ] - SOM_lattice[[2]][[1]]
        matrix_diffs[[2]] <- x[2, ] - SOM_lattice[[2]][[2]]
        
        ## Get the Euclidean distance
        
        mat_a <- matrix_diffs[[1]]^2
        mat_b <- matrix_diffs[[2]]^2
        matrix_dist <- sqrt(mat_a + mat_b)
        
        ## Find the winning PE
        
        min_neuron <- min(matrix_dist)
        min_loc <- as.vector(which(matrix_dist == min(matrix_dist), arr.ind = TRUE))
        
        ## Calculate the Manhattan distance
        
        mat_c <- abs(SOM_lattice[[1]][[1]] - min_loc[1])
        mat_d <- abs(SOM_lattice[[1]][[2]] - min_loc[2])
        man_dist <- mat_c + mat_d
        neighbor_func <- exp(-((man_dist)/(radius))^2)
        
        ## Update the weights
        
        SOM_lattice[[2]][[1]] <- SOM_lattice[[2]][[1]] + ler_rate * (neighbor_func * matrix_diffs[[1]])
        SOM_lattice[[2]][[2]] <- SOM_lattice[[2]][[2]] + ler_rate * (neighbor_func * matrix_diffs[[2]])
        
    }
    
    return(SOM_lattice[[2]])
    
}

