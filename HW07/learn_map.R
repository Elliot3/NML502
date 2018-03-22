## Build the function that learns the mapping

learn_map <- function(SOM_lattice, X, num_iter, radius, ler_rate) {
    
    ## Container for the current prototype lattice
    
    SOM_container <- list()
    
    ## Container for learning step, radius and learning rate
    
    param_container <- list()
    ler_steps <- numeric()
    radii <- numeric()
    ler_rates <- numeric()
    
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
        
        ## Add the parameters states to the container
        
        if (i %% 5000 == 0) {
        
            ler_steps[length(ler_steps) + 1] <- i
            radii[length(radii) + 1] <- radius
            ler_rates[length(ler_rates) + 1] <- ler_rate
        
        }
        
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
        
        ## Add to the container
        
        if ((i == 1) || (i %in% seq(from = 0, to = num_iter, length.out = 6))) {
            
            SOM_container[[length(SOM_container) + 1]] <- list(i, SOM_lattice[[2]])
            
        }
        
    }
    
    param_container[[1]] <- ler_steps
    param_container[[2]] <- radii
    param_container[[3]] <- ler_rates
    
    param_container <<- param_container
    
    return(SOM_container)
    
}

