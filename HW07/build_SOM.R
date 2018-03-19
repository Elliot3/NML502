## Function to build the SOM matrix

build_SOM <- function(input_size, matrix_dim) {
    
    ## Container for the matrices of weights
    
    SOM_weights <- list()
    
    ## Container for the indices
    
    SOM_indices <- list()
    
    ## Build the list of weight indices
    
    SOM_indices[[1]] <- matrix(rep(1:matrix_dim, times = matrix_dim),
                               nrow = matrix_dim, ncol = matrix_dim)
    
    SOM_indices[[2]] <- matrix(rep(1:matrix_dim, times = matrix_dim),
                               byrow = T, nrow = matrix_dim, ncol = matrix_dim)
    
    ## Build the list of weights
    
    for (i in 1:input_size) {
        
        SOM_weights[[i]] <- matrix(runif(n = matrix_dim^2, min = -0.5, max = 0.5),
                                   ncol = matrix_dim, nrow = matrix_dim)
    
    }
    
    ## Return the results
    
    return(list(SOM_indices, SOM_weights))
    
}