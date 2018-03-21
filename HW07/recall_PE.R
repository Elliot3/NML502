## Build the function to recall each input

recall_PE <- function(learn_results, X) {
    
    ## Unwrap the final SOM lattice
    
    x_lattice <- learn_results[[length(learn_results)]][[2]][[1]]
    y_lattice <- learn_results[[length(learn_results)]][[2]][[2]]
    
    ## Container for which PE the input maps to
    
    neuron_map <- matrix(0, ncol = dim(x_lattice)[1], nrow = dim(x_lattice)[1])
    colnames(neuron_map) <- 1:dim(x_lattice)[1]
    rownames(neuron_map) <- 1:dim(x_lattice)[1]
    
    ## Loop through the input data for recall
    
    for (i in 1:dim(X)[2]) {
        
        ## Get the current input vector
        
        x <- matrix(X[, i], ncol = 1)
        
        ## Calculate the matrix of differences
        
        matrix_diffs <- list()
        matrix_diffs[[1]] <- x[1, ] - x_lattice
        matrix_diffs[[2]] <- x[2, ] - y_lattice
        
        ## Get the Euclidean distance
        
        mat_a <- matrix_diffs[[1]]^2
        mat_b <- matrix_diffs[[2]]^2
        matrix_dist <- sqrt(mat_a + mat_b)
        
        ## Find the winning PE
        
        min_neuron <- min(matrix_dist)
        min_loc <- as.vector(which(matrix_dist == min(matrix_dist), arr.ind = TRUE))
        
        ## Add the location to the neuron map
        
        if (neuron_map[min_loc[1], min_loc[2]] == 0) {
            
            neuron_map[min_loc[1], min_loc[2]] <- 1
            
        } else {
            
            neuron_map[min_loc[1], min_loc[2]] <- neuron_map[min_loc[1], min_loc[2]] + 1
            
        }
        
    }
    
    s <- as.character(1:matrix_dim)
    
    temp_vec <- as.vector(neuron_map)
    temp_mat <- matrix(temp_vec, nrow = matrix_dim, ncol = matrix_dim)
    temp_mat <- apply(temp_mat, 2, rev)
    
    
    plot_ly(z = temp_mat, x = ~s, y = ~s, colors = colorRamp(c("white", "black")), type = "heatmap") %>%
        layout(title = "PE Density Map", xaxis = list(title = "PE X Coordinate"), yaxis = list(title = "PE Y Coordinate"))
    
}