## Build the plot_SOM function

plot_SOM <- function(learn_results) {
    
    for (i in 1:length(learn_results)) {
        
        ler_step <- learn_results[[i]][[1]]
        x_lattice <- as.vector(learn_results[[i]][[2]][[1]])
        y_lattice <- as.vector(learn_results[[i]][[2]][[2]])
        
        plot_lattice <- cbind(x_lattice, y_lattice)
        
        plot(plot_lattice, pch = 16, main = c("SOM Grid at Learning Step", ler_step), xlab = "", ylab = "")
        
        for (i in 1:dim(plot_lattice)[1]) {
            
            if (i %% matrix_dim != 0) {
                
                segments(plot_lattice[i, 1], plot_lattice[i, 2], plot_lattice[i + 1, 1], plot_lattice[i + 1, 2])
                
            }
            
        }
        
        for (i in 1:dim(plot_lattice)[1]) {
            
            if (((dim(plot_lattice)[1] - i) - matrix_dim) >= 0) {
                
                segments(plot_lattice[i, 1], plot_lattice[i, 2], plot_lattice[i + matrix_dim, 1], plot_lattice[i + matrix_dim, 2])
                
            }
            
        }
        
    }
    
}