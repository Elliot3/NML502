########## Workspace Preparation ##########





##### Load Packages #####



library(dplyr)



##### Construct the Functions #####



## Function to calculate the Manhattan distance

manhattan_dist <- function(rating1, rating2) {
    
    distance <- abs(rating1 - rating2)
    distance <- sum(distance)
    
    return(distance)
    
}

## Function to build the SOM lattice matrix

build_SOM <- function(input_size, matrix_dim) {
    
    ## Get the number of PEs
    
    num_PEs <- matrix_dim^2
    
    ## Build the list of weights
    
    SOM_weights <- runif(n = input_size * num_PEs, min = -0.5, max = 0.5)
    dim(SOM_weights) <- c(input_size, num_PEs)
    
    ## Return the results
    
    return(SOM_weights)
    
}

## Function to perform the SOM leaarning

learn_SOM <- function(input_data, SOM_lattice, num_iter, ler_rate, radius, matrix_dim) {
    
    ## Container for the current prototype lattice
    
    SOM_container <- list()
    
    ## Container for the indices
    
    SOM_indices <- list()
    
    ## Build the list of weight indices
    
    SOM_indices[[1]] <- matrix(rep(1:matrix_dim, times = matrix_dim),
                               nrow = matrix_dim, ncol = matrix_dim)
    
    SOM_indices[[2]] <- matrix(rep(1:matrix_dim, times = matrix_dim),
                               byrow = T, nrow = matrix_dim, ncol = matrix_dim)
    
    ## Record the original ler_rate and radius
    
    init_ler_rate <- ler_rate
    init_radius <- radius
    
    ## Establish a decay constant
    
    decay_constant <- num_iter / log(init_radius)
    
    for (i in 1:num_iter) {
        
        ## Decay the learning rate
        
        ler_rate <- init_ler_rate * exp(-i / num_iter)
        
        ## Decay the radius
        
        radius <- init_radius * exp(-i / decay_constant)
        
        ## Randomly select an input vector
        
        rand_ind <- sample(x = 1:dim(input_data)[1], size = 1)
        input <- matrix(input_data[rand_ind, ], ncol = 1)
        
        ## Calculate the Euclidean distances
        
        list_diffs <- list()
        vec_dists <- numeric()
        
        for (j in 1:dim(SOM_lattice)[2]) {
            
            list_diffs[[j]] <- input - SOM_lattice[[j]]
            vec_dists[j] <- norm(list_diffs[[j]], type = "2")
            
        }
        
        ## Find the winning PE
        
        min_neuron <- min(vec_dists)
        min_list <- as.vector(which(vec_dists == min(vec_dists), arr.ind = TRUE))
        
        x_pt <- floor(min_list / matrix_dim) + 1
        y_pt <- min_list - ((x_pt - 1) * matrix_dim)
        
        min_loc <- c(x_pt, y_pt)
        
        ## Calculate the Manhattan distance
        
        mat_diffs_y <- abs(SOM_indices[[1]] - min_loc[1])
        mat_diffs_x <- abs(SOM_indices[[2]] - min_loc[2])
        man_dist <- mat_diffs_x + mat_diffs_y
        neighbor_func <- as.vector(exp(-((man_dist)/(radius))^2))
        
        ## Update the weights
        
        for (j in 1:dim(SOM_lattice)[2]) {
            
            SOM_lattice[, j] <- as.matrix(SOM_lattice[, j], ncol = 1) + ler_rate * (neighbor_func[j] * list_diffs[[j]])
            
        }
        
        ## Add lattice to the container
        
        if ((i == 1) || (i %in% seq(from = 0, to = num_iter, length.out = 6))) {
            
            SOM_container[[length(SOM_container) + 1]] <- list(i, SOM_lattice)
            
        }
        
    }
    
    return(SOM_container)
    
}



##### Load the Data #####



## Import the data

data <- read.csv("~/Documents/Rice_University/Spring_2018/NML502/Final_Project/recipeData.csv")

## Select my beer styles and variables of interest

selected_styles <- c(7, 10, 134, 9, 4, 30, 86, 12, 92, 6)

data_final <- data %>%
    select(StyleID, Size.L., OG, FG, ABV, IBU, Color, BoilSize, BoilTime, BoilGravity, Efficiency) %>%
    filter(StyleID %in% selected_styles)

## Remove rows with NAs

data_final[data_final == "N/A"] <- NA
data_final <- data_final[complete.cases(data_final), ]

## Separate the input space from the labels

output_space <- matrix(as.numeric(unlist(data_final$StyleID)), nrow = length(data_final$StyleID))
input_space <- matrix(as.numeric(unlist(data_final[, 2:11])), nrow = nrow(data_final[, 2:11]))

## Define some data details

input_size <- dim(input_space)[2]
matrix_dim <- 15



##### Build the Network #####



## Set some network parameters

ler_rate <- 0.001
num_iter <- 5000
radius <- matrix_dim / 2

## Build the weight matrix

SOM_lattice <- build_SOM(input_size, matrix_dim)





########## Perform Analysis ##########





##### Learn the Network #####



learn_results <- learn_SOM(input_data = input_space, SOM_lattice, num_iter, ler_rate, radius, matrix_dim)














