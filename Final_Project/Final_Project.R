########## Workspace Preparation ##########





##### Load Packages #####



library(dplyr)



##### Construct the Functions #####



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

learn_SOM <- function(input_data, num_iter, ler_rate, radius) {
    
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
        
        ##### START HERE #####
        
    }
    
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




















