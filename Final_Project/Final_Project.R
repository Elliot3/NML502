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

## Define some data details

input_size <- dim(data_final)[2] - 1
matrix_dim <- 15



##### Build the Network #####



SOM_lattice <- build_SOM(input_size, matrix_dim)



########## Perform Analysis ##########




















