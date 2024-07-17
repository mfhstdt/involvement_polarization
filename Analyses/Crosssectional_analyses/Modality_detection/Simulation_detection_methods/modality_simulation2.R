#### Simulation to assess robustness and accuracy of mode detection methods #####
#          Step 2: write functions for modality detection methods              #

if (!require("mousetrap")) install.packages("mousetrap")
if (!require("silvermantest")) remotes::install_github("jenzopr/silvermantest")
if (!require("diptest")) install.packages("diptest")
if (!require("mclust")) install.packages("mclust")

library(silvermantest) # for silverman's method
library(mousetrap)  # for bimodality coefficient
library(diptest) # for Hartigan's dip statistic 
library(mclust) # Gaussian mixture modeling 
source("./helper_functions/compute_DFU.R") # for distance from unimodality measure
source("./helper_functions/compute_DensMMdet.R") # for Haslbeck et al. method
source("./helper_functions/ANAM.R") # for ANAM

# Step 2: Specify mode detection algorithms: #####################################
# - Silverman 
# - BC
# - dip 
# - DFU (implement self)
# - mclust (Gaussian mixture)
# - Density based detection method by Haslbeck,Ryan & Dablander(2022)  
# - Density based detection method with adjusted noise for large N (ANAM)


# Silverman's Mode Estimation Method ------------------------------------------#
# silverman's method tests the H0 that a distribution has at most k modes 

# function to apply silverman's metod iteratively for 1-k_max modes; 
# it stops as soon as it has found a non-significant result (i.e., a robust number of modes)
apply_silverman <- function(data_list){
  
  # store number of modes for each dataset 
  modes <- c()

  # for each distribution
  for(i in 1:length(data_list)){
    x <- data_list[[i]]
    
    # start with k=1 mode, adjust p-value as suggested by Hall & York, 2001
    result <- silverman.test(x, 1, adjust = TRUE) 
    
    k = 1
    while(result@p_value <= 0.01){ # until result is non-significant 
      k = k+1
      result <- silverman.test(x, k)
    }
    
   # store number of modes
    modes[i] <- k
  }
  return(modes)
}


# Bimodality coefficient ------------------------------------------------------#

# function to apply bimodality coefficient to each distribution; stores BC and result
apply_bc <- function(data_list){
  
  # store BC and bimodality result (TRUE/FALSE)
  results_bc <- data.frame(distribution = 1:length(data_list), 
                           BC = NA, 
                           bimodality = NA)
  
  for(i in 1:length(data_list)){ # for each distribution
    x <- data_list[[i]]
    
    bc <- bimodality_coefficient(x)
    
    results_bc$BC[i] <- bc
    results_bc$bimodality[i] <- bc > 0.555
  }
  
  return(results_bc)
}


# Hartigan's dip statistic ----------------------------------------------------#

# function to apply the dip test to multiple distributions and store whether multimodality is detected
apply_dip <- function(data_list){
  dip_results <- c()
  
  for(i in 1:length(data_list)){
    x <- data_list[[i]]
    
    result <- dip.test(x)
    
    dip_results[i] <- result$p.value < 0.01
  }

  return(dip_results)
}


# Gaussian Mixture models (mclust) --------------------------------------------#

# function to apply gaussian mixture models to detect #modes for multiple distributions
apply_gaussianmix <- function(data_list){
  # store estimated number of modes (solution with lowest BIC)
  gaussian_modes <- c()
  
  for(i in 1:length(data_list)){ # for each distribution
    x <- data_list[[i]]
    
    gaussian_modes[i] <- summary(mclustBIC(x))[1]
  }
  return(gaussian_modes)
  
}



# Distance from unimodality (Pavlopoulos & Likas, 2022) -----------------------#

apply_dfu <- function(data_list){
  dfu_results <- c()
  
  for(i in 1:length(data_list)){ # for each distribution
    x <- data_list[[i]]
    
    dfu_results[i] <- dfu(x)
  }
  return(dfu_results)
}


# Density based methodn(Haslbeck et al) ---------------------------------------#

apply_densMM <- function(data_list){
  densMM_results <- c()
  
  for(i in 1:length(data_list)){ #  for each distribution
   x <- distributions_list[[i]]
    
    # define noise hyper-parameter as sd of noise (since we have Likert scale data)
    noise <- (max(table(x))/length(x))
    
    # store results
    v_M <- rep(NA, 100) # store results
    l_M <- list()
    
    # estimate modes and density 100 times to reduce arbitrariness of specific noise draws
    for(r in 1:100) { 
      l_M[[r]] <- DensMMdet(x, n= 100, noise=noise, adjust=3)
      v_M[r] <- l_M[[r]]$M  # save number of modes
    }
    
    # check which mode was detected most frequently 
    tb <- table(v_M)
    most_freq_mode <- as.numeric(names(tb)[which.max(tb)])  
    
    
    # save result
    densMM_results[i] <- most_freq_mode
  }
}

# Density based method with adjusted noise (ANAM) ------------------------------------#

apply_densMM_adj <- function(data_list){
  densMM_adj_results <- c()
  
  for(i in 1:length(data_list)){
    x <- data_list[[i]]
    
    result <- DensMMdet_adaptive_noise(x)
    densMM_adj_results[i] <- result$M
  }
  
  return(densMM_adj_results)
}

