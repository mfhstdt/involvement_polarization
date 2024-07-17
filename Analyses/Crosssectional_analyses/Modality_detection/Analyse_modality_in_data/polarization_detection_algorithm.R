###### Function to determine (level of) polarization in empirical data #######

# pol_modality function tests two criteria to classify a distribution as polarized: 
#
# Criterion 1: The distribution diverges from a uniform distribution 
#              > i.e., p-value of Chi² test is significant 
#              > alpha level adjusted with Bonferroni-Holm adjustment
#
# Criterion 2: Multimodality (at least 2 poles)
#              > density based modality detection method (Haslbeck et al., 2022)
#                with adjustment of noise 
source("helper_functions/ANAM.R")

pol_modality <- function(x){  # input is categorical distribution
  # criterion 1
  is_uniform <- chisq.test(table(x))
  
  # criterion2
  density_list <- list()
  num_modes <- c()
  estimations_list <- list()
  
  # 100 iterations to reduce arbitrariness
  for(r in 1:100) { 
    estimations_list[[r]] <- DensMMdet_adaptive_noise(x)
    num_modes[r]<- estimations_list[[r]]$M
  }
  tb <- table(num_modes)
  most_freq_mode <- as.numeric(names(tb)[which.max(tb)])
  density_list <- estimations_list[[which(num_modes == most_freq_mode)[1]]] 
  
  return(list( # output is Chi² result & most frequently detected #modes
    "uniform_p_value" = is_uniform[[3]],
    "uniformity" = if(is_uniform[[3]] > 0.01) TRUE else FALSE, 
    "modality" = as.numeric(names(tb)[which.max(tb)]),
    "density" = list(
      "den_x" = density_list$den_x, 
      "den_y" = density_list$den_y)
  ))
}


