#### Simulation to assess robustness and accuracy of mode detection methods #####
#             Step 1: Simulate distributions of ordinal variables              #

if (!require("truncnorm")) install.packages("truncnorm")
library(truncnorm)
library(dplyr)


# Step 1: Simulate distributions
# - Unimodal: Gaussians and truncated Gaussians (right and left skewed)
# - Bimodal: Gaussians with small and large overlap; truncated Gaussians
# - Trimodal: Gaussians with small and large overlap; truncated Gaussians
# - Uniform distributions
# - Beta distributions?
# -> different sample sizes (300, 500, 20000, 30000) 
# -> all on categorical scale (1-10)


# STEP 1: SIMULATE DISTRIBUTIONS ----------------------------------------------#

# unimodal distributions------------------------------------

# define function to simulate (truncated) normal distribution
simulate_unimodal <- function(n, mu, sigma, truncated = FALSE){  
    # non-truncated distributions
  if(truncated == FALSE){
    # Generate normal distribution data
    data <- rnorm(n, mean = mu, sd = sigma)
    
    # scale to 1-10 without truncating
    data_scaled <- 9 * (data - min(data)) / (max(data) - min(data)) + 1 
    
    # Converting to categorical data 
    if(mu > 5){
      categorical_data <- floor(data_scaled)
    } else {
      categorical_data <- ceiling(data_scaled)
    }
    
  # truncated distributions   
  } else {
    # Generate truncated normal distribution data 
    data <- rtruncnorm(n, a=1, b=10, mean=mu, sd=sigma)
    
    # Converting to categorical data 
    if(mu > 5){
      categorical_data <- ceiling(data)
    } else {
      categorical_data <- floor(data)
    }
  }
  
  return(categorical_data)
}


# simulate non-truncated normals --
# non-truncated, N = 300
set.seed(1)
normal1 <- simulate_unimodal(n=300, mu=5, sigma=2, truncated = FALSE)

# non-truncated, N = 500
set.seed(1)
normal2 <- simulate_unimodal(n=500, mu=5, sigma=2)

# non-truncated, N = 20,000
set.seed(1)
normal3 <- simulate_unimodal(n=20000, mu=5, sigma=2)

# non-truncated, N = 30,000
set.seed(1)
normal4 <- simulate_unimodal(n=30000, mu=5, sigma=2)

# simulate truncated normals --

# truncated, right skewed,  N = 300
set.seed(1)
trunc_normal1R <- simulate_unimodal(n=300, mu=9, sigma= 2, truncated = TRUE)

# truncated, right skewed,  N = 500  
set.seed(1)
trunc_normal2R <- simulate_unimodal(n=500, mu=9, sigma= 2,  truncated = TRUE)

# truncated, right skewed,  N = 20,000
set.seed(1)
trunc_normal3R <- simulate_unimodal(n=20000, mu=9, sigma= 2, truncated = TRUE)

# truncated, right skewed,  N = 30,000  
set.seed(1)
trunc_normal4R <- simulate_unimodal(n=30000, mu=9, sigma= 2,  truncated = TRUE)

# truncated, left skewed,  N = 300
set.seed(1)
trunc_normal1L <- simulate_unimodal(n=300, mu=2, sigma= 2,  truncated = TRUE)

# truncated, left skewed,  N = 500
set.seed(1)
trunc_normal2L <- simulate_unimodal(n=500, mu=2, sigma= 2, truncated = TRUE)

# truncated, left skewed,  N = 20,000 
set.seed(1)
trunc_normal3L <- simulate_unimodal(n=20000, mu=2, sigma= 2,  truncated = TRUE)

# truncated, left skewed,  N = 30,000
set.seed(1)
trunc_normal4L <- simulate_unimodal(n=30000, mu=2, sigma= 2, truncated = TRUE)


# plot all unimodal distributions -----
unimodal_list <- list(normal1, normal2, normal3, normal4, trunc_normal1R, trunc_normal2R, 
             trunc_normal3R, trunc_normal4R, trunc_normal1L, trunc_normal2L, 
             trunc_normal3L, trunc_normal4L)
titles <- c("Normal distribution (N = 300, mean = 5, SD = 2)", 
            "Normal distribution (N = 500, mean = 5, SD = 2)",
            "Normal distribution (N = 20,000, mean = 5, SD = 2)", 
            "Normal distribution (N = 30,000, mean = 5, SD = 2)", 
            "Truncated normal distribution (mean = 9, sd = 2, N = 300)", 
            "Truncated normal distribution (mean = 9, sd = 2, N = 500)",
            "Truncated normal distribution (mean = 9, sd = 2, N = 20,000)",
            "Truncated normal distribution (mean = 9, sd = 2, N = 30,000)",
            "Truncated normal distribution (mean = 2, sd = 2, N = 300)", 
            "Truncated normal distribution (mean = 2, sd = 2, N = 500)",
            "Truncated normal distribution (mean = 2, sd = 2, N = 20,000)",
            "Truncated normal distribution (mean = 2, sd = 2, N = 30,000)")

colors <- c(rep("lightblue", 4), rep("darkblue", 8))
borders <- c(rep("darkblue", 4), rep("lightblue", 8))

#par(mfrow = c(3,4))
for (i in 1:length(unimodal_list)) {
  hist(unimodal_list[[i]],
       breaks = 0.5 + 0:10, xlim=c(1,11),
       prob = TRUE, 
       main = "", 
       xlab = "Simulated values", 
       xaxt = "n",
       col = colors[i], 
       border = borders[i])
  axis(1, at=1:10, labels=1:10)
}

# save simulated distributions
saveRDS(unimodal_list, "./Analyses/Modality_detection/unimodal_distributions.RData")


# Bimodal mixture distributions ----------------------------------

# define function to simulate (truncated) mixture of 2 normal distributions
simulate_bimodal <- function(n1, mu1, sigma1, n2, mu2, sigma2, truncated = FALSE){  
 
   # non-truncated distributions
  if(truncated == FALSE){
    # Generate 1st normal distribution 
    data1 <- rnorm(n1, mean = mu1, sd = sigma1)
    # General 2nd normal distribution 
    data2 <- rnorm(n2, mean = mu2, sd = sigma2)
  
    # merge 
    data <- c(data1, data2)
    
    # scale to 0.5-10 without truncating (linear transformation)
    data_scaled <- 9.5 * (data - min(data)) / (max(data) - min(data)) + 0.5
    
    # Converting to categorical data 
    categorical_data <- ceiling(data_scaled)
    
    # truncated distributions   
  } else if(truncated == TRUE) {
    # Generate 1st truncated normal distribution data 
    data1 <- rtruncnorm(n1, a=0.5, b=10, mean=mu1, sd=sigma1)
    data2 <- rtruncnorm(n2, a=0.5, b=10, mean=mu2, sd=sigma2)
    
    # merge 
    data <- c(data1, data2)
    
    # categorize
    categorical_data <- ceiling(data)
  } 
  return(categorical_data)
}



# simulate non.truncated normal bimodal distributions ---

# small diverence, N = 300
set.seed(1)
bi_normal1 <- simulate_bimodal(n1 = 125, mu1 = 4, sigma1 = 1, 
                               n2 = 175, mu2 = 7.5, sigma2 = 1)

# small divergence, N = 500
set.seed(1)
bi_normal2 <- simulate_bimodal(n1 = 220, mu1 = 4, sigma1 = 1, 
                               n2 = 280, mu2 = 7.5, sigma2 = 1)

# small divergence, N = 20,000
set.seed(1)
bi_normal3 <- simulate_bimodal(n1 = 9000, mu1 = 4, sigma1 = 1, 
                               n2 = 11000, mu2 = 7.5, sigma2 = 1)

# small divergence, N = 30,000
set.seed(1)
bi_normal4 <- simulate_bimodal(n1 = 12000, mu1 = 4, sigma1 = 1, 
                               n2 = 18000, mu2 = 7.5, sigma2 = 1)

# strong divergence, N = 300
set.seed(1)
bi_normal5 <- simulate_bimodal(n1 = 125, mu1 = 2, sigma1 = 1, 
                               n2 = 175, mu2 = 8, sigma2 = 1)

# strong divergence, N = 500
set.seed(1)
bi_normal6 <- simulate_bimodal(n1 = 220, mu1 = 2, sigma1 = 1, 
                               n2 = 280, mu2 = 8, sigma2 = 1)

# strong divergence, N = 20,000
set.seed(1)
bi_normal7 <- simulate_bimodal(n1 = 9000, mu1 = 2, sigma1 = 1, 
                               n2 = 11000, mu2 = 8, sigma2 = 1)

# strong divergence, N = 30,000
set.seed(1)
bi_normal8 <- simulate_bimodal(n1 = 12000, mu1 = 2, sigma1 = 1, 
                               n2 = 18000, mu2 = 8, sigma2 = 1)


# simulate truncated normal bimodal distributions ---

# strong divergence, N = 300
set.seed(1)
bi_normal_trunc1 <- simulate_bimodal(n1 = 150, mu1 = 1, sigma1 = 1.5, 
                                     n2 = 150, mu2 = 10, sigma2 = 1.5, 
                                     truncated = TRUE)

# strong divergence, N = 500
set.seed(1)
bi_normal_trunc2 <- simulate_bimodal(n1 = 250, mu1 = 1, sigma1 = 1.5, 
                                     n2 = 250, mu2 = 10, sigma2 = 1.5, 
                                     truncated =TRUE)

# strong divergence, N = 20,000
set.seed(1)
bi_normal_trunc3 <- simulate_bimodal(n1 = 10000, mu1 = 1, sigma1 = 1.5, 
                                     n2 = 10000, mu2 = 10, sigma2 = 1.5, 
                                     truncated = TRUE)

# strong divergence, N = 30,000
set.seed(1)
bi_normal_trunc4 <- simulate_bimodal(n1 = 15000, mu1 = 1, sigma1 = 1.5, 
                                     n2 = 15000, mu2 = 10, sigma2 = 1.5, 
                                     truncated = TRUE)


# plot all bimodal distributions --- 
bimodal_list <- list(bi_normal1, bi_normal2, bi_normal3, bi_normal4, bi_normal5, 
                    bi_normal6, bi_normal7, bi_normal8, bi_normal_trunc1, bi_normal_trunc2, 
                    bi_normal_trunc3, bi_normal_trunc4)
titles <- c("2 Normal distributions (N = 300, moderate divergence)", 
            "2 Normal distributions (N = 500,  moderate divergence)",
            "2 Normal distributions (N = 20,000,  moderate divergence)", 
            "2 Normal distributions (N = 30,000,  moderate divergence)", 
            "2 Normal distributions (N = 300, strong divergence)", 
            "2 Normal distributions (N = 500,  strong divergence)",
            "2 Normal distributions (N = 20,000,  strong divergence)", 
            "2 Normal distributions (N = 30,000,  strong divergence)",
            "2 Truncated normal distributions (N = 300)", 
            "2 Truncated normal distributions (N = 500)",
            "2 Truncated normal distributions (N = 20,000)",
            "2 Truncated normal distributions (N = 30,000)")

colors <- c(rep("lightblue", 8), rep("darkblue", 4))
borders <- c(rep("darkblue", 8), rep("lightblue", 4))

par(mfrow = c(3,4))
for (i in 1:length(bimodal_list)) {
  hist(bimodal_list[[i]],
       breaks = 0.5 + 0:10, xlim=c(1,11),
       prob = TRUE, 
       main = "", 
       xlab = "Simulated values", 
       xaxt = "n",
       col = colors[i], 
       border = borders[i])
  axis(1, at=1:10, labels=1:10)
}

# save simulated distributions
saveRDS(bimodal_list, "./Analyses/Modality_detection/bimodal_distributions.RData")


# Simulate uniform distribution ---------------------

# for N = 300 and N=30,000
set.seed(1)
uniform1 <- sample(1:10, size = 300, replace = TRUE)

set.seed(1)
uniform2 <- sample(1:10, size = 30000, replace = TRUE)

# plot
par(mfrow=c(1,2))
hist(uniform1,
     breaks = 0.5 + 0:10, xlim=c(1,11),
     prob = TRUE, 
     main = "", 
     xlab = "Simulated values", 
     xaxt = "n",
     col = "pink")
axis(1, at=1:10, labels=1:10)

hist(uniform2,
     breaks = 0.5 + 0:10, xlim=c(1,11),
     prob = TRUE, 
     main = "", 
     xlab = "Simulated values", 
     xaxt = "n",
     col = "pink")
axis(1, at=1:10, labels=1:10)


# save simulated distributions
uniform_list <- list(uniform1, uniform2)
saveRDS(uniform_list, "./Analyses/Modality_detection/uniform_distributions.RData")


# simulate trimodal distribution -------------

simulate_trimodal <-  function(n1, mu1, sigma1, n2, mu2, sigma2, n3, mu3, sigma3, truncated = FALSE){
  
  # non-truncated 
  if(truncated == FALSE){
    # Generate 3 normal distribution 
    data1 <- rnorm(n1, mean = mu1, sd = sigma1)
    data2 <- rnorm(n2, mean = mu2, sd = sigma2)
    data3 <- rnorm(n3, mean = mu3, sd = sigma3)
    
    # merge 
    data <- c(data1, data2, data3)
    
    # scale to 0.5-10 without truncating (linear transformation)
    data_scaled <- 9.5 * (data - min(data)) / (max(data) - min(data)) + 0.5
    
    # Converting to categorical data 
    categorical_data <- ceiling(data_scaled)
  
  } else {
    # Generate 1st truncated normal distribution data 
    data1 <- rtruncnorm(n1, a=0.5, b=10, mean=mu1, sd=sigma1)
    data2 <- rtruncnorm(n2, a=0.5, b=10, mean=mu2, sd=sigma2)
    data3 <- rtruncnorm(n3, a=0.5, b=10, mean=mu3, sd=sigma3)
    
    # merge 
    data <- c(data1, data2, data3)
    
    # categorize
    categorical_data <- ceiling(data)
    }
  
  return(categorical_data)
}  


# simulate mixture of non-truncated normals ----

# large overlap, N = 300
set.seed(1)
tri_normal1 <- simulate_trimodal(n1 = 100, mu1=2, sigma1 = 1,
                                 n2 = 100, mu2=5, sigma2 = 1, 
                                 n3 = 100, mu3 = 8, sigma3 = 1)

# large overlap, N = 500
set.seed(1)
tri_normal2 <- simulate_trimodal(n1 = 150, mu1=2, sigma1 = 1,
                                 n2 = 150, mu2=5, sigma2 = 1, 
                                 n3 = 200, mu3 = 8, sigma3 = 1)

# large overlap, N = 20,000
set.seed(1)
tri_normal3 <- simulate_trimodal(n1 = 6500, mu1=2, sigma1 = 1,
                                 n2 = 6500, mu2=5, sigma2 = 1, 
                                 n3 = 7000, mu3 = 8, sigma3 = 1)

# large overlap, N = 30,000
set.seed(1)
tri_normal4 <- simulate_trimodal(n1 = 10000, mu1=2, sigma1 = 1,
                                 n2 = 10000, mu2=5, sigma2 = 1, 
                                 n3 = 10000, mu3 = 8, sigma3 = 1)

# large divergence, N = 300
set.seed(1)
tri_normal5 <- simulate_trimodal(n1 = 100, mu1=2, sigma1 = 0.5,
                                 n2 = 100, mu2=6, sigma2 = 0.5, 
                                 n3 = 100, mu3=9, sigma3 = 0.5)
# large divergence, N = 500
set.seed(1)
tri_normal6 <- simulate_trimodal(n1 = 150, mu1=2, sigma1 = 0.5,
                                 n2 = 150, mu2=6, sigma2 = 0.5, 
                                 n3 = 200, mu3=9, sigma3 = 0.5)

# large divergence, N = 20,000
set.seed(1)
tri_normal7 <- simulate_trimodal(n1 = 6500, mu1=2, sigma1 = 0.5,
                                 n2 = 6500, mu2=6, sigma2 = 0.5, 
                                 n3 = 7000, mu3=9, sigma3 = 0.5)
# large divergence, N = 30,000
set.seed(1)
tri_normal8 <- simulate_trimodal(n1 = 10000, mu1=2, sigma1 = 0.5,
                                 n2 = 10000, mu2=6, sigma2 = 0.5, 
                                 n3 = 10000, mu3=9, sigma3 = 0.5)



# simulate truncated trimodal distribution -----

# N = 300
set.seed(1)
tri_normal_trunc1 <- simulate_trimodal(n1 = 100, mu1=1, sigma1 = 1,
                                 n2 = 100, mu2=5, sigma2 = 0.5, 
                                 n3 = 100, mu3=9, sigma3 = 1, 
                                 truncated = TRUE)
# N = 500
set.seed(1)
tri_normal_trunc2 <- simulate_trimodal(n1 = 150, mu1=1, sigma1 = 1,
                                      n2 = 200, mu2=5, sigma2 = 0.5, 
                                      n3 = 150, mu3=9, sigma3 = 1, 
                                      truncated = TRUE)
# N = 20,000
set.seed(1)
tri_normal_trunc3 <- simulate_trimodal(n1 = 6500, mu1=1, sigma1 = 1,
                                      n2 = 7000, mu2=5, sigma2 = 0.5, 
                                      n3 = 6500, mu3=9, sigma3 = 1, 
                                      truncated = TRUE)

# N = 30,000
set.seed(1)
tri_normal_trunc4 <- simulate_trimodal(n1 = 10000, mu1=1, sigma1 = 1,
                                      n2 = 10000, mu2=5, sigma2 = 0.5, 
                                      n3 = 10000, mu3=9, sigma3 = 1, 
                                      truncated = TRUE)


# plot all trimodal distributions --- 
trimodal_list <- list(tri_normal1, tri_normal2, tri_normal3, tri_normal4, tri_normal5,
                     tri_normal6, tri_normal7, tri_normal8, tri_normal_trunc1, 
                     tri_normal_trunc2, tri_normal_trunc3, tri_normal_trunc4)
titles <- c("3 Normal distributions (N = 300, moderate divergence)", 
            "3 Normal distributions (N = 500,  moderate divergence)",
            "3 Normal distributions (N = 20,000,  moderate divergence)", 
            "3 Normal distributions (N = 30,000,  moderate divergence)", 
            "3 Normal distributions (N = 300, strong divergence)", 
            "3 Normal distributions (N = 500,  strong divergence)",
            "3 Normal distributions (N = 20,000,  strong divergence)", 
            "3 Normal distributions (N = 30,000,  strong divergence)",
            "3 Truncated normal distributions (N = 300)", 
            "3 Truncated normal distributions (N = 500)",
            "3 Truncated normal distributions (N = 20,000)",
            "3 Truncated normal distributions (N = 30,000)")

colors <- c(rep("lightblue", 8), rep("darkblue", 4))
borders <- c(rep("darkblue", 8), rep("lightblue", 4))

par(mfrow = c(3,4))
for (i in 1:length(trimodal_list)) {
  hist(trimodal_list[[i]],
       breaks = 0.5 + 0:10, xlim=c(1,11),
       prob = TRUE, 
       main = "", 
       xlab = "Simulated values", 
       xaxt = "n",
       col = colors[i], 
       border = borders[i])
  axis(1, at=1:10, labels=1:10)
}

# save simulated distributions
saveRDS(trimodal_list, "./Analyses/Modality_detection/trimodal_distributions.RData")

# combine into overall list 
distributions_list <- c(uniform_list, unimodal_list, bimodal_list, trimodal_list)
names(distributions_list) <- c("uniform_300", "uniform_30000", "unimodal_normal_300",
                               "unimodal_normal_500", "unimodal_normal_20000", 
                               "unimodal_normal_30000", "unimodal_trunc_right_300",
                               "unimodal_trunc_right_500", "unimodal_truc_right_20000",
                               "unimodal_trunc_right_30000", "unimodal_trunc_left_300", 
                               "unimodal_trunc_left_500", "unimodal_trunc_left_20000",
                               "unimodal_trunc_left_30000", "bimodal_normal_smalldiv_300",
                               "bimodal_normal_smalldiv_500", "bimodal_normal_smalldiv_20000",
                               "bimodal_normal_smalldiv_30000", 
                               "bimodal_normal_largediv_300", "bimodal_normal_largediv_500",
                               "bimodal_normal_largediv_20000", "bimodal_normal_largediv_30000",
                               "bimodal_trunc_300", "bimodal_trunc_500", "bimodal_trunc_20000",
                               "bimodal_trunc_30000",
                               "trimodal_normal_smalldiv_300", "trimodal_normal_smalldiv_500",
                               "trimodal_normal_smalldiv_20000", "trimodal_normal_smalldiv_30000",
                               "trimodal_normal_largediv_300", "trimodal_normal_largediv_500",
                               "trimodal_normal_largediv_20000", "trimodal_normal_largediv_30000",
                               "trimodal_trunc_300", "trimodal_trunc_500", "trimodal_trunc_20000",
                               "trimodal_trunc_30000")

saveRDS(distributions_list, "./Analyses/Modality_detection/all_distributions.RData")


