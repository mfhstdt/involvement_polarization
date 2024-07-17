## Assess the dispersion of attitudes by measuring the median absolute deviation ##
################### from the middle category  #####################################

library(dplyr)
library(scales)
source("helper_functions/split_involvement.R")

# For LISS --------------------------------------------------------------------#
liss_data <- read.csv("Data/processed_data/LISS14_2.csv") %>%
  select(-X) %>%
  na.omit(.)
liss_data$inv_quartile <- split_involvement(liss_data$mean_involvement)

for(i in 1:4){
  data <- liss_data[liss_data$inv_quartile == i, 13]
  print(mad(data, center=5)) 
  print("__")
} 


# For ANES --------------------------------------------------------------------#
anes_data <- read.csv("Data/processed_data/anes_data.csv") %>%
  select(-X) %>%
  na.omit(.)
# compute mean inv and split into quartiles
anes_data$inv2 <- rescale(anes_data$inv2, to = c(1, 5)) # transform inv2 to scale 1-5
anes_data$mean_involvement <- rowMeans(anes_data[, 1:2])
anes_data$inv_quartile <- split_involvement(anes_data$mean_involvement)

for(i in 1:4){
  data <- anes_data[anes_data$inv_quartile == i, 3]
  print(mad(data, center=5)) 
  print("__")
} 


# European & World Value Study ------------------------------------------------#
ewvs_data <- read.csv("Data/processed_data/evs_wvs17.csv") %>%
  select(-X) %>%
  na.omit(.)

for(i in 1:4){
  data <- ewvs_data[ewvs_data$inv_quartile == i, 8]
  print(mad(data, center = 5.5)) 
  print("__")
} 


# Eurobarometer ---------------------------------------------------------------#
euro_data <- read.csv("Data/processed_data/eurobarometer_data.csv") %>%
  select(-X) %>%
  na.omit(.)

for(i in 1:3){
  data <- euro_data[euro_data$involvement == i, 2]
  print(mad(data, center = 0)) 
  print("__")
}  


# For all Covid datasets ------------------------------------------------------#
for(i in 1:6){
  path <- paste0("Data/processed_data/covid_T",i,".csv")
  data <- read.csv(path) %>%
    select(-X)
  
  print(paste0("Covid T",i))
  print(paste("Overall MAD = ", median_AD_continuous(data$mean_attitude, mid = 3.5)))
  
  data_inv <- data %>%
    group_split(Vacc_Oth_6)
  for(j in 1:7){
    print(paste("Involvement", j, "MAD = ", mad(data_inv[[j]]$mean_attitude, center = 4)))
  }
  
  print("____")
} 
