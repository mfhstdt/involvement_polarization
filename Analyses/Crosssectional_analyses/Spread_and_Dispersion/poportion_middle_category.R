#### Compute proportion of respondents choosing middle category of the scale ####
####################### for each involvement level ###############################

library(dplyr)
library(scales)
source("helper_functions/compute_mid_prop.R")
source("helper_functions/split_involvement.R")


# For LISS --------------------------------------------------------------------#
liss_data <- read.csv("Data/processed_data/LISS14_2.csv") %>%
  select(-X) %>%
  na.omit(.)
liss_data$inv_quartile <- split_involvement(liss_data$mean_involvement)

for(i in 1:4){
  print(paste("Involvement", i, "Proportion middle category:",
              round(mid_prop(liss_data$attitude[liss_data$inv_quartile == i]), 3)))
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
  print(paste("Involvement", i, "Proportion middle category:",
              round(mid_prop(anes_data$attitude[anes_data$inv_quartile == i]), 3)))
}


# For EVS/WVS ------------------------------------------------------------------#
ewvs_data <- read.csv("Data/processed_data/evs_wvs17.csv") %>%
  select(-X) %>%
  na.omit(.)

for(i in 1:4){
  print(paste("Involvement", i, "Proportion middle category:",
              round(mid_prop(ewvs_data$attitude[ewvs_data$inv_quartile == i]), 3)))
}


# for Eurobarometer ------------------------------------------------------------#
euro_data <- read.csv("Data/processed_data/eurobarometer_data.csv") %>%
  select(-X) %>%
  na.omit(.)

table(euro_data$attitude) # scale -2 to 2; midpoint is 0


for(i in 1:3){
  print(paste("Involvement", i, "Proportion middle category:",
              round(mid_prop(euro_data$attitude[euro_data$involvement == i]), 3)))
}


# for Covid datasets -----------------------------------------------------------#
for(i in 1:6){
  path <- paste0("Data/processed_data/covid_T",i,".csv")
  data <- read.csv(path) %>% 
    select(-X)
  
  for(j in 1:7){
    sub_set <- data[data$Vacc_Oth_6 == j, ]
    print(paste("Covid T", i, "- Involvement", j, "- Proportion middle category:",
                round(mid_prop_continuous(sub_set$mean_attitude, cats = c(1,7)), 3)))
  }
  print("------")
}


