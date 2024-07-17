# Assess differences in mean attitude and SD of attitude between involvement levels #
#####################################################################################

library(dplyr)
library(scales)
library(car)
source("helper_functions/split_involvement.R")
source("helper_functions/print_descriptives.R")


# LISS 14 ---------------------------------------------------------------------#
liss_data <- read.csv("Data/processed_data/LISS14_2.csv") %>%
  select(-X) %>%
  na.omit(.)

liss_data$inv_quartile <- split_involvement(liss_data$mean_involvement)

print_descriptives(liss_data, liss_data$inv_quartile, 13)

# test if significant
summary(aov(attitude ~ as.factor(inv_quartile), data = liss_data)) 
leveneTest(attitude ~ as.factor(inv_quartile), data = liss_data) 


# EVS & WVS -------------------------------------------------------------------#
# load and prepare data 
ewvs_data <- read.csv("Data/processed_data/evs_wvs17.csv") %>%
  select(-X) %>%
  na.omit(.)

# print descriptive mean and SD
print_descriptives(ewvs_data, ewvs_data$inv_quartile, 8)

# test if significant
summary(aov(attitude ~ as.factor(inv_quartile), data = ewvs_data)) 
leveneTest(attitude ~ as.factor(inv_quartile), data = ewvs_data) 
# is there a significant difference between highest and lowest inv regarding SD?
leveneTest(attitude ~ as.factor(inv_quartile), data = ewvs_data[ewvs_data$inv_quartile == 1 | ewvs_data$inv_quartile == 4,]) 

# ANES -----------------------------------------------------------------------#
anes_data <- read.csv("Data/processed_data/anes_data.csv") %>%
  select(-X) %>%
  na.omit(.)
# compute mean inv and split into quartiles
anes_data$inv2 <- rescale(anes_data$inv2, to = c(1, 5)) # transform inv2 to scale 1-5
anes_data$mean_involvement <- rowMeans(anes_data[, 1:2])
anes_data$inv_quartile <- split_involvement(anes_data$mean_involvement)
table(anes_data$inv_quartile)

print_descriptives(anes_data, anes_data$inv_quartile, 3)

summary(aov(attitude ~ as.factor(inv_quartile), data = anes_data)) 
leveneTest(attitude ~ as.factor(inv_quartile), data = anes_data) 


# Covid datasets -------------------------------------------------------------#
for(i in 1:6){
  path <- paste0("Data/processed_data/covid_T",i,".csv")
  data <- read.csv(path)
  
  descriptives <- print_descriptives(data, data$Vacc_Oth_6, 18)
  anova <- summary(aov(mean_attitude ~ as.factor(Vacc_Oth_6), data = data)) 
  levene <- leveneTest(mean_attitude ~ as.factor(Vacc_Oth_6), data = data)
  
  print(paste("Covid T",i))
  print(descriptives)
  print(paste("Mean difference significant:", (anova[[1]]$`Pr(>F)` < 0.01)[1], 
              "Diff in variance significant:", levene[1,3] < 0.01))
  print("----------")
}


# Eurobarometer --------------------------------------------------------------#
euro_data <- read.csv("Data/processed_data/eurobarometer_data.csv") %>%
  select(-X) %>%
  na.omit(.)

print_descriptives(euro_data, euro_data$involvement, 2)

summary(aov(attitude ~ as.factor(involvement), data = euro_data)) 
leveneTest(attitude ~ as.factor(involvement), data = euro_data) 

