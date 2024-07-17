################### FIT DATA TO CUSP & LINREG    ###############################

library(cusp)
library(dplyr)
library(scales)
source("helper_functions/split_involvement.R")

# For LISS 14 -----------------------------------------------------------------#
liss_data <- read.csv("Data/processed_data/LISS14_2.csv") %>%
  select(-X) %>%
  na.omit(.)

# fit cusp with involvement as splitting axis and constant normal axis
cusp_liss <- cusp(y ~ attitude, alpha ~ 1, beta ~ mean_involvement, data = liss_data)
summary(cusp_liss) 
plot(cusp_liss)

# with involvement also loading on normal axis
cusp_liss2 <- cusp(y ~ attitude, alpha ~ mean_involvement, beta ~ mean_involvement, data = liss_data)
summary(cusp_liss2) 
plot(cusp_liss2)

# compare to linear regression 
liss_data$attitude_centered <- abs(liss_data$attitude - 5) 
lm_liss<- lm(attitude_centered ~ mean_involvement, data = liss_data)
summary(lm_liss) 
AIC(lm_liss)
BIC(lm_liss) 


# American National Election Study --------------------------------------------#
anes_data <- read.csv("Data/processed_data/anes_data.csv") %>%
  select(-X) %>%
  na.omit(.)
# compute mean inv and split into quartiles
anes_data$inv2 <- rescale(anes_data$inv2, to = c(1, 5)) # transform inv2 to scale 1-5
anes_data$mean_involvement <- rowMeans(anes_data[, 1:2])

# split cusp with fixed normal axis 
cusp_anes <- cusp(y ~ attitude, alpha ~ 1, beta ~ mean_involvement, data = anes_data)
summary(cusp_anes) 
plot(cusp_anes)

# cusp with involvement on normal axis 
cusp_anes2 <- cusp(y ~ attitude, alpha ~ mean_involvement, beta ~ mean_involvement, data = anes_data)
summary(cusp_anes2) 
plot(cusp_anes2)

# compare to linear regression
anes_data$attitude_centered <- abs(anes_data$attitude - 5)
lm_anes <- lm(attitude_centered ~ mean_involvement, data = anes_data)
summary(lm_anes) 
AIC(lm_anes)
BIC(lm_anes) 


# European & World Value Study -------------------------------------------------#
ewvs_data <- read.csv("Data/processed_data/evs_wvs17.csv") %>%
  select(-X) %>%
  na.omit(.)

# cusp with fixed alpha 
cusp_ewvs <- cusp(y ~ attitude, alpha ~ 1, beta ~ mean_involvement, data = ewvs_data)
summary(cusp_ewvs)
plot(cusp_ewvs)

# cusp with involvement on alpha 
cusp_ewvs2 <- cusp(y ~ attitude, alpha ~ mean_involvement, beta ~ mean_involvement, data = ewvs_data)
summary(cusp_ewvs2)
plot(cusp_ewvs2)

# compare to linear regression 
ewvs_data$attitude_centered <- abs(ewvs_data$attitude - 5.5)
lm_ewvs <- lm(attitude_centered ~ mean_involvement, data = ewvs_data)
summary(lm_ewvs) 
AIC(lm_ewvs) 
BIC(lm_ewvs) 


# For Covid datasets ----------------------------------------------------------#
covid_cusps <- list()
for(i in 1:6){
  path <- paste0("Data/processed_data/covid_T",i,".csv")
  data <- read.csv(path)
  
  cusp_fit <- cusp(y ~ mean_attitude, alpha ~ 1, beta ~ Vacc_Oth_6,
                           data = data)
  #plot(cusp_fit)
  covid_cusps[[i]] <- summary(cusp_fit)
  
}


# compare to lm
covid_lms <- list()
for(i in 1:6){
  path <- paste0("Data/processed_data/covid_T",i,".csv")
  data <- read.csv(path)
  data$attitude_centered <- abs(data$mean_attitude - 3)
  
  lm_fit <- lm(attitude_centered ~ Vacc_Oth_6, data = data)
  covid_lms[[i]] <- lm_fit
}
# linear model fits: 
AIC(covid_lms[[1]]); BIC(covid_lms[[1]]) 
AIC(covid_lms[[2]]); BIC(covid_lms[[2]]) 
AIC(covid_lms[[3]]); BIC(covid_lms[[3]]) 
AIC(covid_lms[[4]]); BIC(covid_lms[[4]]) 
AIC(covid_lms[[5]]); BIC(covid_lms[[5]]) 
AIC(covid_lms[[6]]); BIC(covid_lms[[6]]) 





