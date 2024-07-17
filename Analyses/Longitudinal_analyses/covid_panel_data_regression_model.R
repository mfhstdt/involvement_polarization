# within-person fixed-effects panel data regression model on COVID-19 dataset #

library(dplyr)
library(plm)

# DATA PREPARATION -----------#

# merge data from six measurement waves
covid_merged <- data.frame()
covid_list <- list()
for(i in 1:6){
  path <- paste0("Data/processed_data/covid_T",i,".csv")
  covid_list[[i]] <- read.csv(path)
  covid_merged <- rbind(covid_merged, covid_list[[i]])
}

# only include ps that took part in all waves
covid_complete <- covid_merged %>%
  group_by(CaseID) %>% 
  filter(n_distinct(time) == 6) %>% # Filter IDs with exactly 6 unique time points
  ungroup() 

# recode time and inv (time 1 and inv 1 become reference group)
covid_complete$Vacc_Oth_6 <- covid_complete$Vacc_Oth_6 - 1
covid_complete$time <- covid_complete$time - 1

# compute extremity of attitude
covid_complete$attitude_extr <- abs(covid_complete$mean_attitude - 4)

# Fit Model ----------------
panel_lm <- plm(attitude_extr ~ Vacc_Oth_6, index=c("CaseID", "time"), data = covid_complete, 
                model="within", effect="twoways") #(twoways include also time fixed effects)
summary(panel_lm)






