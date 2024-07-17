############# CLEAN AND RECODE COVID VACCINATION ATTITUDE DATA #################
#######                             WAVE 2                              #######

library(foreign)
library(dplyr)


covid_T2 <- read.spss("Attitudes COVID-19 vaccins_T2_IDrecoded_v2.sav")

# OVERVIEW - items and their coding --------------------------------------------
#
# Potentioanl involvement items: 
#   Vacc_Oth_6: "I know much about COVID-19 vaccines." (1-7)
#   Vacc_Oth_7: "I think COVID-19 vaccines is an important topic." (1-7)
#   Vacc_Oth_8: "I follow the news about COVID-19 vaccines." (1-7)
#
# Attitude towards COVID-19 vaccines: 
#   Vacc_Aff_2: "I am hopeful about COVID-19 vaccines"
#   Vacc_Aff_4: "I have a good feeling about COVID-19 vaccines."
#   Vacc_Aff_7: "People who do not want to get vaccinated against COVID-19 make me angry."
#   Vacc_Cog_1: "COVID-19 vaccines protect well against COVID-19."
#   Vacc_Cog_2: "To stop the pandemic, it is important that most people get vaccinated against COVID-19."
#   Vacc_Cog_3: "COVID-19 vaccines are safe for one's health."
#   Vacc_Cog_4: "The side effects of COVID-19 vaccines have been sufficiently studied."
#   Vacc_Cog_5: "People without a COVID-19 vaccination should no longer be allowed everywhere."
#   Vacc_Beh_2: "I encourage people to get vaccinated against COVID-19."
#   Vacc_Beh_3: "I avoid people who do not get vaccinated against COVID-19."
#   Vacc_Oth_5: "By getting vaccinated against COVID-19, I am protecting others from COVID-19.
#------------------------------------------------------------------------------

# derive variables, replace responses by numeric values 1-7 (expect for ID)
covid2 <- data.frame(
  CaseID = covid_T2$CaseID,
  time = rep(2, n= 1505),
  Vacc_Aff_2 = covid_T2$Vacc_Aff_2, 
  Vacc_Aff_4 = covid_T2$Vacc_Aff_4, 
  Vacc_Aff_7 = covid_T2$Vacc_Aff_7, 
  Vacc_Cog_1 = covid_T2$Vacc_Cog_1, 
  Vacc_Cog_2 = covid_T2$Vacc_Cog_2, 
  Vacc_Cog_3 = covid_T2$Vacc_Cog_3, 
  Vacc_Cog_4 = covid_T2$Vacc_Cog_4, 
  Vacc_Cog_5 = covid_T2$Vacc_Cog_5, 
  Vacc_Beh_2 = covid_T2$Vacc_Beh_2, 
  Vacc_Beh_3 = covid_T2$Vacc_Beh_3, 
  Vacc_Oth_5 = covid_T2$Vacc_Oth_5, 
  Vacc_Oth_6 = covid_T2$Vacc_Oth_6,
  Vacc_Oth_7 = covid_T2$Vacc_Oth_7,
  Vacc_Oth_8 = covid_T2$Vacc_Oth_8
)

for(col in 2:ncol(covid2)){
  covid2[, col] <- as.numeric(covid2[, col])
}

# compute average attitude 
covid2$mean_attitude <- covid2 %>%
  select(3:13) %>%
  rowSums(.)/11

# compute mean involvement 
covid2$mean_involvement <- covid2 %>%
  select(14:16) %>%
  rowSums(.)/3

# save dataframe 
write.csv(covid2,
          "Data/processed_data/covid_T2.csv")
