############# CLEAN AND RECODE COVID VACCINATION ATTITUDE DATA #################
#######                             WAVE 5                               #######

library(foreign)
library(dplyr)

covid_T5 <- read.spss("Attitudes COVID-19 vaccins_T5_IDrecoded_v2.sav")

# variables of interest:
# - vaccines attitude 
#     - Vacc_Aff_2: "I am hopeful about COVID-19 vaccines"
#     - Vacc_Aff_4: "I have a good feeling about COVID-19 vaccines."
#     - Vacc_Aff_7: "People who do not want to get vaccinated against COVID-19 make me angry."
#     - Vacc_Cog_1: "COVID-19 vaccines protect well against COVID-19."
#     - Vacc_Cog_2: "To stop the pandemic, it is important that most people get vaccinated against COVID-19."
#     - Vacc_Cog_3: "COVID-19 vaccines are safe for one's health."
#     - Vacc_Cog_4: "The side effects of COVID-19 vaccines have been sufficiently studied."
#     - Vacc_Cog_5: "People without a COVID-19 vaccination should no longer be allowed everywhere."
#     - Vacc_Beh_2: "I encourage people to get vaccinated against COVID-19."
#     - Vacc_Beh_3: "I avoid people who do not get vaccinated against COVID-19."
#     - Vacc_Oth_5: "By getting vaccinated against COVID-19, I am protecting others from COVID-19."
# - vaccines involvement
#     - Vacc_Oth_6: "I know much about COVID-19 vaccines."
#     - Vacc_Oth_7: "I think COVID-19 vaccines is an important topic."
#     - Vacc_Oth_8: "I follow the news about COVID-19 vaccines."
#------------------------------------------------------------------------------

# derive variables, replace responses by numeric values 1-7 (expect for ID)
covid5 <- data.frame(
  CaseID = covid_T5$CaseID,
  time = rep(5, n= 1506),
  Vacc_Aff_2 = covid_T5$Vacc_Aff_2, 
  Vacc_Aff_4 = covid_T5$Vacc_Aff_4, 
  Vacc_Aff_7 = covid_T5$Vacc_Aff_7, 
  Vacc_Cog_1 = covid_T5$Vacc_Cog_1, 
  Vacc_Cog_2 = covid_T5$Vacc_Cog_2, 
  Vacc_Cog_3 = covid_T5$Vacc_Cog_3, 
  Vacc_Cog_4 = covid_T5$Vacc_Cog_4, 
  Vacc_Cog_5 = covid_T5$Vacc_Cog_5, 
  Vacc_Beh_2 = covid_T5$Vacc_Beh_2, 
  Vacc_Beh_3 = covid_T5$Vacc_Beh_3, 
  Vacc_Oth_5 = covid_T5$Vacc_Oth_5, 
  Vacc_Oth_6 = covid_T5$Vacc_Oth_6,
  Vacc_Oth_7 = covid_T5$Vacc_Oth_7,
  Vacc_Oth_8 = covid_T5$Vacc_Oth_8
)

for(col in 2:ncol(covid5)){
  covid5[, col] <- as.numeric(covid5[, col])
}

# compute average attitude 
covid5$mean_attitude <- covid5 %>%
  select(3:13) %>%
  rowSums(.)/11

# compute average involvement 
covid5$mean_involvement <- covid5 %>%
  select(14:16) %>%
  rowSums(.)/3


# save dataframe 
write.csv(covid5,
          "Data/processed_data/covid_T5.csv")





