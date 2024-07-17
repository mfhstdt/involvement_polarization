############# CLEAN AND RECODE JOINED EVS & WVS DATA 2017 - ####################

library(foreign)
evswvs17 <- read.spss("ZA7505_v4-0-0.sav")

# OVERVIEW - items and their coding --------------------------------------------
#
# Potential involvement items: 
#     Q29: "How interested would you say you are in politics?" 
#         > E023 in data;
#         > Note: recoded and coded "don't know" (8) and "no answer" (9) as NA
#         > 0.6% NA
#     Q30: "I’m going to read out some different forms of political action that people can take,
#         and I’d like you to tell me, for each one, whether you have actually done any of
#         these things, whether you might do it or would never, under any circumstances,
#         do it."
#         > E025 in data: "Signed a petition" 
#         > E026 in data: "Joining in boycotts" 
#         > E027 in data: "Attending lawful demonstrations"
#         > Note: recoded and made 8 & 9 NA
#
# General political attitude: 
#     Q31: "In political matters, people talk of ‘the left’ and ‘the right’. 
#       How would you place your views on this scale, generally speaking?" 
#         > E033 in data 
#         > Note: recoded and made "don't knwo" answers NA
#
# -----------------------------------------------------------------------

# Prepare data (include country variable) --------------
european_values17 <- read.spss("C:/Users/Michael/OneDrive - UvA/Thesis/Data_attention/European_Values_Study/ZA7505_v4-0-0.sav/ZA7505_v4-0-0.sav")
ewvs_data <- data.frame(  # get variables of interest
  study = european_values17$studytit,
  country = european_values17$cntry,
  ID = european_values17$uniqid,
  inv_interest = european_values17$E023,
  inv_boycott = european_values17$E026, 
  inv_petition = european_values17$E025,
  inv_demonstration = european_values17$E027,
  attitude = european_values17$E033
)
ewvs_data$inv_interest <- dplyr::recode(ewvs_data$inv_interest,  # recode
                                        "Not at all interested" = 1, 
                                        "Not very interested" = 2, 
                                        "Somewhat interested" = 3, 
                                        "Very interested" = 4)
ewvs_data$inv_boycott <- dplyr::recode(ewvs_data$inv_boycott,
                                       "Would never do" = 1, 
                                       "Might do" = 2, 
                                       "Have done" = 3)
ewvs_data$inv_petition <- dplyr::recode(ewvs_data$inv_petition,
                                        "Would never do" = 1, 
                                        "Might do" = 2, 
                                        "Have done" = 3)
ewvs_data$inv_demonstration <- dplyr::recode(ewvs_data$inv_demonstration,
                                             "Would never do" = 1, 
                                             "Might do" = 2, 
                                             "Have done" = 3)
ewvs_data$attitude <- dplyr::recode(ewvs_data$attitude,
                                    "Left" = 1,
                                    "Right" = 10, 
                                    "2" = 2, 
                                    "3" = 3, 
                                    "4" = 4, 
                                    "5" = 5, 
                                    "6" = 6, 
                                    "7" = 7, 
                                    "8" = 8, 
                                    "9" = 9)

# compute mean involvement 
ewvs_data$inv_boycott <- rescale(ewvs_data$inv_boycott, to = c(1, 4)) # unify scale
ewvs_data$inv_petition <- rescale(ewvs_data$inv_petition, to = c(1, 4)) 
ewvs_data$inv_demonstration <- rescale(ewvs_data$inv_demonstration, to = c(1, 4))
ewvs_data$mean_involvement <- ewvs_data %>% 
  select(4:7) %>%
  rowSums(.) / 4
ewvs_data <- na.omit(ewvs_data) # remove NAS

# compute involvement quartiles (within each country)
ewvs_countries <- ewvs_data %>% # split into separate lists per country
  mutate(inv_quartile = as.numeric(1)) %>% 
  mutate(mean_involvement = as.numeric(mean_involvement)) %>%
  group_split(country)
names(ewvs_countries) <- sort(unique(ewvs_data$country))
# Split each country into involvement quartiles
for(i in 1:length(ewvs_countries)){
  quartiles <- split_involvement(ewvs_countries[[i]]$mean_involvement)
  ewvs_countries[[i]]$inv_quartile <- quartiles
}
# combine again 
combined_data <- data.frame()
for(i in 1:length(ewvs_countries)){
  combined_data <- rbind(combined_data, ewvs_countries[[i]])
}

# save as csv 
write.csv(combined_data, "Data/processed_data/evs_wvs17.csv")
