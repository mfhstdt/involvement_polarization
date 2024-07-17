#############        CLEAN AND RECODE ANES 2020 DATA       ####################
#                     Pre-election data collection                            #

library(dplyr)
data <- read.csv("anes_timeseries_2020_csv_20220210.csv")


# OVERVIEW - items and their coding --------------------------------------------
#
# Potential involvement items: 
#   V201005: "How often do you pay attention to what’s going on in government and
#           politics?"
#     > Note: recoded and coded -9 as NA
#     > inv1
#   V201006: "Would you say that you have been [very much interested, somewhat 
#             interested, or not much interested] in the political campaign so 
#             far this year?
#     > Note: recoded 
#
# General political attitude: 
#   V202439:  "Where would you place yourself on this scale?" (0-Left; 10- right)
#     > all missing responses (values < 0) were code as NA  
# ------------------------------------------------------------------------------


anes <- data.frame(
  inv1 = data$V201005, 
  inv2 = data$V201006, 
  attitude = data$V202439
)

# code -9 as NA
anes$attitude <- ifelse(anes$attitude < 0, NA, anes$attitude)
anes$inv1 <- ifelse(anes$inv1 < 0, NA, anes$inv1)
anes$inv2 <- ifelse(anes$inv2 < 0, NA, anes$inv2)


# recode 
anes$inv1 <- dplyr::recode(anes$inv1,
                           "5" = 1, 
                           "4" = 2, 
                           "3" = 3, 
                           "2" = 4, 
                           "1" = 5) 

anes$inv2 <- dplyr::recode(anes$inv2, 
                           "3" = 1, 
                           "2" = 2, 
                           "1" = 3)


# save as csv
write.csv(anes, "Data/processed_data/anes_data.csv")
