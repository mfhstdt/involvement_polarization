############# CLEAN AND RECODE EUROBAROMETER 2022 DATA - ####################

library(dplyr)
library(foreign)

eurobarometer <- read.spss("ZA7886_v2-0-0.sav")

# OVERVIEW - items and their coding --------------------------------------------
# 
# Involvement in EU politics
#   d71.2: "When you are around friends or family would you say that you
#          often, sometimes, or never discuss European politics?
#       > Note: recoded and coded "don't know" as NA

# Attitude towards EU
#   d78: "In general, do you have a positive, rather positive, neutral, rather
#         negative, or negative image of the EU?"
#       > Note: recoded to a scale from -2 (very negative) to 2 (very positive)
#         and coded "don't know" as NA   
# ------------------------------------------------------------------------------

eurodata <- data.frame(
  involvement = eurobarometer$d71_2,
  attitude = eurobarometer$d78
)

# reverse code involvement 
eurodata$involvement <- dplyr::recode(eurodata$involvement,
                                      "Frequently" = 3, 
                                      "Occasionally" = 2,
                                      "Never" = 1)

# recode attitude
eurodata$attitude <- dplyr::recode(eurodata$attitude,
                                   "Very negative" = -2, 
                                   "Fairly negative" = -1, 
                                   "Neutral" = 0,
                                   "Fairly positive" = 1,
                                   "Very positive" = 2)


# save dataframe 
write.csv(eurodata, "Data/processed_data/eurobarometer_data.csv")
