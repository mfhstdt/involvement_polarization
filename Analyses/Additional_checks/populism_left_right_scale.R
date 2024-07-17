# Test if those voting PVV self-report as politically right in LISS panel

library(dplyr)
library(scales)
library(ggplot2)
source("helper_functions/split_involvement.R")

# relevant variables ----------------------------------------------------------#

# Involvement
#   cv22n008: "Are you very interested in the news, fairly interested or not 
#             interested?" (1-3) 
#         > Note: item was recoded & "don't know" answers (-9) replaced by NA
#         > 6.3 % NAs
#   cv22n012: "Are you very interested in political topics, fairly interested or 
#             not interested?" (1-3)
#         > Note: recoded 
#         > 5.2 % NAs
#   cv22n006: Do you not follow the news or hardly follow the news? (1- Yes, 0-No)
#         > Note: recoded to make those who follow the news score 1 
#         > 5.4 % NAs

# General political attitude
#   cv22n101: "In politics, a distinction is often made between "the left" and 
#             'the right'. Where would you place yourself on the scale below, 
#             where 0 means left and 10 means right?"
#         > Note: excluded "don't know" (-9)
#         > 17.9% NA

# Party preference
#   cv22n307: "For which party did you vote in the parliamentary elections of 
#               17 March 2021?" 
#   cv22n213: "What do you think of the PVV?" (0- very unsympathic - 10 very sympathic)

# Attitude towards immigration
#  cv22n120: "There are too many people of foreign origin or descent in the Netherlands."
#              1 fully disagree - 5 fully agree (3 is neither agree nor disagree)

# Media: 
#  cv22n021: "On a scale from 0-10 how much confidence do you have in the media?


wave14 <- read.csv2("Data/raw_data/LISS14_EN_1.0p.csv")

# select variables ---
LISS14 <- wave14 %>%
  select(cv22n008,  cv22n012,  cv22n006, cv22n101, cv22n307, cv22n308, cv22n213, cv22n120, cv22n021)

# recode cv22n008 and make "don't know" answers NA --
LISS14$cv22n008 <- ifelse(LISS14$cv22n008 == 1, 3,
                          ifelse(LISS14$cv22n008 == 3, 1,
                                 ifelse(LISS14$cv22n008 == -9, NA, 
                                        LISS14$cv22n008)))
# recode cv22n012 --
LISS14$cv22n012 <- ifelse(LISS14$cv22n012 == 1, 3,
                          ifelse(LISS14$cv22n012 == 3, 1, 
                                 LISS14$cv22n012))

# recode cv22n006 --
LISS14$cv22n006 <- abs(LISS14$cv22n006 - 1)

# exclude "don't know" responses  ---
LISS14$cv22n101 <- ifelse(LISS14$cv22n101 == -9, NA, LISS14$cv22n101)
LISS14$cv22n213 <- ifelse(LISS14$cv22n213 == -9, NA, LISS14$cv22n213)
LISS14$cv22n308 <- ifelse((LISS14$cv22n308 == -9 | LISS14$cv22n308 == -8), NA, LISS14$cv22n308)
LISS14$cv22n307 <- ifelse((LISS14$cv22n307 == -9 | LISS14$cv22n307 == -8), NA, LISS14$cv22n307)
LISS14$cv22n006 <- ifelse(LISS14$cv22n006 == 10, NA, LISS14$cv22n006)
LISS14$cv22n021 <- ifelse(LISS14$cv22n021 == -9, NA, LISS14$cv22n021)

#  code responses to voting questions 
LISS14 <- LISS14 %>%
  mutate(would_vote = case_when(cv22n308 == 1 ~ "no vote", 
                                cv22n308 == 2 ~ "VVD", 
                                cv22n308 == 3 ~ "PVV",
                                cv22n308 == 4 ~ "CDA", 
                                cv22n308 == 5 ~ "D66", 
                                cv22n308 == 6 ~ "GroenLinks", 
                                cv22n308 == 7 ~ "SP", 
                                cv22n308 == 8 ~ "PvdA", 
                                cv22n308 == 9 ~ "ChristenUnie", 
                                cv22n308 == 10 ~ "Partij voor de Dieren", 
                                cv22n308 == 11 ~ "50 PLUS", 
                                cv22n308 == 12 ~ "SGP", 
                                cv22n308 == 13 ~ "DENK", 
                                cv22n308 == 14 ~ "Forum voor Democratie", 
                                cv22n308 == 15 ~ "Blank", 
                                cv22n308 == 16 ~ "Other", 
                                cv22n308 == 17 ~ "Volt", 
                                cv22n308 == 18 ~ "JA21", 
                                cv22n308 == 19 ~ "BBB", 
                                cv22n308 == 20 ~ "BIJ1"), 
         party_voted  = case_when(cv22n307 == 1 ~ "VVD", 
                                  cv22n307 == 2 ~ "PVV", 
                                  cv22n307 == 3 ~ "CDA",
                                  cv22n307 == 4 ~ "D66", 
                                  cv22n307 == 5 ~ "GroenLinks", 
                                  cv22n307 == 6 ~ "SP", 
                                  cv22n307 == 7 ~ "PvdA", 
                                  cv22n307 == 8 ~ "CU", 
                                  cv22n307 == 9 ~ "Partij voor de Dieren", 
                                  cv22n307 == 10 ~ "50Plus", 
                                  cv22n307 == 11 ~ "SGP", 
                                  cv22n307 == 12 ~ "DENK", 
                                  cv22n307 == 13 ~ "Forum voor Democratie", 
                                  cv22n307 == 14 ~ "Blank", 
                                  cv22n307 == 15 ~ "Other", 
                                  cv22n307 == 16 ~ "Volt", 
                                  cv22n307 == 17 ~ "JA21", 
                                  cv22n307 == 18 ~ "BBB", 
                                  cv22n307 == 19 ~ "BIJ1")
  )

LISS14$party_voted <- as.factor(LISS14$party_voted)
LISS14$would_vote <- as.factor(LISS14$would_vote)

# create mean involvement variable 
LISS14$cv22n006 <- rescale(LISS14$cv22n006, to = c(1,3)) 
LISS14$mean_involvement <- LISS14 %>%  
  select(cv22n006, cv22n008, cv22n012) %>%
  rowSums(.)/3

# split into quartiles 
LISS14 <- LISS14 %>% filter(!is.na(mean_involvement))
LISS14$inv_quartile <- split_involvement(LISS14$mean_involvement)



# SELF_IDENTIFICATION OF POPULIST VOTERS -------------------------------------#
# How do PVV voters identify? 
hist(LISS14$cv22n101[LISS14$party_voted == "PVV"], xlab = "Political Orientation",
     breaks = 0.5 + -1:10, xlim=c(-0.5,11), xaxt = "n",
     main = "Political Orientation of PVV voters 2021")
axis(1, at=0:10, labels=0:10)

# How do BBB voters identify? 
hist(LISS14$cv22n101[LISS14$party_voted == "BBB"], xlab = "Political Orientation",
     breaks = 0.5 + -1:10, xlim=c(-0.5,11), xaxt = "n",
     main = "Political Orientation of BBB voters 2021")
axis(1, at=0:10, labels=0:10)

# How do potential PVV voters identify? 
hist(LISS14$cv22n101[LISS14$would_vote == "PVV"], xlab = "Political Orientation",
     breaks = 0.5 + -1:10, xlim=c(-0.5,11), xaxt = "n",
     main = "Political Orientation of potential PVV voters")
axis(1, at=0:10, labels=0:10)

# How do potential BBB voters identify?
hist(LISS14$cv22n101[LISS14$would_vote == "BBB"], xlab = "Political Orientation",
     breaks = 0.5 + -1:10, xlim=c(-0.5,11), xaxt = "n",
     main = "Political Orientation of potential BBB voters")
axis(1, at=0:10, labels=0:10)


