############# CLEAN AND RECODE LISS CORE STUDY DATA- ##########################
#######      POLITICS AND VALUES QUESTIONNAIRE - WAVE 14                #######

library(dplyr)
wave14 <- read.csv2("LISS14_EN_1.0p.csv")


# OVERVIEW - items and their coding --------------------------------------------

# Potential involvement items:
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

#   cv22n053: "Did you vote in the most recent parliamentary elections, held on
#             17 March 2021?" (1-2) 
#         > Note: excluded 3 (not eligible to vote) and -9 (I don't know)
#                 and recoded "did not vote" as 0 while "did vote" remains 1
#         > 11.1 % NAs
#   cv22n066 - cv22n071: "There are different ways of raising a political issue 
#             or of influencing politicians or government. Can you indicate which 
#             of the following ways you have exercised over the past five years?
#             (Yes/ No)"
#         > Note: excluded "don't know" (-9) and computed sum score
#     cv22n066:	"by making use of a political party or organization"
#     cv22n067: "participated in a government-organized public hearing, discussion or citizens' participation meeting"
#     cv22n068:	"contacted a politician or civil servant"
#     cv22n069:	"participated in an action group"
#     cv22n070:	"participated in a protest action, protest march, or demonstration"

# Engagement in discourse:
#   cv22n010: "If you are in the company of other people and the conversation
#             turns to national news, do you usually participate in the conversation, 
#             listen with interest, do not listen or do not have any interest?"
#         > Note: recoded 
#         > 5.1% NA
#   cv22n071: "Over the past 5 years, have you participated in a political 
#             discussion or campaign on the Internet, by e-mail or SMS?" (Yes/No)
#         > Note: excluded "don't know" (-9)
#         > 9.5% NA

# General political attitude
#   cv22n101: "In politics, a distinction is often made between "the left" and 
#             'the right'. Where would you place yourself on the scale below, 
#             where 0 means left and 10 means right?"
#         > Note: excluded "don't know" (-9)
#         > 17.9% NA

# -------------------------------------------------------------------------------

# select variables ---
LISS14 <- wave14 %>%
  select(cv22n008, cv22n012, cv22n006, cv22n053, cv22n066, cv22n067,
         cv22n068, cv22n069, cv22n070, cv22n071,  cv22n010, cv22n071,
         cv22n101)

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

# cv22n053: make "don't know" answers NA & exclude non-eligible voters --
LISS14$cv22n053 <- ifelse(LISS14$cv22n053 == -9 | LISS14$cv22n053 == 3, NA, 
                          ifelse(LISS14$cv22n053 == 2, 0, LISS14$cv22n053)) 

# create participation sum score --
participation_df <- LISS14 %>%
  select(6:10)  

for(i in 1:ncol(participation_df)){   # remove -9
  participation_df[, i] <- ifelse(participation_df[, i] == -9, NA, participation_df[, i])
}
LISS14$inv_beh2 <- rowSums(participation_df)  

# recode cv22n010 --
LISS14$cv22n010 <- ifelse(LISS14$cv22n010 == 1, 3,
                          ifelse(LISS14$cv22n010 == 3, 1,
                                 LISS14$cv22n010))

# exclude "don't know" responses  ---
LISS14$cv22n071 <- ifelse(LISS14$cv22n071 == -9, NA, LISS14$cv22n071)
LISS14$cv22n101 <- ifelse(LISS14$cv22n101 == -9, NA, LISS14$cv22n101)
LISS14$cv22n006 <- ifelse(LISS14$cv22n006 == 10, NA, LISS14$cv22n006)

# keep and rename relevant columns ---
# NOTE: inv_beh2 is sum of participation items
LISS14 <- LISS14 %>%
  select(cv22n008, cv22n012, cv22n006, cv22n053, cv22n066, 
         cv22n067, cv22n068, cv22n069, cv22n070, inv_beh2,   
         cv22n010, cv22n071, cv22n101) %>%
  rename(inv_cogn1 = cv22n008, 
         inv_cogn2 = cv22n012,
         inv_cognNews = cv22n006, 
         inv_beh1 = cv22n053,
         inv_beh_p1 = cv22n066, 
         inv_beh_p2 = cv22n067,
         inv_beh_p3 = cv22n068,
         inv_beh_p4 = cv22n069,
         inv_beh_p5 = cv22n070,
         discourse1 = cv22n010,
         discourse2 = cv22n071,
         attitude = cv22n101)

write.csv(LISS14, "Data/processed_data/LISS14_2.csv")
