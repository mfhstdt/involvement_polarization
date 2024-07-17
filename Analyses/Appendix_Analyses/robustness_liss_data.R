# Testing the robustness of results when using different item combinations for 
################### involvement in the LISS data ###############################

library(dplyr)
library(scales)
library(car)
library(cusp)
source("helper_functions/split_involvement.R")
source("helper_functions/print_descriptives.R")
source("helper_functions/compute_mid_prop.R")
source("Analyses/Modality_detection/Analyse_modality_in_data/polarization_detection_algorithm.R")
liss_color = "#CAF8C3"

# Overview: -----------------------------------------------------------------------
# based on literature review, the following items from the LISS Panel were identified
# as potential proxies from involvement: 
# 4 cognitive items: 
#   cv22n008: "Are you very interested in the news, fairly interested or not 
#             interested?" (1-3) 
#   cv22n012: "Are you very interested in political topics, fairly interested or 
#             not interested?" (1-3)
#   cv23n006: Do you not follow the news or hardly follow the news? (1- Yes, 0-No)
#         > Note: recoded to make those who follow the news score 1 
#         > 6.17 % NAs
# 1 behavioral items: 
#   cv22n053: "Did you vote in the most recent parliamentary elections, held on
#             17 March 2021?" (1-2) 
# 5 binary items on participation: 
#   cv22n066 - cv22n071: "There are different ways of raising a political issue 
#             or of influencing politicians or government. Can you indicate which 
#             of the following ways you have exercised over the past five years?
#             (Yes/ No)"
#     cv22n066:	"by making use of a political party or organization"
#     cv22n067: "participated in a government-organized public hearing, discussion or citizens' participation meeting"
#     cv22n068:	"contacted a politician or civil servant"
#     cv22n069:	"participated in an action group"
#     cv22n070:	"participated in a protest action, protest march, or demonstration"

# CFA fit scores for a hierarchical 2-subfactor structure (cognitive and behavioral 
# involvenet loading on general involvement). We therefore only used the cognitive
# items for our analyses


#- A: -------------------------------------------------------------------------------
# Testing results when including behavioral items and participation items as a sum score

# prepare involvement variable 
liss_data <- read.csv("Data/processed_data/LISS14_2.csv") %>%
  select(-X) %>%
  na.omit(.)
liss_data$participation_sum <- liss_data %>% # compute participation sum score
  select(inv_beh_p1, inv_beh_p2, inv_beh_p3, inv_beh_p4, inv_beh_p5) %>%
  rowSums(.)
liss_data$inv_cognNews <- rescale(liss_data$inv_cognNews, to = c(0,4)) # rescale for mean
liss_data$inv_cogn2 <- rescale(liss_data$inv_cogn2, to = c(0,4))
liss_data$inv_cogn1 <- rescale(liss_data$inv_cogn1, to = c(0,4))
liss_data$inv_beh1 <- rescale(liss_data$inv_beh1, to = c(0,4))
liss_data$involvement <- liss_data %>%
  select(participation_sum, inv_beh1, inv_cogn1, inv_cogn2, inv_cognNews) %>%
  rowSums(.)/5
liss_data$inv_quartile <- split_involvement(liss_data$involvement)

# compute means and variances per inv level - not different from our results
print_descriptives(liss_data, liss_data$inv_quartile, 13)
summary(aov(attitude ~ as.factor(inv_quartile), data = liss_data)) 
leveneTest(attitude ~ as.factor(inv_quartile), data = liss_data) 


# compute median abs dev from midpoint: same pattern as original analysis
for(i in 1:4){
  data <- liss_data[liss_data$inv_quartile == i, 14]
  print(mad(data, center=5)) 
  print("__")
} 

# compute % choosing middle category: pattern & conclusions do not change
for(i in 1:4){
  print(paste("Involvement", i, "Proportion middle category:",
              round(mid_prop(liss_data$attitude[liss_data$inv_quartile == i]), 3)))
} 

# detect modality: all unimodal; same pattern as in our analyses
liss_inv <- liss_data %>%
  group_split(inv_quartile)
par(mfrow = c(1, 4))
for(i in 1:4){
  mod <- pol_modality(liss_inv[[i]]$attitude)
  print(paste("Involvement level", i, "- Modality p value:", 
              mod[1], "Uniform:", mod[2], "Number of modes:", mod[[3]]))
  h <- hist(liss_inv[[i]]$attitude, main = paste("LISS involvement", i), 
            cex.main = 2, font.lab = 2, xaxt = "n",
            xlab = "Attitude (1 left - 10 right)", col = liss_color,
            freq=FALSE, ylim=c(0,0.27),  breaks = 0.5 + -1:10, xlim=c(-1,11))
  axis(1, at=0:10, labels=0:10)
  mtext(side=3, line=-0.5, at=-0.07, adj=0, cex=1, paste("Number of modes:", mod[[3]]))
  mtext(side=3, line=-2, at=-0.07, adj=0, cex=1, paste("N =", nrow(liss_inv[[i]])))
  lines(mod[[4]]$den_x, mod[[4]]$den_y, col="red", lwd=2)
} 
par(mfrow = c(1, 1))

# fit cusp: beta becomes significant, no points in bifurcation and better fit than lm
cusp_liss2 <- cusp(y ~ attitude, alpha ~ 1, beta ~ involvement, data = liss_data)
summary(cusp_liss2) 
plot(cusp_liss2) 
lm_liss2 <- lm(abs(attitude - 5) ~ involvement, data = liss_data) 
BIC(lm_liss2) 

#- B: -------------------------------------------------------------------------------
# Testing results when including behavioral items; take participation items separately

# prepare involvement
liss_data <- read.csv("Data/processed_data/LISS14_2.csv") %>%
  select(-X) %>%
  na.omit(.)
liss_data$inv_cogn1 <- rescale(liss_data$inv_cogn1, to = c(1,4)) # rescale for mean
liss_data$inv_cogn2 <- rescale(liss_data$inv_cogn2, to = c(1,4))
liss_data$inv_cognNews <- rescale(liss_data$inv_cognNews, to = c(1,4))
liss_data$inv_beh1 <- rescale(liss_data$inv_beh1, to = c(1,4))
liss_data$inv_beh_p1 <- rescale(liss_data$inv_beh_p1, to = c(1,4))
liss_data$inv_beh_p2 <- rescale(liss_data$inv_beh_p2, to = c(1,4))
liss_data$inv_beh_p3 <- rescale(liss_data$inv_beh_p3, to = c(1,4))
liss_data$inv_beh_p4 <- rescale(liss_data$inv_beh_p4, to = c(1,4))
liss_data$inv_beh_p5 <- rescale(liss_data$inv_beh_p5, to = c(1,4))
liss_data$involvement <- liss_data %>% 
  select(inv_cogn1, inv_cogn2, inv_cognNews, inv_beh1, inv_beh_p1, 
         inv_beh_p2, inv_beh_p3, inv_beh_p4, inv_beh_p5) %>%
  rowSums(.)/10
liss_data$inv_quartile <- split_involvement(liss_data$involvement) # split into quartiles


# compute means and variances per inv level - attiude difference becomes significant
print_descriptives(liss_data, liss_data$inv_quartile, 14)
summary(aov(attitude ~ as.factor(inv_quartile), data = liss_data)) 
leveneTest(attitude ~ as.factor(inv_quartile), data = liss_data) 

# compute median abs dev from midpoint: no change
for(i in 1:4){
  data <- liss_data[liss_data$inv_quartile == i, 14]
  print(mad(data, center=5)) 
  print("__")
} 

# compute % choosing middle category: pattern & conclusions do not change
for(i in 1:4){
  print(paste("Involvement", i, "Proportion middle category:",
              round(mid_prop(liss_data$attitude[liss_data$inv_quartile == i]), 3)))
} 

# detect modality: all unimodal; same pattern as in our analyses
liss_inv <- liss_data %>%
  group_split(inv_quartile)
par(mfrow = c(1, 4))
for(i in 1:4){
  mod <- pol_modality(liss_inv[[i]]$attitude)
  print(paste("Involvement level", i, "- Modality p value:", 
              mod[1], "Uniform:", mod[2], "Number of modes:", mod[[3]]))
  h <- hist(liss_inv[[i]]$attitude, main = paste("LISS involvement", i), 
            cex.main = 2, font.lab = 2, xaxt = "n",
            xlab = "Attitude (1 left - 10 right)", col = liss_color,
            freq=FALSE, ylim=c(0,0.27),  breaks = 0.5 + -1:10, xlim=c(-1,11))
  axis(1, at=0:10, labels=0:10)
  mtext(side=3, line=-0.5, at=-0.07, adj=0, cex=1, paste("Number of modes:", mod[[3]]))
  mtext(side=3, line=-2, at=-0.07, adj=0, cex=1, paste("N =", nrow(liss_inv[[i]])))
  lines(mod[[4]]$den_x, mod[[4]]$den_y, col="red", lwd=2)
} 
par(mfrow = c(1, 1))

# fit cusp: beta becomes significant but no points in bifurcation and better fit than lm
cusp_liss3 <- cusp(y ~ attitude, alpha ~ 1, beta ~ involvement, data = liss_data)
summary(cusp_liss3) 
plot(cusp_liss3) 
lm_liss3 <- lm(abs(attitude - 5) ~ involvement, data = liss_data) 
BIC(lm_liss3) 







