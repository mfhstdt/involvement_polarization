########### CONFIRMATORY FACTOR ANALYSES FOR INVOLVEMENT FACTORS ##############

library(lavaan)
library(dplyr)
library(psych)
library(scales)
library(paran)

# LISS Panel --------------------------------------------------------------------#
LISS14 <- read.csv("Data/processed_data/LISS14_2.csv") %>%
  select(-X) 

# correlation plot
LISS14_inv <- LISS14[, 1:9] %>%
  na.omit(.)
cor.plot(cor(LISS14_inv, use="pairwise.complete.obs"))

# specify hierarchical 2-factor model
model_liss <- '
  # Factor loadings
    cognitive =~ inv_cogn1 + inv_cogn2 + inv_cognNews
    behavioral =~ inv_beh1 + inv_beh2
  
  # Subfactors loading on the overarching factor
  f =~ cognitive + behavioral 
'

# fit to data 
fit_liss <- cfa(model_liss, data = LISS14, estimator = "MLR")
summary(fit_liss, fit.measures = TRUE, standardized = TRUE)
# RMSEA = .096
# CFI = .972

# EFA LISS - only cognitive items ---------------------------#
LISS14_cogn <- LISS14[,1:3]

# create training and test set
set.seed(1)
LISS14_cogn <- na.omit(LISS14_cogn)
LISS14_cogn$nrow <- rep(1:nrow(LISS14_cogn))
training_set <- LISS14_cogn[sample(nrow(LISS14_cogn), 1406), ]
vec <- training_set$nrow
test_set <- LISS14_cogn[!(LISS14_cogn$nrow %in% vec ) , ]
training_set <- training_set[,-4] # remove nrow column
test_set <- test_set[,-4] 

# assumption checks 
cortest.bartlett(training_set, n = nrow(evs)) # (sphericity) significant
KMO(training_set) # Kaiser-Meyer-Olkin > .70

# parallel analysis suggests 2 factors
paran(training_set, cfa=TRUE, graph=TRUE)

# run EFA with oblique rotation (factors assumed to correlate)
efa <- fa(training_set, nfactors = 2, rotate = "oblimin")
fa.diagram(efa)

# validate EFA structure on test data (CFA) -----------------#
model_liss_cogn <- '
  # Factor loadings
    cognitive_inform =~ inv_cognNews + inv_cogn1 + inv_cogn2
' 

fit_liss_cogn <- cfa(model_liss_cogn, data = training_set, estimator = "MLR")
summary(fit_liss_cogn, fit.measures = TRUE, standardized = TRUE)
# RMSEA = 0
# CFI = 1

# thus: combine cognitive involvement items into involvement mean score
LISS14$inv_cognNews <- rescale(LISS14$inv_cognNews, to = c(1,3)) 
LISS14$mean_involvement <- LISS14 %>%  # compute mean cogn. involvement
  select(starts_with("inv_cogn")) %>%
  rowSums(.)/3

# save 
#write.csv(LISS14, "Data/processed_data/LISS14_2.csv")


# European & World Value Study ------------------------------------------------#
# load involvement variables 
ewvs <- read.csv("Data/processed_data/evs_wvs17.csv") %>%
  select(-X)

# correlation plot
ewvs_inv <- ewvs %>%
  select(starts_with("inv"))
cor.plot(cor(ewvs_inv, use="pairwise.complete.obs"))

# proportion missing: 12.44%
sum(is.na(ewvs_inv))/ nrow(ewvs_inv) 

# specify model 
model_ewvs <- '
  # Factor loadings
    cognitive =~ inv_interest
    behavioral =~ inv_boycott + inv_petition + inv_demonstration
  
  # Subfactors loading on the overarching factor
  overall =~ cognitive + behavioral 
' 

fit_ewvs <- cfa(model_ewvs, data = ewvs_inv, estimator = "MLR")

summary(fit_ewvs, fit.measures = TRUE, standardized = TRUE)
# RMSEA = 0.033
# CFI = 0.99

# BIC = 1226565.995



# American National Election Study --------------------------------------------#
# Note: CFA not possible with only 2 items, therefore testing if cor is significant
anes <- read.csv("Data/processed_data/anes_data.csv") %>%
  select(-X)

# proportion missing: 0.02%
sum(is.na(anes))/ nrow(anes) 


# test if correlation is significant (Kendall's tau) ----
cor.test(anes$inv1, anes$inv2, method= "kendall") # tau = .55; significant (p < .001)
# items will be combined as means after re-scaling both items to same scale range


# CFA on covid data was reported by Chambon et al., 2020 ----------------------#
# Examplary results for T1: 
covid <- read.csv("Data/processed_data/covid_T1.csv") 

# correlation matrix
cor.plot(cor(covid, use="pairwise.complete.obs"))

# No missing data 
sum(is.na(covid))

# specify model
model_covid <- '
  # Factor loadings
    involvement =~ Vacc_Oth_6 + Vacc_Oth_7 + Vacc_Oth_8
' 

# run CFA
covid_fit <- cfa(model_covid, data = covid, estimator = "MLR")

# inspect results
summary(covid_fit, fit.measures = TRUE, standardized = TRUE)
# RMSEA = 0
# CFI = 1































