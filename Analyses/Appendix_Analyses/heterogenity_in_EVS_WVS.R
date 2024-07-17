########## ANALYZE POLARIZATION MEASURES IN EVS/WVS PER COUNTRY ##################
#######################[ONLINE APPENDIX]#######################################

library(dplyr)
library(ggplot2)
library(foreign)
library(scales)
library(stats)
library(car)
library(lme4)
library(lmerTest)
source("helper_functions/split_involvement.R")
source("helper_functions/compute_mid_prop.R")
source("helper_functions/compute_median_AD.R")
source("helper_functions/ANAM.R")
source("Analyses/Modality_detection/Analyse_modality_in_data/polarization_detection_algorithm.R")

ewvs_color = "#FFE790"


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
ewvs_data$inv_boycott <- rescale(ewvs_data$inv_boycott, to = c(1, 4)) # unify scale
ewvs_data$inv_petition <- rescale(ewvs_data$inv_petition, to = c(1, 4)) 
ewvs_data$inv_demonstration <- rescale(ewvs_data$inv_demonstration, to = c(1, 4))
ewvs_data$mean_involvement <- ewvs_data %>% # compute mean involvement
  select(4:7) %>%
  rowSums(.) / 4
ewvs_data <- na.omit(ewvs_data) # remove NAs
ewvs_countries <- ewvs_data %>% # split into separate lists per country
  mutate(inv_quartile = as.numeric(1)) %>% 
  mutate(mean_involvement = as.numeric(mean_involvement)) %>%
  group_split(country)
names(ewvs_countries) <- sort(unique(ewvs_data$country))

# Split each country into involvement quartiles ------------
for(i in 1:length(ewvs_countries)){
  quartiles <- split_involvement(ewvs_countries[[i]]$mean_involvement)
  ewvs_countries[[i]]$inv_quartile <- quartiles
}

############################################################################
# For each country compute for lowest and highest involvement respectively: 
# - modality 
# - med. abs. dev. from midpoint
# - % choosing middle categories
# - SD
# - whether differences in SD are significant
#---------------------------------------------------------------------------
country_measures <- data.frame(
  country = names(ewvs_countries),
  modality_low_inv = NA, 
  modality_high_inv = NA, 
  mdn_low_inv = NA, 
  mdn_high_inv = NA, 
  percentage_moderate_low = NA, 
  percentage_moderate_high = NA, 
  SD_low_inv = NA, 
  SD_high_inv = NA, 
  SD_significant = NA
)

for(i in 1:length(ewvs_countries)){
  attitude_low <- ewvs_countries[[i]]$attitude[ewvs_countries[[i]]$inv_quartile == 1]
  attitude_high <- ewvs_countries[[i]]$attitude[ewvs_countries[[i]]$inv_quartile == 4]
  
  country_measures$modality_low_inv[i] <- pol_modality(attitude_low)$modality
  country_measures$modality_high_inv[i] <- pol_modality(attitude_high)$modality
  country_measures$mdn_low_inv[i] <- median_AD(attitude_low)[[2]]
  country_measures$mdn_high_inv[i] <- median_AD(attitude_high)[[2]]
  country_measures$percentage_moderate_low[i] <- round(mid_prop(attitude_low), 3)
  country_measures$percentage_moderate_high[i] <- round(mid_prop(attitude_high), 3)
  country_measures$SD_low_inv[i] <- round(sd(attitude_low),3)
  country_measures$SD_high_inv[i] <- round(sd(attitude_high),3)
  levene <- leveneTest(attitude ~ as.factor(inv_quartile), data = ewvs_countries[[i]])
  country_measures$SD_significant[i] <- (levene$`Pr(>F)` < 0.01)[1]
}


# MODALITY ----------------------------------------------------------------------
# frequencies of modality at high involvement
table(country_measures$modality_high_inv)
#  1  2  3 
# 58  5 16 

# frequencies of modality at low involvement
table(country_measures$modality_low_inv)
# 1  2  3  4 
# 36 15 25  3 

# visualize the frequencies of these combinations
country_measures %>%
  count(modality_low_inv, modality_high_inv) %>%
  ggplot(aes(x=modality_low_inv, y=modality_high_inv, size = n)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  geom_point(alpha=0.5, color = "blue") + 
  scale_size(range = c(.1, 24), name="Frequency") +
  xlab("Modality at low involvement") + 
  ylab("Modality at high involvement") +
  ylim(0.7,4) +
  xlim(0.7,4) +
  annotate("text", x = 3.2, y = 3.2, label = "No difference in modality", 
           angle = 43, vjust = -0.5, hjust = 0, size = 4.5) +
  annotate("text", x = 2, y = 4, label = "Higher modality at high involvement", 
           vjust = -0.5, hjust = 0, color = "darkgrey", size = 5, fontface = "bold") +
  annotate("text", x = 4, y = 2, label = "Higher modality at low involvement", 
           angle = 90, vjust = -0.5, hjust = 0, color = "darkgrey", size = 5, fontface = "bold") +
  annotate("rect", xmin = 0.7, xmax = 1.3, ymin = 1.7, ymax = 2.3, fill = "lightgrey", alpha = 0.5) +
  annotate("text", x = 1, y = 2, label = "Prediction of Cusp", size = 3.5, fontface = "bold") +
 # annotate("rect", xmin = 0.7, xmax = 1.3, ymin = 2.7, ymax = 3.3, fill = "lightgrey", alpha = 0.5) +
 # annotate("text", x = 1, y = 3, label = "Prediction tricritical model", size = 3) +
  theme_classic() + 
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)), 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.background = element_rect(color = "lightgrey", size = 1)) 

# For how many countries does modality not differ between high and low inv?-> 52
sum(country_measures$modality_low_inv == country_measures$modality_high_inv)
# For how many countries do we see more modes at low inv? -> 26
sum(country_measures$modality_low_inv > country_measures$modality_high_inv)

# countries with trimodal distribution at low involvement: 25 (31%)
country_measures$country[which(country_measures$modality_low_inv == 3)] 

# countries for which SD INcreases significantly (31 out of 79 =~ 39%)
country_measures$country[which(country_measures$SD_significant == TRUE & 
                                 (country_measures$SD_low_inv < country_measures$SD_high_inv))]
# countries for which SD DEcreases significantly (6 out of 79 =~ 7.6%)
country_measures$country[which(country_measures$SD_significant == TRUE & 
                                 (country_measures$SD_low_inv > country_measures$SD_high_inv))]
# thus: no change for 46.8 %


# Changes in % choosing the extremes ------------------#
df_extremes <- data.frame()

for(i in 1:length(ewvs_countries)){
  data <- ewvs_countries[[i]]
  data_inv <- data %>%
    na.omit(.) %>%
    group_split(inv_quartile)
  for(j in 1:length(data_inv)){
    percentage_extreme = (sum(data_inv[[j]]$attitude == 1) + sum(data_inv[[j]]$attitude == 10)) / nrow(data_inv[[j]])
    df <- data.frame(
      country = names(ewvs_countries)[i],
      quartile = j, 
      perc_extreme = percentage_extreme
    )
    df_extremes <- rbind(df_extremes, df)
  }
}

# plot
ggplot(df_extremes, aes(x=quartile, y=perc_extreme, color = country)) +
  geom_line()

# how often is the lowest inv. quartiles more extreme than highest? -> 28 countries
df_extremes %>%
  filter(quartile %in% c(1, 4)) %>%
  group_by(country) %>%
  summarise(difference = perc_extreme[quartile == 1] - perc_extreme[quartile == 4]) %>%
  summarise(count = sum(difference > 0))

# how often is the highest inv. quatiles more extreme than the lowest? -> 51 countries
df_extremes %>%
  filter(quartile %in% c(1, 4)) %>%
  group_by(country) %>%
  summarise(difference = perc_extreme[quartile == 4] - perc_extreme[quartile == 1]) %>%
  summarise(count = sum(difference > 0))

highest_quartile_per_country <- df_extremes %>%
  group_by(country) %>%
  summarise(highest_group = quartile[which.max(perc_extreme)])
table(highest_quartile_per_country$highest_group)
# 1  2  3  4 
#22 10  8 39



# MEASURES OF SPREAD & DISPERSION-----------------------------------------------------
# countries for which SD INcreases significantly (6 out of 79 =~ 7.5%) 
country_measures$country[which(country_measures$SD_significant == TRUE & 
                                 (country_measures$SD_low_inv > country_measures$SD_high_inv))]
# sign. decrease in SDs for 31 countries (~39.2%) -> i.e., no change for 46.5%
country_measures$country[which(country_measures$SD_significant == TRUE & 
                                 (country_measures$SD_low_inv < country_measures$SD_high_inv))]

# countries for which percentage of moderate attitudes decreases (72 / 79 =~ 91%)
country_measures$country[which(country_measures$percentage_moderate_low > country_measures$percentage_moderate_high)]


# MDN abs. dev. stays constant for all countries, just like when aggregated
country_measures$country[which(country_measures$mdn_low_inv == country_measures$mdn_low_inv)]

