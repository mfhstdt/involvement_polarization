####### CHECK POTENTIAL CONFOUNDED INVOLVEMENT ITEMS IN COVID-19 DATASET ####### 

library(dplyr)
source("Analyses/Crosssectional_analyses/Modality_detection/Analyse_modality_in_data/polarization_detection_algorithm.R")
covid_color = "#73ABD5"


# LOAD DATA (TIME POINT 1) #---------------------------------------------
covidT1 <- read.csv("Data/processed_data/covid_T1.csv") %>%
  select(-X)
par(mfrow =c(2,4))

# A: Check knowledge item ------------------------------------
for(j in 1:7){
  data <- covidT1$mean_attitude[covidT1$Vacc_Oth_6 == j]
  mod <- pol_modality(data)
  print(paste("COVID T1 - Knowledge level", j, "- Modality p value:", 
              mod[1], "Uniform:", mod[2], "Number of modes:", mod[[3]]))
  h <- hist(data, main = paste0("Covid T",i, " involvement ", j), 
            xlab = "Attitude towards Covid vaccines", col = covid_color, freq=FALSE,
            cex.main = 1.5, font.lab = 2, xaxt = "n",
            breaks = 0.5 + 0:7, ylim=c(0, 0.6), xlim=c(0.5,7.5))
  axis(1, at=1:7, labels=1:7)
  mtext(side = 3, line = -1, adj = 0.1, cex=1, paste("Number of modes:", mod[[3]]))
  mtext(side = 3, line = -2.5, adj = 0.1, cex=1, paste("N =", length(data)))
  lines(mod[[4]]$den_x, mod[[4]]$den_y, col="red", lwd=3)
} 

# B: check importance item ------------------------------------
for(j in 1:7){
  data <- covidT1$mean_attitude[covidT1$Vacc_Oth_7 == j]
  mod <- pol_modality(data)
  print(paste("Covid T1 - Importance level", j, "- Modality p value:", 
              mod[1], "Uniform:", mod[2], "Number of modes:", mod[[3]]))
  h <- hist(data, main = paste0("Covid T",i, " involvement ", j), 
            xlab = "Attitude towards Covid vaccines", col = covid_color, freq=FALSE,
            cex.main = 1.5, font.lab = 2, xaxt = "n",
            breaks = 0.5 + 0:7, ylim=c(0, 0.6), xlim=c(0.5,7.5))
  axis(1, at=1:7, labels=1:7)
  mtext(side = 3, line = -1, adj = 0.1, cex=1, paste("Number of modes:", mod[[3]]))
  mtext(side = 3, line = -2.5, adj = 0.1, cex=1, paste("N =", length(data)))
  lines(mod[[4]]$den_x, mod[[4]]$den_y, col="red", lwd=3)
} 

# C: check news item ---------------------------------------------
par(mfrow = c(2,4))
for(j in 1:7){
  data <- covidT1$mean_attitude[covidT1$Vacc_Oth_7 == j]
  mod <- pol_modality(data)
  print(paste("Covid T1 - News level", j, "- Modality p value:", 
              mod[1], "Uniform:", mod[2], "Number of modes:", mod[[3]]))
  h <- hist(data, main = paste0("Covid T",i, " involvement ", j), 
            xlab = "Attitude towards Covid vaccines", col = covid_color, freq=FALSE,
            cex.main = 1.5, font.lab = 2, xaxt = "n",
            breaks = 0.5 + 0:7, ylim=c(0, 0.6), xlim=c(0.5,7.5))
  axis(1, at=1:7, labels=1:7)
  mtext(side = 3, line = -1, adj = 0.1, cex=1, paste("Number of modes:", mod[[3]]))
  mtext(side = 3, line = -2.5, adj = 0.1, cex=1, paste("N =", length(data)))
  lines(mod[[4]]$den_x, mod[[4]]$den_y, col="red", lwd=3)
} 




