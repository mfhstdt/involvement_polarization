###### Determine modality of attitude distribution in large-scale datasets #####

library(dplyr)
library(scales)
source("Analyses/Modality_detection/Analyse_modality_in_data/polarization_detection_algorithm.R")
source("helper_functions/split_involvement.R")

# colors for plotting
liss_color = "#CAF8C3"
anes_color = "#55DDE0"
ewvs_color = "#FFE790"
covid_color = "#73ABD5"
euro_color = "#FFEAEE"

# LISS Panel ------------------------------------------------------------------#
liss_data <- read.csv("Data/processed_data/LISS14_2.csv") %>%
  select(-X) %>%
  na.omit(.)
liss_data$inv_quartile <- split_involvement(liss_data$mean_involvement)
table(liss_data$inv_quartile)

# do we see polarization when splitting by involvement levels?
liss_inv <- liss_data %>%
  group_split(inv_quartile)
par(mfrow = c(1, 4))
for(i in 1:4){
  mod <- pol_modality(liss_inv[[i]]$attitude)
  print(paste("Involvement level", i, "- Modality p value:", 
              mod[1], "Uniform:", mod[2], "Number of modes:", mod[[3]]))
  h <- hist(liss_inv[[i]]$attitude, main = paste("LISS involvement", i), freq = FALSE,
            xlab = "Political attitude (0 left - 10 right)", font.lab = 2, cex.lab = 1.5,
            ylab = "Density", font.lab = 2, cex.lab = 1.5, cex.main = 2, ylim = c(0, 0.25),
            col = liss_color, xaxt = "n", yaxt = "n")
  axis(1, at=0:10, labels=0:10, cex.axis = 1.5, cex.lab = 2)
  axis(2, cex.axis = 1.5, cex.lab = 2)
  mtext(side=3, line=-0.5, at=-0.07, adj=0, cex=1, paste("Number of modes:", mod[[3]]))
  mtext(side=3, line=-2, at=-0.07, adj=0, cex=1, paste("N =", nrow(liss_inv[[i]])))
  lines(mod[[4]]$den_x, mod[[4]]$den_y, col="red", lwd=3)
} # not uniform and unimodal for all levels --> no polarization
par(mfrow = c(1, 1))


# ANES -----------------------------------------------------------------------#

# load data and split sample into involvement quartiles
anes_data <- read.csv("Data/processed_data/anes_data.csv") %>%
  select(-X) %>%
  na.omit(.)
anes_data$inv2 <- rescale(anes_data$inv2, to = c(1, 5)) # transform inv2 to scale 1-5
anes_data$mean_involvement <- rowMeans(anes_data[, 1:2])
anes_data$inv_quartile <- split_involvement(anes_data$mean_involvement)
table(anes_data$inv_quartile)

# polarization in different involvement groups? 
anes_inv <- anes_data %>%
  group_split(inv_quartile)
par(mfrow = c(1,4))
for(i in 1:4){
  mod <- pol_modality(anes_inv[[i]]$attitude)
  print(paste("Involvement level", i, "- Modality p value:", 
              mod[1], "Uniform:", mod[2], "Number of modes:", mod[[3]]))
  h <- hist(anes_inv[[i]]$attitude, main = paste("ANES involvement", i),
            xlab = "Political attitude (0 left - 10 right)", font.lab = 2, cex.lab = 1.5,
            ylab = "Density", font.lab = 2, cex.lab = 1.5, cex.main = 2, ylim = c(0, 0.38),
            col = anes_color, xaxt = "n", yaxt = "n", freq = FALSE)
  axis(1, at=0:10, labels=0:10, cex.axis = 1.5, cex.lab = 2)
  axis(2, cex.axis = 1.5, cex.lab = 2)
  mtext(side=3, line=-0.5, at=-0.07, adj=0, cex=1, paste("Number of modes:", mod[[3]]))
  mtext(side=3, line=-2, at=-0.07, adj=0, cex=1, paste("N =", nrow(anes_inv[[i]])))
  lines(mod[[4]]$den_x, mod[[4]]$den_y, col="red", lwd=2)
} # modes: 3,1,1,2
par(mfrow = c(1, 1))


# EVS/ WVS -----------------------------------------------------------------------#

# load and prepare data 
ewvs_data <- read.csv("Data/processed_data/evs_wvs17.csv") %>%
  select(-X) %>%
  na.omit(.)
table(ewvs_data$inv_quartile)

# polarization in different involvement groups? 
ewvs_inv <- ewvs_data %>%
  group_split(inv_quartile)
par(mfrow = c(1, 4))
for(i in 1:4){
  mod <- robust_density(ewvs_inv[[i]]$attitude)
  print(paste("Involvement level", i, "- Modality p value:", 
              mod[1], "Uniform:", mod[2], "Number of modes:", mod[[3]]))
  h <- hist(ewvs_inv[[i]]$attitude, main = paste("EWVS involvement", i), 
            xlab = "Attitude (1 left - 10 right)", font.lab = 2, cex.lab = 1.5,
            breaks = 0.5 + 0:10, xlim=c(0.5,10.5),
            ylab = "Density", font.lab = 2, cex.lab = 1.5, cex.main = 2, ylim = c(0, 0.4),
            col = ewvs_color, xaxt = "n", yaxt = "n", freq = FALSE)
  axis(1, at=1:10, labels=1:10, cex.axis = 1.5, cex.lab = 2)
  axis(2, cex.axis = 1.5, cex.lab = 2)
  mtext(side=3, line=-0.5, at=02, adj=0, cex=1, paste("Number of modes:", mod[[1]]))
  mtext(side=3, line=-2, at=0.2, adj=0, cex=1, paste("N =", nrow(ewvs_inv[[i]])))
  lines(mod[[2]]$den_x, mod[[2]]$den_y, col="red", lwd=3)
} 
par(mfrow = c(1, 1))

# Eurobarometer ----------------------------------------------------------------#
euro_data <- read.csv("Data/processed_data/eurobarometer_data.csv") %>%
  select(-X) %>%
  na.omit(.)
 
# general polaization in sample? 
pol_euro <- pol_modality(euro_data$attitude) # unimodal 
h <- hist(euro_data$attitude, main = "Attitude towards EU (N=26,187)", 
          cex.main = 1.5, font.lab = 2, 
          xlab = "negative/ rather negative/ neutral/ rather positive /positive image of EU", 
          col = euro_color, breaks = 0.5 + -3:2, xlim=c(-2.5,2.5))
mtext(side=3, line=-1, adj=0, cex=1, paste("Number of modes:", pol_euro[[3]]))
lines(pol_euro[[4]]$den_x, (pol_euro[[4]]$den_y * length(euro_data$attitude) * mean(diff(h$breaks))), col="red", lwd=3)

# for each inv level (engagement in discourse)?
euro_inv <- euro_data  %>%
  group_split(involvement)
par(mfrow = c(1, 3))
for(i in 1:3){
  mod <- pol_modality(euro_inv[[i]]$attitude)
  print(paste("Involvement level", i, "- Modality p value:", 
              mod[1], "Uniform:", mod[2], "Number of modes:", mod[[3]]))
  h <- hist(euro_inv[[i]]$attitude, main = paste("Eurobarometer involvement", i), 
            xlab = "Negative (-2) to positive (2) image of the EU",  font.lab = 2, cex.lab = 1.5,
            ylab = "Density", font.lab = 2, cex.lab = 1.5, cex.main = 2, ylim = c(0, 0.5),
            col = euro_color, xaxt = "n", yaxt = "n", freq = FALSE,  breaks = 0.5 + -3:2, xlim=c(-2.5,2.5))
  axis(1,at=-2:2, labels = -2:2, cex.axis = 1.5, cex.lab = 2)
  axis(2, cex.axis = 1.5, cex.lab = 2)
  mtext( side = 3, line = -1, adj = 0, cex=1, paste("Number of modes:", mod[[3]]))
  mtext( side = 3, line = -2.5, adj = 0, cex=1, paste("N =", nrow(euro_inv[[i]])))
  lines(mod[[4]]$den_x, mod[[4]]$den_y, col="red", lwd=3)
} 
par(mfrow = c(1, 1))


# Covid  ----------------------------------------------------------------------#

# general polarization across involvement levels?
pol_covid_overall <- list()
for(i in 1:6){
  path <- paste0("Data/processed_data/covid_T",i,".csv")
  data <- read.csv(path) %>% 
    select(-X)
  
  pol_covid_overall[[i]] <- pol_modality(data$mean_attitude) 
  h <- hist(data$mean_attitude, main = paste0("Attitude towards Covid vaccines T", i), 
            xlab = " ", col = "lightblue",  breaks = 0.5 + 0:7, xlim=c(1,7.5))
  mtext(side=3, line=-1, adj=0.1, cex=1, paste("Number of modes:", pol_covid_overall[[i]][[3]]))
  lines(pol_covid_overall[[i]][[4]]$den_x, (pol_covid_overall[[i]][[4]]$den_y * length(data$mean_attitude) * mean(diff(h$breaks))), col="red", lwd=2)
}

# check polarization across involvement levels 
pol_covid_inv <- list()
for(i in 1:6){
  path <- paste0("Data/processed_data/covid_T",i,".csv")
  data <- read.csv(path) %>% 
    select(-X)
  
  covid_inv <- data %>%
      group_split(Vacc_Oth_6)
  par(mfrow=c(2,4))
  for(j in 1:7){
    mod <- pol_modality(covid_inv[[j]]$mean_attitude)
    print(paste("Covid T", i, "Involvement level", j, "- Modality p value:", 
                mod[1], "Uniform:", mod[2], "Number of modes:", mod[[3]]))
    h <- hist(covid_inv[[j]]$mean_attitude, main = "",
              xlab = "Attitude towards Covid vaccines",  font.lab = 2, cex.lab = 1.5,
              ylab = "Density",  font.lab = 2, cex.lab = 1.5, col = covid_color, freq=FALSE,
              cex.main = 1.5, font.lab = 2, 
              breaks = 0.5 + 0:7, ylim=c(0, 0.6), xlim=c(0.5,7.5), xaxt = "n", yaxt = "n")
    title(main = paste("Involvement ", j), cex.main = 2.5)
    axis(1, at=1:7, labels=1:7, cex.axis = 1.5, cex.lab = 3.5)
    axis(2, cex.axis = 1.5, cex.lab = 3.5)
    mtext(side = 3, line = -1, adj = 0.1, cex=1.5, paste("Number of modes:", mod[[3]]))
    lines(mod[[4]]$den_x, mod[[4]]$den_y, col="red", lwd=3)
  } 
  par(mfrow=c(1,1))
}


# merging all timepoints ---
covid <- data.frame()
for(i in 1:6){
  path <- paste0("Data/processed_data/covid_T",i,".csv")
  data <- read.csv(path) %>% 
    select(-X)
  covid <- rbind(covid, data)
  print(paste(i, "done"))
}

# overall polarization?
pol_covid <- pol_modality(covid$mean_attitude) # unimodal
h <- hist(covid$mean_attitude, main = paste0("Attitude towards Covid vaccines"), 
          cex.main = 1.5, font.lab = 2, xaxt = "n",
          xlab = "Against (1) - In favor (7)", col = covid_color,  breaks = 0.5 + 0:7, xlim=c(0.5,7.5))
axis(1, at=1:7, labels=1:7)
mtext(side=3, line=-1, adj=0.1, cex=1, paste("Number of modes:", pol_covid[[3]]))
lines(pol_covid[[4]]$den_x, (pol_covid[[4]]$den_y * length(covid$mean_attitude) * mean(diff(h$breaks))), col="red", lwd=3)

# per involvement level
covid_inv <- covid %>%
  group_split(Vacc_Oth_6)
par(mfrow=c(2,4))
for(j in 1:7){
  mod <- pol_modality(covid_inv[[j]]$mean_attitude)
  print(paste("Covid T1-T6; Involvement level", j, "- Modality p value:", 
              mod[1], "Uniform:", mod[2], "Number of modes:", mod[[3]]))
  h <- hist(covid_inv[[j]]$mean_attitude, main = paste0("Covid T1-T6 involvement ", j), 
            xlab = "Attitude towards Covid vaccines", col = "lightblue", freq=FALSE,
            breaks = 0.5 + 0:7, ylim=c(0, 0.6), xlim=c(0.5,7.5))
  mtext(side = 3, line = -1, adj = 0.1, cex=0.7, paste("Number of modes:", mod[[3]]))
  mtext(side = 3, line = -2, adj = 0.1, cex=0.7, paste("N =", nrow(covid_inv[[i]])))
  lines(mod[[4]]$den_x, mod[[4]]$den_y, col="red", lwd=2)
} 
