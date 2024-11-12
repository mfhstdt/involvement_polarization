#### Simulation to assess robustness and accuracy of mode detection methods #####
#  Step 3: apply modality detection methods to simulated distributions         #

source("./Simulation_detection_methods/modality_simulation2.R")
distributions_list <- readRDS("./Simulation_detection_methods/all_distributions.RData")

# data frame to save results ---
results_simulations <- data.frame(
  detection_method = rep(c("silverman", "bc", "dip", "Gaussian mixture", "DFU",
                           "Density based detection method", 
                           "Adjusted density based detection method"), each = 38),
  distribution = rep(names(distributions_list), 7),
  N = c(300, 30000,
        rep(c(300,500,20000,30000), 9)),
  true_modes = c(rep(1, 14),
                 rep(2, 12),
                 rep(3, 12)),
  multimodality_predicted = NA,
  result = NA
)

# compute #modes with silverman's method ----------------------
results_silverman <- apply_silverman(distributions_list)
results_simulations[, 6] <- results_silverman # save number of modes
results_simulations[, 5] <- ifelse(results_silverman > 1, TRUE, FALSE) # multimodality detected?

# compute Bimodality coefficient -------------------------------
bc_results <- apply_bc(distributions_list) # compute for each distribution
results_simulations[39:76, 5] <- bc_results$bimodality
results_simulations[39:76, 6] <- bc_results$BC

# compute Hartigan's dip --------------------------------------
results_simulations[77:114, 5] <- apply_dip(distributions_list)

# Fit Gaussian mixture models ---------------------------------
#results_gmm <- apply_gaussianmix(distributions_list)

# Compute distance from unimodality (DFU) ---------------------
results_dfu <- apply_dfu(distributions_list)
results_simulations[153:190, 6] <- results_dfu
results_simulations[153:190, 5] <- ifelse(results_dfu > 0, TRUE, FALSE)

# Density based measure (Haslbeck et al.) -----------------
results_densMM <- apply_densMM(distributions_list)
results_simulations[191:228, 6] <- results_densMM
results_simulations[191:228, 5] <- ifelse(results_densMM > 1, TRUE, FALSE)

# Density based measure with adjusted noise (ANAM) -----------------
results_ANAM <- apply_ANAM(distributions_list)
results_simulations[229:266, 6] <- results_ANAM
results_simulations[229:266, 5] <- ifelse(results_ANAM > 1, TRUE, FALSE)

# save results 
write.csv(results_simulations, "Analyses/Modality_detection/Simulation_detection_methods/results_simulations.csv")

# Assess overall performance of each method ----------------------------------#
results_simulations$accuracy_multimodality <- ifelse((results_simulations$true_modes > 1 & results_simulations$multimodality_predicted == TRUE) | (results_simulations$true_modes <= 1 & results_simulations$multimodality_predicted == FALSE), 
                                       1,
                                       0)
# accuracy in discriminating uni- from multimodality
results_simulations %>%
  group_by(detection_method) %>%
  summarise(mean(accuracy_multimodality, na.rm = TRUE))

# accuracy in detecting correct number of modes for adjusted densMM
results_simulations %>%
  filter(detection_method == "Adjusted density based mode detection") %>%
  summarise(sum(true_modes == result)/38)   # 78%

            