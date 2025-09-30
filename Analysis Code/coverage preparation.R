## Calculation of Coverage ##

library(foreach)
library(tidyverse)

grid = expand.grid(
  N = c(400, 800, 1600)
  , p = c(0.1, 0.2, 0.4)
  , korr = c(sin(pi*0.2))
  , pm = c(0.1, 0.3, 0.5)
  , mech = c("MCAR", "MAR", "MNAR")
  , stringsAsFactors = FALSE
)

## COVERAGE CALCULATION

# SPECIFICITY ----

results$Spec_Waldcoverage70[results$SpecWaldL <= 0.7 & results$SpecWaldU >= 0.7 & results$Scenario == c(1:243)] <- 1 
results$Spec_Waldcoverage80[results$SpecWaldL <= 0.8 & results$SpecWaldU >= 0.8 & results$Scenario == c(244:486)] <- 1
results$Spec_Waldcoverage90[results$SpecWaldL <= 0.9 & results$SpecWaldU >= 0.9 & results$Scenario == c(487:729)] <- 1 

results$Spec_Logitcoverage70[results$SpecLogitL <= 0.7 & results$SpecLogitU >= 0.7 & results$Scenario == c(1:243)] <- 1 
results$Spec_Logitcoverage80[results$SpecLogitL <= 0.8 & results$SpecLogitU >= 0.8 & results$Scenario == c(244:486)] <- 1
results$Spec_Logitcoverage90[results$SpecLogitL <= 0.9 & results$SpecLogitU >= 0.9 & results$Scenario == c(487:729)] <- 1

# Average Coverages Specificity

# Wald

average_Waldcoverage70_spec_cca <- foreach(i = 1:243, .combine = c) %do% {
  sum(results$Spec_Waldcoverage70[results$Scenario == i & results$Method == "CCA"], na.rm = TRUE)/nsim
}

average_Waldcoverage80_spec_cca <- foreach(i = 244:486, .combine = c) %do% {
  sum(results$Spec_Waldcoverage80[results$Scenario == i & results$Method == "CCA"], na.rm = TRUE)/nsim
}

average_Waldcoverage90_spec_cca <- foreach(i = 487:729, .combine = c) %do% {
  sum(results$Spec_Waldcoverage90[results$Scenario == i & results$Method == "CCA"], na.rm = TRUE)/nsim
}

average_Waldcoverage70_spec_wc <- foreach(i = 1:243, .combine = c) %do% {
  sum(results$Spec_Waldcoverage70[results$Scenario == i & results$Method == "WC"], na.rm = TRUE)/nsim
}

average_Waldcoverage80_spec_wc <- foreach(i = 244:486, .combine = c) %do% {
  sum(results$Spec_Waldcoverage80[results$Scenario == i & results$Method == "WC"], na.rm = TRUE)/nsim
}

average_Waldcoverage90_spec_wc <- foreach(i = 487:729, .combine = c) %do% {
  sum(results$Spec_Waldcoverage90[results$Scenario == i & results$Method == "WC"], na.rm = TRUE)/nsim
}

average_Waldcoverage70_spec_rhd <- foreach(i = 1:243, .combine = c) %do% {
  sum(results$Spec_Waldcoverage70[results$Scenario == i & results$Method == "RHD"], na.rm = TRUE)/nsim
}

average_Waldcoverage80_spec_rhd <- foreach(i = 244:486, .combine = c) %do% {
  sum(results$Spec_Waldcoverage80[results$Scenario == i & results$Method == "RHD"], na.rm = TRUE)/nsim
}

average_Waldcoverage90_spec_rhd <- foreach(i = 487:729, .combine = c) %do% {
  sum(results$Spec_Waldcoverage90[results$Scenario == i & results$Method == "RHD"], na.rm = TRUE)/nsim
}

average_Waldcoverage70_spec_mlmcar <- foreach(i = 1:243, .combine = c) %do% {
  sum(results$Spec_Waldcoverage70[results$Scenario == i & results$Method == "MLMCAR"], na.rm = TRUE)/nsim
}

average_Waldcoverage80_spec_mlmcar <- foreach(i = 244:486, .combine = c) %do% {
  sum(results$Spec_Waldcoverage80[results$Scenario == i & results$Method == "MLMCAR"], na.rm = TRUE)/nsim
}

average_Waldcoverage90_spec_mlmcar <- foreach(i = 487:729, .combine = c) %do% {
  sum(results$Spec_Waldcoverage90[results$Scenario == i & results$Method == "MLMCAR"], na.rm = TRUE)/nsim
}

average_Waldcoverage70_spec_mlmar <- foreach(i = 1:243, .combine = c) %do% {
  sum(results$Spec_Waldcoverage70[results$Scenario == i & results$Method == "MLMAR"], na.rm = TRUE)/nsim
}

average_Waldcoverage80_spec_mlmar <- foreach(i = 244:486, .combine = c) %do% {
  sum(results$Spec_Waldcoverage80[results$Scenario == i & results$Method == "MLMAR"], na.rm = TRUE)/nsim
}

average_Waldcoverage90_spec_mlmar <- foreach(i = 487:729, .combine = c) %do% {
  sum(results$Spec_Waldcoverage90[results$Scenario == i & results$Method == "MLMAR"], na.rm = TRUE)/nsim
}

average_Waldcoverage70_spec_wlsmcar <- foreach(i = 1:243, .combine = c) %do% {
  sum(results$Spec_Waldcoverage70[results$Scenario == i & results$Method == "WLSMCAR"], na.rm = TRUE)/nsim
}

average_Waldcoverage80_spec_wlsmcar <- foreach(i = 244:486, .combine = c) %do% {
  sum(results$Spec_Waldcoverage80[results$Scenario == i & results$Method == "WLSMCAR"], na.rm = TRUE)/nsim
}

average_Waldcoverage90_spec_wlsmcar <- foreach(i = 487:729, .combine = c) %do% {
  sum(results$Spec_Waldcoverage90[results$Scenario == i & results$Method == "WLSMCAR"], na.rm = TRUE)/nsim
}

average_Waldcoverage70_spec_mice <- foreach(i = 1:243, .combine = c) %do% {
  sum(results$Spec_Waldcoverage70[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}

average_Waldcoverage80_spec_mice <- foreach(i = 244:486, .combine = c) %do% {
  sum(results$Spec_Waldcoverage80[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}

average_Waldcoverage90_spec_mice <- foreach(i = 487:729, .combine = c) %do% {
  sum(results$Spec_Waldcoverage90[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}


# Logit

average_Logitcoverage70_spec_cca <- foreach(i = 1:243, .combine = c) %do% {
  sum(results$Spec_Logitcoverage70[results$Scenario == i & results$Method == "CCA"], na.rm = TRUE)/nsim
}

average_Logitcoverage80_spec_cca <- foreach(i = 244:486, .combine = c) %do% {
  sum(results$Spec_Logitcoverage80[results$Scenario == i & results$Method == "CCA"], na.rm = TRUE)/nsim
}

average_Logitcoverage90_spec_cca <- foreach(i = 487:729, .combine = c) %do% {
  sum(results$Spec_Logitcoverage90[results$Scenario == i & results$Method == "CCA"], na.rm = TRUE)/nsim
}

average_Logitcoverage70_spec_wc <- foreach(i = 1:243, .combine = c) %do% {
  sum(results$Spec_Logitcoverage70[results$Scenario == i & results$Method == "WC"], na.rm = TRUE)/nsim
}

average_Logitcoverage80_spec_wc <- foreach(i = 244:486, .combine = c) %do% {
  sum(results$Spec_Logitcoverage80[results$Scenario == i & results$Method == "WC"], na.rm = TRUE)/nsim
}

average_Logitcoverage90_spec_wc <- foreach(i = 487:729, .combine = c) %do% {
  sum(results$Spec_Logitcoverage90[results$Scenario == i & results$Method == "WC"], na.rm = TRUE)/nsim
}

average_Logitcoverage70_spec_rhd <- foreach(i = 1:243, .combine = c) %do% {
  sum(results$Spec_Logitcoverage70[results$Scenario == i & results$Method == "RHD"], na.rm = TRUE)/nsim
}

average_Logitcoverage80_spec_rhd <- foreach(i = 244:486, .combine = c) %do% {
  sum(results$Spec_Logitcoverage80[results$Scenario == i & results$Method == "RHD"], na.rm = TRUE)/nsim
}

average_Logitcoverage90_spec_rhd <- foreach(i = 487:729, .combine = c) %do% {
  sum(results$Spec_Logitcoverage90[results$Scenario == i & results$Method == "RHD"], na.rm = TRUE)/nsim
}

average_Logitcoverage70_spec_mlmcar <- foreach(i = 1:243, .combine = c) %do% {
  sum(results$Spec_Logitcoverage70[results$Scenario == i & results$Method == "MLMCAR"], na.rm = TRUE)/nsim
}

average_Logitcoverage80_spec_mlmcar <- foreach(i = 244:486, .combine = c) %do% {
  sum(results$Spec_Logitcoverage80[results$Scenario == i & results$Method == "MLMCAR"], na.rm = TRUE)/nsim
}

average_Logitcoverage90_spec_mlmcar <- foreach(i = 487:729, .combine = c) %do% {
  sum(results$Spec_Logitcoverage90[results$Scenario == i & results$Method == "MLMCAR"], na.rm = TRUE)/nsim
}

average_Logitcoverage70_spec_mlmar <- foreach(i = 1:243, .combine = c) %do% {
  sum(results$Spec_Logitcoverage70[results$Scenario == i & results$Method == "MLMAR"], na.rm = TRUE)/nsim
}

average_Logitcoverage80_spec_mlmar <- foreach(i = 244:486, .combine = c) %do% {
  sum(results$Spec_Logitcoverage80[results$Scenario == i & results$Method == "MLMAR"], na.rm = TRUE)/nsim
}

average_Logitcoverage90_spec_mlmar <- foreach(i = 487:729, .combine = c) %do% {
  sum(results$Spec_Logitcoverage90[results$Scenario == i & results$Method == "MLMAR"], na.rm = TRUE)/nsim
}

average_Logitcoverage70_spec_wlsmcar <- foreach(i = 1:243, .combine = c) %do% {
  sum(results$Spec_Logitcoverage70[results$Scenario == i & results$Method == "WLSMCAR"], na.rm = TRUE)/nsim
}

average_Logitcoverage80_spec_wlsmcar <- foreach(i = 244:486, .combine = c) %do% {
  sum(results$Spec_Logitcoverage80[results$Scenario == i & results$Method == "WLSMCAR"], na.rm = TRUE)/nsim
}

average_Logitcoverage90_spec_wlsmcar <- foreach(i = 487:729, .combine = c) %do% {
  sum(results$Spec_Logitcoverage90[results$Scenario == i & results$Method == "WLSMCAR"], na.rm = TRUE)/nsim
}

average_Logitcoverage70_spec_mice <- foreach(i = 1:243, .combine = c) %do% {
  sum(results$Spec_Logitcoverage70[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}

average_Logitcoverage80_spec_mice <- foreach(i = 244:486, .combine = c) %do% {
  sum(results$Spec_Logitcoverage80[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}

average_Logitcoverage90_spec_mice <- foreach(i = 487:729, .combine = c) %do% {
  sum(results$Spec_Logitcoverage90[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}



grid <- rbind(grid, grid, grid)
colnames(grid) <- c("samplesize", "proportion", "korr", "proportionmissing", "Missingness")

average_Logitcoverage70_spec_cca_df <- data.frame(average_Logitcoverage70_spec_cca, grid)
average_Logitcoverage80_spec_cca_df <- data.frame(average_Logitcoverage80_spec_cca, grid)
average_Logitcoverage90_spec_cca_df <- data.frame(average_Logitcoverage90_spec_cca, grid)

average_Logitcoverage70_spec_wc_df <- data.frame(average_Logitcoverage70_spec_wc, grid)
average_Logitcoverage80_spec_wc_df <- data.frame(average_Logitcoverage80_spec_wc, grid)
average_Logitcoverage90_spec_wc_df <- data.frame(average_Logitcoverage90_spec_wc, grid)

average_Logitcoverage70_spec_rhd_df <- data.frame(average_Logitcoverage70_spec_rhd, grid)
average_Logitcoverage80_spec_rhd_df <- data.frame(average_Logitcoverage80_spec_rhd, grid)
average_Logitcoverage90_spec_rhd_df <- data.frame(average_Logitcoverage90_spec_rhd, grid)

average_Logitcoverage70_spec_mlmcar_df <- data.frame(average_Logitcoverage70_spec_mlmcar, grid)
average_Logitcoverage80_spec_mlmcar_df <- data.frame(average_Logitcoverage80_spec_mlmcar, grid)
average_Logitcoverage90_spec_mlmcar_df <- data.frame(average_Logitcoverage90_spec_mlmcar, grid)

average_Logitcoverage70_spec_mlmar_df <- data.frame(average_Logitcoverage70_spec_mlmar, grid)
average_Logitcoverage80_spec_mlmar_df <- data.frame(average_Logitcoverage80_spec_mlmar, grid)
average_Logitcoverage90_spec_mlmar_df <- data.frame(average_Logitcoverage90_spec_mlmar, grid)

average_Logitcoverage70_spec_wlsmcar_df <- data.frame(average_Logitcoverage70_spec_wlsmcar, grid)
average_Logitcoverage80_spec_wlsmcar_df <- data.frame(average_Logitcoverage80_spec_wlsmcar, grid)
average_Logitcoverage90_spec_wlsmcar_df <- data.frame(average_Logitcoverage90_spec_wlsmcar, grid)

average_Logitcoverage70_spec_mice_df <- data.frame(average_Logitcoverage70_spec_mice, grid)
average_Logitcoverage80_spec_mice_df <- data.frame(average_Logitcoverage80_spec_mice, grid)
average_Logitcoverage90_spec_mice_df <- data.frame(average_Logitcoverage90_spec_mice, grid)


average_Waldcoverage70_spec_cca_df <- data.frame(average_Waldcoverage70_spec_cca, grid)
average_Waldcoverage80_spec_cca_df <- data.frame(average_Waldcoverage80_spec_cca, grid)
average_Waldcoverage90_spec_cca_df <- data.frame(average_Waldcoverage90_spec_cca, grid)

average_Waldcoverage70_spec_wc_df <- data.frame(average_Waldcoverage70_spec_wc, grid)
average_Waldcoverage80_spec_wc_df <- data.frame(average_Waldcoverage80_spec_wc, grid)
average_Waldcoverage90_spec_wc_df <- data.frame(average_Waldcoverage90_spec_wc, grid)

average_Waldcoverage70_spec_rhd_df <- data.frame(average_Waldcoverage70_spec_rhd, grid)
average_Waldcoverage80_spec_rhd_df <- data.frame(average_Waldcoverage80_spec_rhd, grid)
average_Waldcoverage90_spec_rhd_df <- data.frame(average_Waldcoverage90_spec_rhd, grid)

average_Waldcoverage70_spec_mlmcar_df <- data.frame(average_Waldcoverage70_spec_mlmcar, grid)
average_Waldcoverage80_spec_mlmcar_df <- data.frame(average_Waldcoverage80_spec_mlmcar, grid)
average_Waldcoverage90_spec_mlmcar_df <- data.frame(average_Waldcoverage90_spec_mlmcar, grid)

average_Waldcoverage70_spec_mlmar_df <- data.frame(average_Waldcoverage70_spec_mlmar, grid)
average_Waldcoverage80_spec_mlmar_df <- data.frame(average_Waldcoverage80_spec_mlmar, grid)
average_Waldcoverage90_spec_mlmar_df <- data.frame(average_Waldcoverage90_spec_mlmar, grid)

average_Waldcoverage70_spec_wlsmcar_df <- data.frame(average_Waldcoverage70_spec_wlsmcar, grid)
average_Waldcoverage80_spec_wlsmcar_df <- data.frame(average_Waldcoverage80_spec_wlsmcar, grid)
average_Waldcoverage90_spec_wlsmcar_df <- data.frame(average_Waldcoverage90_spec_wlsmcar, grid)

average_Waldcoverage70_spec_mice_df <- data.frame(average_Waldcoverage70_spec_mice, grid)
average_Waldcoverage80_spec_mice_df <- data.frame(average_Waldcoverage80_spec_mice, grid)
average_Waldcoverage90_spec_mice_df <- data.frame(average_Waldcoverage90_spec_mice, grid)



# SENSITIVITY

results$Sens_Waldcoverage[results$SensWaldL <= 0.7 & results$SensWaldU >= 0.7 & results$sensitivity == 0.7] <- 1 
results$Sens_Waldcoverage[results$SensWaldL <= 0.8 & results$SensWaldU >= 0.8 & results$sensitivity == 0.8] <- 1
results$Sens_Waldcoverage[results$SensWaldL <= 0.9 & results$SensWaldU >= 0.9 & results$sensitivity == 0.9] <- 1 

results$Sens_Logitcoverage[results$SensLogitL <= 0.7 & results$SensLogitU >= 0.7 & results$sensitivity == 0.7] <- 1 
results$Sens_Logitcoverage[results$SensLogitL <= 0.8 & results$SensLogitU >= 0.8 & results$sensitivity == 0.8] <- 1
results$Sens_Logitcoverage[results$SensLogitL <= 0.9 & results$SensLogitU >= 0.9 & results$sensitivity == 0.9] <- 1 

average_Waldcoverage_sens_cca <- foreach(i = 1:729, .combine = c) %do% {
  sum(results$Sens_Waldcoverage[results$Scenario == i & results$Method == "CCA"], na.rm = TRUE)/nsim
}
  
average_Waldcoverage_sens_wc <- foreach(i = 1:729, .combine = c) %do% {
  sum(results$Sens_Waldcoverage[results$Scenario == i & results$Method == "WC"], na.rm = TRUE)/nsim
}

average_Waldcoverage_sens_rhd <- foreach(i = 1:729, .combine = c) %do% {
  sum(results$Sens_Waldcoverage[results$Scenario == i & results$Method == "RHD"], na.rm = TRUE)/nsim
}

average_Waldcoverage_sens_mlmcar <- foreach(i = 1:729, .combine = c) %do% {
  sum(results$Sens_Waldcoverage[results$Scenario == i & results$Method == "MLMCAR"], na.rm = TRUE)/nsim
}

average_Waldcoverage_sens_mlmar <- foreach(i = 1:729, .combine = c) %do% {
  sum(results$Sens_Waldcoverage[results$Scenario == i & results$Method == "MLMAR"], na.rm = TRUE)/nsim
}

average_Waldcoverage_sens_wlsmcar <- foreach(i = 1:729, .combine = c) %do% {
  sum(results$Sens_Waldcoverage[results$Scenario == i & results$Method == "WLSMCAR"], na.rm = TRUE)/nsim
}

average_Waldcoverage_sens_mice <- foreach(i = 1:729, .combine = c) %do% {
  sum(results$Sens_Waldcoverage[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}

average_Logitcoverage_sens_cca <- foreach(i = 1:729, .combine = c) %do% {
  sum(results$Sens_Logitcoverage[results$Scenario == i & results$Method == "CCA"], na.rm = TRUE)/nsim
}

average_Logitcoverage_sens_wc <- foreach(i = 1:729, .combine = c) %do% {
  sum(results$Sens_Logitcoverage[results$Scenario == i & results$Method == "WC"], na.rm = TRUE)/nsim
}

average_Logitcoverage_sens_rhd <- foreach(i = 1:729, .combine = c) %do% {
  sum(results$Sens_Logitcoverage[results$Scenario == i & results$Method == "RHD"], na.rm = TRUE)/nsim
}

average_Logitcoverage_sens_mlmcar <- foreach(i = 1:729, .combine = c) %do% {
  sum(results$Sens_Logitcoverage[results$Scenario == i & results$Method == "MLMCAR"], na.rm = TRUE)/nsim
}

average_Logitcoverage_sens_mlmar <- foreach(i = 1:729, .combine = c) %do% {
  sum(results$Sens_Logitcoverage[results$Scenario == i & results$Method == "MLMAR"], na.rm = TRUE)/nsim
}

average_Logitcoverage_sens_wlsmcar <- foreach(i = 1:729, .combine = c) %do% {
  sum(results$Sens_Logitcoverage[results$Scenario == i & results$Method == "WLSMCAR"], na.rm = TRUE)/nsim
}

average_Logitcoverage_sens_mice <- foreach(i = 1:729, .combine = c) %do% {
  sum(results$Sens_Logitcoverage[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}

grid729 <- rbind(grid, grid, grid)

average_Logitcoverage_sens_cca_df <- data.frame(average_Logitcoverage_sens_cca, grid729)
average_Logitcoverage_sens_wc_df <- data.frame(average_Logitcoverage_sens_wc, grid729)
average_Logitcoverage_sens_rhd_df <- data.frame(average_Logitcoverage_sens_rhd, grid729)
average_Logitcoverage_sens_mlmcar_df <- data.frame(average_Logitcoverage_sens_mlmcar, grid729)
average_Logitcoverage_sens_mlmar_df <- data.frame(average_Logitcoverage_sens_mlmar, grid729)
average_Logitcoverage_sens_wlsmcar_df <- data.frame(average_Logitcoverage_sens_wlsmcar, grid729)
average_Logitcoverage_sens_mice_df <- data.frame(average_Logitcoverage_sens_mice, grid729)


average_Waldcoverage_sens_cca_df <- data.frame(average_Waldcoverage_sens_cca, grid729)
average_Waldcoverage_sens_wc_df <- data.frame(average_Waldcoverage_sens_wc, grid729)
average_Waldcoverage_sens_rhd_df <- data.frame(average_Waldcoverage_sens_rhd, grid729)
average_Waldcoverage_sens_mlmcar_df <- data.frame(average_Waldcoverage_sens_mlmcar, grid729)
average_Waldcoverage_sens_mlmar_df <- data.frame(average_Waldcoverage_sens_mlmar, grid729)
average_Waldcoverage_sens_wlsmcar_df <- data.frame(average_Waldcoverage_sens_wlsmcar, grid729)
average_Waldcoverage_sens_mice_df <- data.frame(average_Waldcoverage_sens_mice, grid729)

