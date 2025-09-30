### Simulation Study for the comparison of imputation methods for sensitivity and specificity as
### co-primary endpoints in diagnostic studies.

library(mvtnorm)
library(mice)
library(missMethods)
library(tidyverse)
library(vctrs)
library(DescTools)
library(foreach)
library(doParallel)
library(VIM)
library(miceafter)

source("functions 3.R") #data generating functions
source("catdata.R") #ACD package
source("grid.R") #dataframe with all scenarios

nsim <- 1000 #number of simulations

#predictor matrix that is needed for mice
row1 <- c(0, 1, 1, 1, 1)
rowelse <- c(0, 0, 0, 0, 0)
prepred <- c(row1, rowelse, rowelse, rowelse, rowelse)
predictors <- t(matrix(prepred, nrow = 5, ncol = 5))

#number of imputations mice
m0 <- 5

#matrix needed for ML method
a1 <- c(
  c(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1)
)

zp_a <- matrix(a1, nrow = 32, ncol = 16)


#z value to calculate confidence intervals
z <- qnorm(0.975)

#parallel computing
cores <- detectCores()
cl <- makeCluster(cores - 2)
registerDoParallel(cl)

TIME1 <- proc.time()
# COMPLETE CASE ANALYSIS ----
  
  ### Here, only complete cases will be used to estimate sensitivity and specificity. ###
  ### This means that all lines containing missing values will be omitted.            ###

rm(.Random.seed, envir=globalenv()) #remove seed

cca_result <- foreach(a = 1:nsim, .combine = rbind, .packages = c("vctrs", "DescTools", "foreach", "doParallel", "mvtnorm", "mice")) %dopar% {
  
  cca_result_pre <- foreach(i = 1:nrow(grid), .combine = rbind) %do% {
    seed <- (1000*a) + i
    cca_time <- NA
    
    #generate data
    newdata <- data_fun(grid$N[i], grid$p[i], grid$mu[i], grid$c[i], grid$korr[i], grid$mech[i], grid$pm[i], c(0,1,1,1,1), seed)
    
    #save "true empirical" sensitivity and specificity
    tpf <- newdata$sens[1]
    tnf <- newdata$spec[1]
    
    
    #check if all test results for diseased/non-diseased patients are missing
    if (sum(!is.na(newdata[which(newdata$D==1), "V1"]))==0 | sum(!is.na(newdata[which(newdata$D==0), "V1"]))==0) {
      
      cca_sens <- rep(NA, 6)
      cca_spec <- rep(NA, 6)
      
    } else {
    
      
    newdata <- newdata[complete.cases(newdata), ]  
    newdata.cca.d0 <- vec_slice(newdata, newdata$D == 0) #non-diseased subset
    newdata.cca.d1 <- vec_slice(newdata, newdata$D == 1) #diseased subset
    
    #sensitivity estimate and CI
    cca_sens <- BinomCI(x = length(newdata.cca.d1$V1[newdata.cca.d1$V1 == 1]), n = length(newdata.cca.d1$D), conf.level = 0.95, method = c("logit", "wald"))
    
    #specificity estimate and CI
    cca_spec <- BinomCI(x = length(newdata.cca.d0$V1[newdata.cca.d0$V1 == 0]), n = length(newdata.cca.d0$D), conf.level = 0.95, method = c("logit", "wald"))
    }
    #store results
    cca <- c(i, cca_time, cca_sens, cca_spec, seed, tpf, tnf)
    
  }
}

#store results in data frame and change column names
cca_result_df <- as.data.frame(cca_result)
colnames(cca_result_df) <- c("Scenario", "Time", "Sens", "Sens2", "SensLogitL", "SensWaldL", "SensLogitU", "SensWaldU"
                             , "Spec", "Spec2", "SpecLogitL", "SpecWaldL", "SpecLogitU", "SpecWaldU", "Seed", "eTPF", "eTNF")




# WORST CASE SCENARIO ----

### Missing values are replaced by the inverse of the true disease status, i.e. if D == 1, then V1 = 0, to estimate
### the worst case sensitivity and specificity.

rm(.Random.seed, envir=globalenv()) #remove seed

wc_result <- foreach(a = 1:nsim, .combine = rbind, .packages = c("vctrs", "DescTools", "foreach", "doParallel", "mvtnorm", "mice", "tidyverse")) %dopar% {

wc_result_pre <- foreach(i = 1:nrow(grid), .combine = rbind) %do% {
  
  seed <- (1000*a) + i
  
  #generate data
  newdata <- data_fun(grid$N[i], grid$p[i], grid$mu[i], grid$c[i], grid$korr[i], grid$mech[i], grid$pm[i], c(0,1,1,1,1), seed)
  
  #save "true empirical" sensitivity and specificity
  tpf <- newdata$sens[1]
  tnf <- newdata$spec[1]
  
  #check if all test results for diseased/non-diseased patients are missing
  if (sum(!is.na(newdata[which(newdata$D==1), "V1"]))==0 | sum(!is.na(newdata[which(newdata$D==0), "V1"]))==0) {
    
    wc_sens <- rep(NA, 6)
    wc_spec <- rep(NA, 6)
    
  } else {
  
  
  t1 <- proc.time() #time start impute
  newdata.wc <- worstcase(newdata) #worst case single imputation
  t2 <- proc.time() #time end impute
  wc_time <- t2[3] - t1[3] #calculate imputation time
  
  newdata.wc.d0 <- vec_slice(newdata.wc, newdata.wc$D == 0) #non-diseased subset
  newdata.wc.d1 <- vec_slice(newdata.wc, newdata.wc$D == 1) #diseased subset
  
  #sensitivity estimate and CI
  wc_sens <- BinomCI(x = length(newdata.wc.d1$V1[newdata.wc.d1$V1 == 1]), n = length(newdata.wc.d1$D), conf.level = 0.95, method = c("logit", "wald"))
  
  #specificity estimate and CI
  wc_spec <- BinomCI(x = length(newdata.wc.d0$V1[newdata.wc.d0$V1 == 0]), n = length(newdata.wc.d0$D), conf.level = 0.95, method = c("logit", "wald"))
  }
  
  #store results
  wc <- c(i, wc_time, wc_sens, wc_spec, seed, tpf, tnf)
  
}
}

#store results in data frame and change column names
wc_result_df <- as.data.frame(wc_result)
colnames(wc_result_df) <- c("Scenario", "Time", "Sens", "Sens2", "SensLogitL", "SensWaldL", "SensLogitU", "SensWaldU"
                            , "Spec", "Spec2", "SpecLogitL", "SpecWaldL", "SpecLogitU", "SpecWaldU", "Seed", "eTPF", "eTNF")
 
# RANDOM HOT DECK ----

### The data set is split into a non-diseased and a diseased subset where each missing value is randomly imputed by a donor value from
### the subset. (package {VIM})

rm(.Random.seed, envir=globalenv()) #remove seed

rhd_result <- foreach(a = 1:nsim, .combine = rbind, .packages = c("vctrs", "DescTools", "foreach", "doParallel", "mvtnorm", "mice", "VIM", "tidyverse")) %dopar% {

  rhd_result_pre <- foreach(i = 1:nrow(grid), .combine = rbind) %do% {
  
  seed <- (1000*a) + i
  
  #generate data
  newdata <- data_fun(grid$N[i], grid$p[i], grid$mu[i], grid$c[i], grid$korr[i], grid$mech[i], grid$pm[i], c(0,1,1,1,1), seed)
  
  #save "true empirical" sensitivity and specificity
  tpf <- newdata$sens[1]
  tnf <- newdata$spec[1]
  
  #check if all test results for diseased/non-diseased patients are missing
  if (sum(!is.na(newdata[which(newdata$D==1), "V1"]))==0 | sum(!is.na(newdata[which(newdata$D==0), "V1"]))==0) {
    
    rhd_sens <- rep(NA, 6)
    rhd_spec <- rep(NA, 6)
    
  } else {
  
  t1 <- proc.time() #time start impute
  newdata.rhd <- hotdeck(newdata, variable = "V1", domain_var = "D", imp_var = FALSE)
  t2 <- proc.time() #time end impute
  rhd_time <- t2[3] - t1[3] #calculate imputation time
  
  newdata.rhd.d0 <- vec_slice(newdata.rhd, newdata.rhd$D == 0) #non-diseased subset
  newdata.rhd.d1 <- vec_slice(newdata.rhd, newdata.rhd$D == 1) #diseased subset
  
  #calculate sensitivity and CI
  rhd_sens <- BinomCI(x = length(newdata.rhd.d1$V1[newdata.rhd.d1$V1 == 1]), n = length(newdata.rhd.d1$D), conf.level = 0.95, method = c("logit", "wald"))
  
  #calculate specificity and CI
  rhd_spec <- BinomCI(x = length(newdata.rhd.d0$V1[newdata.rhd.d0$V1 == 0]), n = length(newdata.rhd.d0$D), conf.level = 0.95, method = c("logit", "wald"))
  
  }
  #store results
  rhd <- c(i, rhd_time, rhd_sens, rhd_spec, seed, tpf, tnf)
  
  
}
}

#store results in data frame and change column names
rhd_result_df <- as.data.frame(rhd_result)
colnames(rhd_result_df) <- c("Scenario", "Time", "Sens", "Sens2", "SensLogitL", "SensWaldL", "SensLogitU", "SensWaldU"
                             , "Spec", "Spec2", "SpecLogitL", "SpecWaldL", "SpecLogitU", "SpecWaldU", "Seed", "eTPF", "eTNF")

# MICE ----

### Multiple Imputation Chained Equations via a logistic regression imputation model. (package {mice})

rm(.Random.seed, envir=globalenv()) #remove seed


mice_result <- foreach(a = 1:nsim, .combine = rbind, .packages = c("vctrs", "DescTools", "foreach", "doParallel", "mvtnorm", "mice", "miceafter", "tidyverse")) %dopar% {
  
  mice_result_pre <- foreach(i = 1:nrow(grid), .combine = rbind) %do% {
    
    
    seed <- (1000*a) + i
    
    #generate data
    newdata <- data_fun(grid$N[i], grid$p[i], grid$mu[i], grid$c[i], grid$korr[i], grid$mech[i], grid$pm[i], c(0,1,1,1,1), seed) 
    
    #save "true empirical" sensitivity and specificity
    tpf <- newdata$sens[1]
    tnf <- newdata$spec[1]
    
    #remove tpf tnf columns
    newdata <- newdata[, -6]
    newdata <- newdata[, -6]
    
    #check if all test results for diseased/non-diseased patients are missing
    if(sum(!is.na(newdata[which(newdata$D==1), "V1"]))==0 | sum(!is.na(newdata[which(newdata$D==0), "V1"]))==0){
      
      mice_sens <- NA
      mice_sens_lower_logit <- NA
      mice_sens_lower_wald <- NA
      mice_sens_upper_logit <- NA
      mice_sens_upper_wald <- NA
      
      mice_spec <- NA
      mice_spec_lower_logit <- NA
      mice_spec_lower_wald <- NA
      mice_spec_upper_logit <- NA
      mice_spec_upper_wald <- NA
      
    }else{
      
      #factorize test results
      newdata$V1 <- as.factor(newdata$V1)
      t1 <- proc.time() #time start impute
      newdata.mice <- mice(newdata, m = m0, method = "logreg", predictors, printFlag = FALSE, threshold = 100, remove.constant = FALSE, remove.collinear = FALSE) #mice imputation
      t2 <- proc.time() #time end impute
      mice_time <- t2[3] - t1[3] #calculate imputation time
      
      newdata.mice <- complete(newdata.mice, 'long') #extract the m imputed data sets
      
      newdata.mice.d0 <- vec_slice(newdata.mice, newdata.mice$D == 0) #subset non-diseased
      newdata.mice.d1 <- vec_slice(newdata.mice, newdata.mice$D == 1) #subset diseased
      
      vwspecm <- foreach(b = 1:m0, .combine = "+") %do% {
        vw_specm <- (length(newdata.mice.d0$V1[newdata.mice.d0$V1 == 0 & newdata.mice.d0$.imp == b])/length(newdata.mice.d0$.imp[newdata.mice.d0$.imp == b]))*
          (1-(length(newdata.mice.d0$V1[newdata.mice.d0$V1 == 0 & newdata.mice.d0$.imp == b])/length(newdata.mice.d0$.imp[newdata.mice.d0$.imp == b])))/
          length(newdata.mice.d0$.imp[newdata.mice.d0$.imp == b])
      }
      
      
      vwsensm <- foreach(b = 1:m0, .combine = "+") %do% {
        vw_sensm <- (length(newdata.mice.d1$V1[newdata.mice.d1$V1 == 1 & newdata.mice.d1$.imp == b])/length(newdata.mice.d1$.imp[newdata.mice.d1$.imp == b]))*
          (1-(length(newdata.mice.d1$V1[newdata.mice.d1$V1 == 1 & newdata.mice.d1$.imp == b])/length(newdata.mice.d1$.imp[newdata.mice.d1$.imp == b])))/
          length(newdata.mice.d1$.imp[newdata.mice.d1$.imp == b])
      }
      
      vwspec <- vwspecm/m0
      vwsens <- vwsensm/m0
      
      micespecm <- foreach(b = 1:m0, .combine = c) %do% {
        mice_specm <- length(newdata.mice.d0$V1[newdata.mice.d0$V1 == 0 & newdata.mice.d0$.imp == b])/length(newdata.mice.d0$.imp[newdata.mice.d0$.imp == b])
      }
      
      micesensm <- foreach(b = 1:m0, .combine = c) %do% {
        mice_sensm <- length(newdata.mice.d1$V1[newdata.mice.d1$V1 == 1 & newdata.mice.d1$.imp == b])/length(newdata.mice.d1$.imp[newdata.mice.d1$.imp == b])
      }
      
      
      mice_sens <- length(newdata.mice.d1$V1[newdata.mice.d1$V1 == 1])/length(newdata.mice.d1$D)
      mice_spec <- length(newdata.mice.d0$V1[newdata.mice.d0$V1 == 0])/length(newdata.mice.d0$D)
      
      vbspec <- sum((micespecm - mice_spec)^2)/(m0-1)
      vbsens <- sum((micesensm - mice_sens)^2)/(m0-1)
      
      sespec <- sqrt(vwspec + vbspec + (vbspec)/m0)
      sesens <- sqrt(vwsens + vbsens + vbsens/m0)
      
      mice_sens_lower_wald <- mice_sens - z*sesens
      mice_sens_upper_wald <- mice_sens + z*sesens
      mice_spec_lower_wald <-mice_spec - z*sespec
      mice_spec_upper_wald <- mice_spec + z*sespec
      
      logitsens <- logit_trans(mice_sens, sesens)
      logitspec <- logit_trans(mice_spec, sespec)
      logitsensci <- invlogit_ci(logitsens[1], logitsens[2], qnorm(0.975))
      logitspecci <- invlogit_ci(logitspec[1], logitspec[2], qnorm(0.975))
      
      
      mice_sens_lower_logit <- logitsensci[3]
      mice_sens_upper_logit <- logitsensci[4]
      mice_spec_lower_logit <- logitspecci[3]
      mice_spec_upper_logit <- logitspecci[4]
      
    }
    
    #store results
    mice <- c(i, mice_time, mice_sens, mice_sens, mice_sens_lower_logit, mice_sens_lower_wald, mice_sens_upper_logit, mice_sens_upper_wald,
              mice_spec, mice_spec, mice_spec_lower_logit, mice_spec_lower_wald, mice_spec_upper_logit, mice_spec_upper_wald, seed, tpf, tnf)
    
    
  }
}


#store results in data frame and change column names
mice_result_df <- as.data.frame(mice_result)
colnames(mice_result_df) <- c("Scenario", "Time", "Sens", "Sens2", "SensLogitL", "SensWaldL", "SensLogitU", "SensWaldU"
                              , "Spec", "Spec2", "SpecLogitL", "SpecWaldL", "SpecLogitU", "SpecWaldU", "Seed", "eTPF", "eTNF")



# MAXIMUM LIKELIHOOD ----

### Maximum Likelihood method for categorical data (only) that can be specified for MCAR and MAR data. (package {ACD})


## MCAR ----


rm(.Random.seed, envir=globalenv()) #remove seed


mlmcar_result <- foreach(a = 1:nsim, .combine = rbind, .packages = c("vctrs", "DescTools", "foreach", "doParallel", "mvtnorm", "mice", "tidyverse")) %dopar% {
  
  ml_result_pre <- foreach(i = 1:nrow(grid), .combine = rbind) %do% {
    
    
    seed <- (1000*a) + i
    
    #generate data
    newdata <- data_fun(grid$N[i], grid$p[i], grid$mu[i], grid$c[i], grid$korr[i], grid$mech[i], grid$pm[i], c(0,1,1,1,1), seed) 
    
    #save "true empirical" sensitivity and specificity
    tpf <- newdata$sens[1]
    tnf <- newdata$spec[1]
    
    #check if all test results for diseased/non-diseased patients are missing
    if(sum(!is.na(newdata[which(newdata$D==1), "V1"]))==0 | sum(!is.na(newdata[which(newdata$D==0), "V1"]))==0){
    
      cat_sens <- NA
      cat_sens_lower_logit <- NA
      cat_sens_lower_wald <- NA
      cat_sens_upper_logit <- NA
      cat_sens_upper_wald <- NA
      
      cat_spec <- NA
      cat_spec_lower_logit <- NA
      cat_spec_lower_wald <- NA
      cat_spec_upper_logit <- NA
      cat_spec_upper_wald <- NA
      
    }else{
    
    #"transform" data into a vector that represents a table for complete cases
    catprep_cc <- c(
      
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
      
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
      
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1])),
      
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1]))
      )
    
    #same step as before but with missing cases
    catprep_na <- c(
      
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),  
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),  
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
      
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1])),  
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),  
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1]))
      )
    
    #transform "table" data into catdata
    catprep <- c(catprep_cc, catprep_na)
    catprep[which(catprep == 0)] <- 0.000001
    catdat <- readCatdata(TF = catprep, Zp = zp_a, Rp = c(16))
    
    t1 <- proc.time() #time start impute
    catdatsat <- satMarML(catdat, missing = "MCAR") #MCAR ML imputation
    t2 <- proc.time() #time end impute
    cat_time <- t2[3] - t1[3]
    
    #extract imputed probabilities
    catdat_mcarimp <- catdatsat$theta
    
    #calculate frequencies for each relevant condition
    catdatt0d0_mcar <- sum(catdat_mcarimp[seq(from = 1, to = 8)])
    catdatt1d0_mcar <- sum(catdat_mcarimp[seq(from = 9, to = 16)]) 
    catdatt0d1_mcar <- sum(catdat_mcarimp[seq(from = 17, to = 24)])
    catdatt1d1_mcar <- sum(catdat_mcarimp[seq(from = 25, to = 32)])
    
    #calculate sensitivity and specificity and CIs
    cat_sens <- catdatt1d1_mcar/(catdatt1d1_mcar + catdatt0d1_mcar)
    cat_spec <- catdatt0d0_mcar/(catdatt0d0_mcar + catdatt1d0_mcar)
    
    cat_sens_lower_wald <- cat_sens - z*sqrt((cat_sens*(1 - cat_sens))/length(newdata$D[newdata$D == 1]))
    cat_sens_lower_logit <- LogitInv(Logit(cat_sens) - z/sqrt(length(newdata$D[newdata$D == 1])*cat_sens*(1-cat_sens)))
    
    cat_sens_upper_wald <- cat_sens + z*sqrt((cat_sens*(1 - cat_sens))/length(newdata$D[newdata$D == 1]))
    cat_sens_upper_logit <- LogitInv(Logit(cat_sens) + z/sqrt(length(newdata$D[newdata$D == 1])*cat_sens*(1-cat_sens)))
    
    cat_spec_lower_wald <- cat_spec - z*sqrt((cat_spec*(1 - cat_spec))/length(newdata$D[newdata$D == 0]))
    cat_spec_lower_logit <- LogitInv(Logit(cat_spec) - z/sqrt(length(newdata$D[newdata$D == 0])*cat_spec*(1-cat_spec)))
    
    cat_spec_upper_wald <- cat_spec + z*sqrt((cat_spec*(1 - cat_spec))/length(newdata$D[newdata$D == 0]))
    cat_spec_upper_logit <- LogitInv(Logit(cat_spec) + z/sqrt(length(newdata$D[newdata$D == 0])*cat_spec*(1-cat_spec)))
    
    
    
    
    
    }
    
    #store results
    mlmcar <- c(i, cat_time, cat_sens, cat_sens, cat_sens_lower_logit, cat_sens_lower_wald, cat_sens_upper_logit, cat_sens_upper_wald,
                cat_spec, cat_spec, cat_spec_lower_logit, cat_spec_lower_wald, cat_spec_upper_logit, cat_spec_upper_wald, seed, tpf, tnf)
    
  }
}

#sore results as data frame and change column names
mlmcar_result_df <- as.data.frame(mlmcar_result)
colnames(mlmcar_result_df) <- c("Scenario", "Time", "Sens", "Sens2", "SensLogitL", "SensWaldL", "SensLogitU", "SensWaldU"
                             , "Spec", "Spec2", "SpecLogitL", "SpecWaldL", "SpecLogitU", "SpecWaldU", "Seed", "eTPF", "eTNF")


## MAR ----


rm(.Random.seed, envir=globalenv()) #remove seed

mlmar_result <- foreach(a = 1:nsim, .combine = rbind, .packages = c("vctrs", "DescTools", "foreach", "doParallel", "mvtnorm", "mice", "tidyverse")) %dopar% {
  
  ml_result_pre <- foreach(i = 1:nrow(grid), .combine = rbind) %do% {
    
    seed <- (1000*a) + i
    newdata <- data_fun(grid$N[i], grid$p[i], grid$mu[i], grid$c[i], grid$korr[i], grid$mech[i], grid$pm[i], c(0,1,1,1,1), seed) #generate data
    
    #save "true empirical" sensitivity and specificity
    tpf <- newdata$sens[1]
    tnf <- newdata$spec[1]
    
    if(sum(!is.na(newdata[which(newdata$D==1), "V1"]))==0 | sum(!is.na(newdata[which(newdata$D==0), "V1"]))==0){
      
      cat_sens <- NA
      cat_spec <- NA
      
    }else{
    
    
    
    catprep_cc <- c(
      
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
      
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
      
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1])),
      
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
      length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1]))
    )
    
    
    catprep_na <- c(
      
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),  
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),  
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
      
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1])),  
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),  
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
      length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
      length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1]))
    )
    
    catprep <- c(catprep_cc, catprep_na)
    catprep[which(catprep == 0)] <- 0.000001
    catdat <- readCatdata(TF = catprep, Zp = zp_a, Rp = c(16))
    
    t1 <- proc.time()
    catdatsat <- satMarML(catdat, missing = "MAR")
    t2 <- proc.time()
    cat_time <- t2[3] - t1[3]
    
    #extract imputed probabilities
    catdat_mcarimp <- catdatsat$theta
    
    #calculate frequencies for each relevant condition
    catdatt0d0_mcar <- sum(catdat_mcarimp[seq(from = 1, to = 8)])
    catdatt1d0_mcar <- sum(catdat_mcarimp[seq(from = 9, to = 16)]) 
    catdatt0d1_mcar <- sum(catdat_mcarimp[seq(from = 17, to = 24)])
    catdatt1d1_mcar <- sum(catdat_mcarimp[seq(from = 25, to = 32)])
    
    #calculate sensitivity and specificity
    cat_sens <- catdatt1d1_mcar/(catdatt1d1_mcar + catdatt0d1_mcar)
    cat_spec <- catdatt0d0_mcar/(catdatt0d0_mcar + catdatt1d0_mcar)
    
    cat_sens_lower_wald <- cat_sens - z*sqrt((cat_sens*(1 - cat_sens))/length(newdata$D[newdata$D == 1]))
    cat_sens_lower_logit <- LogitInv(Logit(cat_sens) - z/sqrt(length(newdata$D[newdata$D == 1])*cat_sens*(1-cat_sens)))
    
    cat_sens_upper_wald <- cat_sens + z*sqrt((cat_sens*(1 - cat_sens))/length(newdata$D[newdata$D == 1]))
    cat_sens_upper_logit <- LogitInv(Logit(cat_sens) + z/sqrt(length(newdata$D[newdata$D == 1])*cat_sens*(1-cat_sens)))
    
    cat_spec_lower_wald <- cat_spec - z*sqrt((cat_spec*(1 - cat_spec))/length(newdata$D[newdata$D == 0]))
    cat_spec_lower_logit <- LogitInv(Logit(cat_spec) - z/sqrt(length(newdata$D[newdata$D == 0])*cat_spec*(1-cat_spec)))
    
    cat_spec_upper_wald <- cat_spec + z*sqrt((cat_spec*(1 - cat_spec))/length(newdata$D[newdata$D == 0]))
    cat_spec_upper_logit <- LogitInv(Logit(cat_spec) + z/sqrt(length(newdata$D[newdata$D == 0])*cat_spec*(1-cat_spec)))
    
    }
    
    mlmcar <- c(i, cat_time, cat_sens, cat_sens, cat_sens_lower_logit, cat_sens_lower_wald, cat_sens_upper_logit, cat_sens_upper_wald,
                cat_spec, cat_spec, cat_spec_lower_logit, cat_spec_lower_wald, cat_spec_upper_logit, cat_spec_upper_wald, seed, tpf, tnf)
    
  }
}

mlmar_result_df <- as.data.frame(mlmar_result)
colnames(mlmar_result_df) <- c("Scenario", "Time", "Sens", "Sens2", "SensLogitL", "SensWaldL", "SensLogitU", "SensWaldU"
                               , "Spec", "Spec2", "SpecLogitL", "SpecWaldL", "SpecLogitU", "SpecWaldU", "Seed", "eTPF", "eTNF")


# WLS ----

### Weighted Least Squares method for missing categorical data. (package {ACD})

rm(.Random.seed, envir=globalenv()) #remove seed


wlsmcar_result <- foreach(a = 1:nsim, .combine = rbind, .packages = c("vctrs", "DescTools", "foreach", "doParallel", "mvtnorm", "mice", "tidyverse")) %dopar% {
  
  ml_result_pre <- foreach(i = 1:nrow(grid), .combine = rbind) %do% {
    
    
    seed <- (1000*a) + i
    
    #generate data
    newdata <- data_fun(grid$N[i], grid$p[i], grid$mu[i], grid$c[i], grid$korr[i], grid$mech[i], grid$pm[i], c(0,1,1,1,1), seed) 
    
    #save "true empirical" sensitivity and specificity
    tpf <- newdata$sens[1]
    tnf <- newdata$spec[1]
    
    #check if all test results for diseased/non-diseased patients are missing
    if(sum(!is.na(newdata[which(newdata$D==1), "V1"]))==0 | sum(!is.na(newdata[which(newdata$D==0), "V1"]))==0){
      
      cat_sens <- NA
      cat_sens_lower_logit <- NA
      cat_sens_lower_wald <- NA
      cat_sens_upper_logit <- NA
      cat_sens_upper_wald <- NA
      
      cat_spec <- NA
      cat_spec_lower_logit <- NA
      cat_spec_lower_wald <- NA
      cat_spec_upper_logit <- NA
      cat_spec_upper_wald <- NA
      
    }else{
      
      #"transform" data into a vector that represents a table for complete cases
      catprep_cc <- c(
        
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
        
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
        
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1])),
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
        length(na.omit(newdata$V1[newdata$V1 == 0 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1])),
        
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1])),
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
        length(na.omit(newdata$V1[newdata$V1 == 1 & newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1]))
      )
      
      #same step as before but with missing cases
      catprep_na <- c(
        
        length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),  
        length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),  
        length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
        length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
        length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 0])),
        length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 0])),
        length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 0])),
        length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 0])),
        
        length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1])),  
        length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),  
        length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
        length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
        length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 2 & newdata$D == 1])),
        length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 2 & newdata$V4 == 2 & newdata$D == 1])),
        length(is.na(newdata$V1[newdata$V2 == 1 & newdata$V3 == 2 & newdata$V4 == 1 & newdata$D == 1])),
        length(is.na(newdata$V1[newdata$V2 == 2 & newdata$V3 == 1 & newdata$V4 == 1 & newdata$D == 1]))
      )
      
      #transform "table" data into catdata
      catprep <- c(catprep_cc, catprep_na)
      catprep[which(catprep == 0)] <- 0.000001
      catdat <- readCatdata(TF = catprep, Zp = zp_a, Rp = c(16))
      
      
      t1 <- proc.time() #time start impute
      catdatsat <- satMcarWLS(catdat) 
               
      t2 <- proc.time() #time end impute
      cat_time <- t2[3] - t1[3]
      
      #extract imputed probabilities
      catdat_mcarimp <- catdatsat$theta
      
      #calculate frequencies for each relevant condition
      catdatt0d0_mcar <- sum(catdat_mcarimp[seq(from = 1, to = 8)])
      catdatt1d0_mcar <- sum(catdat_mcarimp[seq(from = 9, to = 16)]) 
      catdatt0d1_mcar <- sum(catdat_mcarimp[seq(from = 17, to = 24)])
      catdatt1d1_mcar <- sum(catdat_mcarimp[seq(from = 25, to = 32)])
      
      #calculate sensitivity and specificity and CIs
      cat_sens <- catdatt1d1_mcar/(catdatt1d1_mcar + catdatt0d1_mcar)
      cat_spec <- catdatt0d0_mcar/(catdatt0d0_mcar + catdatt1d0_mcar)
      
      cat_sens_lower_wald <- cat_sens - z*sqrt((cat_sens*(1 - cat_sens))/length(newdata$D[newdata$D == 1]))
      cat_sens_lower_logit <- LogitInv(Logit(cat_sens) - z/sqrt(length(newdata$D[newdata$D == 1])*cat_sens*(1-cat_sens)))
      
      cat_sens_upper_wald <- cat_sens + z*sqrt((cat_sens*(1 - cat_sens))/length(newdata$D[newdata$D == 1]))
      cat_sens_upper_logit <- LogitInv(Logit(cat_sens) + z/sqrt(length(newdata$D[newdata$D == 1])*cat_sens*(1-cat_sens)))
      
      cat_spec_lower_wald <- cat_spec - z*sqrt((cat_spec*(1 - cat_spec))/length(newdata$D[newdata$D == 0]))
      cat_spec_lower_logit <- LogitInv(Logit(cat_spec) - z/sqrt(length(newdata$D[newdata$D == 0])*cat_spec*(1-cat_spec)))
      
      cat_spec_upper_wald <- cat_spec + z*sqrt((cat_spec*(1 - cat_spec))/length(newdata$D[newdata$D == 0]))
      cat_spec_upper_logit <- LogitInv(Logit(cat_spec) + z/sqrt(length(newdata$D[newdata$D == 0])*cat_spec*(1-cat_spec)))
      
      
      
      
      
    }
    
    #store results
    wlsmcar <- c(i, cat_time, cat_sens, cat_sens, cat_sens_lower_logit, cat_sens_lower_wald, cat_sens_upper_logit, cat_sens_upper_wald,
                cat_spec, cat_spec, cat_spec_lower_logit, cat_spec_lower_wald, cat_spec_upper_logit, cat_spec_upper_wald, seed, tpf, tnf)
    
  }
}

#store results as data frame and change column names
wlsmcar_result_df <- as.data.frame(wlsmcar_result)
colnames(wlsmcar_result_df) <- c("Scenario", "Time", "Sens", "Sens2", "SensLogitL", "SensWaldL", "SensLogitU", "SensWaldU"
                                , "Spec", "Spec2", "SpecLogitL", "SpecWaldL", "SpecLogitU", "SpecWaldU", "Seed", "eTPF", "eTNF")



# SAVE RESULTS ----

save(cca_result_df, file = "cca.Rdata")
save(wc_result_df, file = "wc.Rdata")
save(rhd_result_df, file = "rhd.Rdata")
save(mice_result_df, file = "mice.Rdata")
save(mlmcar_result_df, file = "mlmcar.Rdata")
save(mlmar_result_df, file = "mlmar.Rdata")
save(wlsmcar_result_df, file = "wlsmcar.Rdata")



###ENDE###
TIME2 <- proc.time()

TIMEFULL <- TIME2[3] - TIME1[3]


print("Wir sind durch :)")


TIMEFULL

