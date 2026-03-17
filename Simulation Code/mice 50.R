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
source("grid mice50.R") #dataframe with all scenarios

nsim <- 200 #number of simulations

#predictor matrix that is needed for mice
row1 <- c(0, 1, 1, 1, 1)
rowelse <- c(0, 0, 0, 0, 0)
prepred <- c(row1, rowelse, rowelse, rowelse, rowelse)
predictors <- t(matrix(prepred, nrow = 5, ncol = 5))

#number of imputations mice
m0 <- 50

#z value to calculate confidence intervals
z <- qnorm(0.975)

#parallel computing
cores <- detectCores()
cl <- makeCluster(cores - 2)
registerDoParallel(cl)

TIME1 <- proc.time()

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
mice_50_result_df <- as.data.frame(mice_result)
colnames(mice_50_result_df) <- c("Scenario", "Time", "Sens", "Sens2", "SensLogitL", "SensWaldL", "SensLogitU", "SensWaldU"
                              , "Spec", "Spec2", "SpecLogitL", "SpecWaldL", "SpecLogitU", "SpecWaldU", "Seed", "eTPF", "eTNF")

save(mice_50_result_df, file = "mice50.Rdata")
