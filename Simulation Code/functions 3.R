#function to generate data sets
data_fun <- function(N,p,mu,c,korr,mech,pm,mypattern,seed){
  sigma <- matrix(ncol=4, nrow=4, korr)
  diag(sigma) <- 1
  n1=N*p
  n0=N-n1
  set.seed(seed)
  X0 <-as.data.frame(rmvnorm(n0, mean = c(0, 0, 5, 35), sigma = sigma,
                             method=c("eigen"), pre0.9_9994 = FALSE, checkSymmetry = TRUE)) # non-diseased pop
  X1 <- as.data.frame(rmvnorm(n1, mean = c(mu, 0, 5, 35), sigma = sigma,
                              method=c("eigen"), pre0.9_9994 = FALSE, checkSymmetry = TRUE)) # diseased pop; V1 ist index test
  X0$D <- 0 # reference test non-diseased
  X1$D <- 1 # reference test diseased
  data <- rbind(X0, X1)
  
  #dichotomize variables
  data$V2 <- ifelse((data$V2<=0),1,2)
  data$V3 <- ifelse((data$V3<=5),1,2)
  data$V4 <- ifelse((data$V4<=35),1,2)
  
  #set cutoff c to dichotomize test results
  data$V1 <- ifelse((data$V1<=c),0,1)
  
  #calculate sensitivity and specificity without missings
  
  sens <- length(data$V1[data$V1 == 1 & data$D == 1])/length(data$D[data$D == 1])
  spec <- length(data$V1[data$V1 == 0 & data$D == 0])/length(data$D[data$D == 0])
  
  sens <- rep(sens, N)
  spec <- rep(spec, N)
  
  # insert missing values with ampute function (mice package)
  amp <- ampute(data, prop = pm, patterns = mypattern, mech = mech)
  data_mis <- amp$amp
  data_mis <- cbind(data_mis, sens, spec)
  return(data_mis)
}


#worst case scenario function
worstcase <- function(ds) {
  
  dsd0 <- ds %>% filter(D == 0)
  dsd1 <- ds %>% filter(D == 1)
  
  dsd0$V1[is.na(dsd0$V1)] <- 1
  dsd1$V1[is.na(dsd1$V1)] <- 0  
  
  ds.wc <- rbind(dsd0, dsd1)
  return(ds.wc)
  
}