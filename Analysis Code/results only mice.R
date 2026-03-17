library(svglite)
library(rsimsum)
library(tidyverse)
library(ggfortify)
library(jtools)


### Load Data
load("mice.Rdata")

nsim <- 1000

spec70 <- c(1:243)
spec80 <- c(244:486)
spec90 <- c(487:729)


mice_result_df$Method <- "MICE"


results <- rbind(mice_result_df)

for(i in 0:8){
  results$Missingness[results$Scenario == c(1:27)+i*81] <- "MCAR"
  results$Missingness[results$Scenario == c(28:54)+i*81] <- "MAR"
}

results$Missingness[is.na(results$Missingness)] <- "MNAR"

for (i in 0:26) {
  results$proportionmissing[results$Scenario == c(1:9) + i*27] <- 0.1
  results$proportionmissing[results$Scenario == c(10:18) + i*27] <- 0.3
  results$proportionmissing[results$Scenario == c(19:27) + i*27] <- 0.5
}

for (i in 0:80) {
  results$proportion[results$Scenario == c(1:3) + i*9] <- 0.1
  results$proportion[results$Scenario == c(4:6) + i*9] <- 0.2
  results$proportion[results$Scenario == c(7:9) + i*9] <- 0.4
}

for (i in 0:242) {
  results$samplesize[results$Scenario == 1 + i*3] <- 400
  results$samplesize[results$Scenario == 2 + i*3] <- 800
  results$samplesize[results$Scenario == 3 + i*3] <- 1600
}

for (i in 0:2){
  results$sensitivity[results$Scenario == c(1:81) + i*243] <- 0.9
  results$sensitivity[results$Scenario == c(82:162) + i*243] <- 0.8
  results$sensitivity[results$Scenario == c(163:243) + i*243] <- 0.7
}

results$specificity[results$Scenario == spec70] <- 0.7
results$specificity[results$Scenario == spec80] <- 0.8                     
results$specificity[results$Scenario == spec90] <- 0.9

results$`Missingness mechanism` <- results$Missingness
results$`Proportion of missings` <- results$proportionmissing
results$Prevalence <- results$proportion
results$`Sample size` <- results$samplesize
results$Sensitivity <- results$sensitivity
results$Specificity <- results$specificity





### BIAS MAR ### ----

resultsmar <- results %>% filter(Missingness == "MAR")

senssimresmar <- simsum(data = resultsmar, estvarname = "Sens", true = "Sensitivity", methodvar = "Method", 
                        by = c("Missingness mechanism", "Specificity", "Sample size",  "Sensitivity", "Prevalence",  "Proportion of missings"))

sensbiasmar <- autoplot(senssimresmar, type = "nlp", stats = "bias") +
  scale_color_manual(values=c("#1192E9")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.01)) + labs(y = "Bias")

ggsave(file="sensbiasmarONLYMICE.pdf", plot=sensbiasmar, width=5262, height=3720, units = "px")


specsimresmar <- simsum(data = resultsmar, estvarname = "Spec", true = "Specificity", methodvar = "Method", 
                        by = c("Missingness mechanism", "Specificity", "Sample size",  "Sensitivity", "Prevalence",  "Proportion of missings"))


specbiasmar <- autoplot(specsimresmar, type = "nlp", stats = "bias") + 
  scale_color_manual(values=c("#1192E9")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.01)) + labs(y = "Bias")

ggsave(file="specbiasmarONLYMICE.pdf", plot=specbiasmar, width=5262, height=3720, units = "px")





### BIAS MNAR ### ----

resultsmnar <- results %>% filter(Missingness == "MNAR")

senssimresmnar <- simsum(data = resultsmnar, estvarname = "Sens", true = "Sensitivity", methodvar = "Method", 
                         by = c("Missingness mechanism", "Specificity", "Sample size",  "Sensitivity", "Prevalence",  "Proportion of missings"))

sensbiasmnar <- autoplot(senssimresmnar, type = "nlp", stats = "bias") +
  scale_color_manual(values=c("#1192E9")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Bias")

ggsave(file="sensbiasmnarONLYMICE.pdf", plot=sensbiasmnar, width=5262, height=3720, units = "px")


specsimresmnar <- simsum(data = resultsmnar, estvarname = "Spec", true = "Specificity", methodvar = "Method", 
                        by = c("Missingness mechanism", "Specificity", "Sample size",  "Sensitivity", "Prevalence",  "Proportion of missings"))

specbiasmnar <- autoplot(specsimresmnar, type = "nlp", stats = "bias") + 
  scale_color_manual(values=c("#1192E9")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Bias")

ggsave(file="specbiasmnarONLYMICE.pdf", plot=specbiasmnar, width=5262, height=3720, units = "px")




## CI length ## ----

results$SensWaldlength <- results$SensWaldU - results$SensWaldL
results$SensLogitlength <- results$SensLogitU - results$SensLogitL


results$SpecWaldlength <- results$SpecWaldU - results$SpecWaldL
results$SpecLogitlength <- results$SpecLogitU - results$SpecLogitL


hist(results$SensWaldlength[results$Method == "MICE" & results$Missingness == "MCAR"])
hist(results$SensWaldlength[results$Method == "RHD" & results$Missingness == "MCAR"])


## Monte Carlo Error ## ----

sensbias <- tidy(senssimres, stats = "bias") 
mean(sensbias$mcse)
median(sensbias$mcse)
min(sensbias$mcse)
max(sensbias$mcse)

specbias <- tidy(specsimres, stats = "bias") 
mean(specbias$mcse)
median(specbias$mcse)
min(specbias$mcse)
max(specbias$mcse)

sensmse <- tidy(senssimres, stats = "mse") 
mean(sensmse$mcse)
median(sensmse$mcse)
min(sensmse$mcse)
max(sensmse$mcse)

specmse <- tidy(specsimres, stats = "mse") 
mean(specmse$mcse)
median(specmse$mcse)
min(specmse$mcse)
max(specmse$mcse)

sensempse <- tidy(senssimres, stats = "empse") 
mean(sensempse$mcse)
median(sensempse$mcse)
min(sensempse$mcse)
max(sensempse$mcse)

specempse <- tidy(specsimres, stats = "empse") 
mean(specempse$mcse)
median(specempse$mcse)
min(specempse$mcse)
max(specempse$mcse)

## TRUE VALUES ##

etpf70 <- as.data.frame(results$eTPF[results$sensitivity == 0.7])
colnames(etpf70) <- 'etpf70'

etpf80 <- as.data.frame(results$eTPF[results$sensitivity == 0.8])
colnames(etpf80) <- 'etpf80'

etpf90 <- as.data.frame(results$eTPF[results$sensitivity == 0.9])
colnames(etpf90) <- 'etpf90'


etpf70_hist <- ggplot(etpf70, aes(x=etpf70)) + geom_histogram(color = "black", fill = "#E5E4E2", bins = 100) +
  theme( axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
         text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black"), axis.line.x = element_line(color = "black")) +
  labs(x = "(Empirical) sensitivity before amputation", y = "Count") + scale_x_continuous(breaks = seq(from = 0.4, to = 1, by = 0.05))
ggsave(file="etpf70_hist.pdf", plot=etpf70_hist, width=2000, height=2000, units = "px")
etpf70_hist

ggplot(etpf80, aes(x=etpf80)) + geom_histogram(color = "black", fill = "#E5E4E2") +
  theme( axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
         text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black"), axis.line.x = element_line(color = "black")) +
  labs(x = "(Empirical) sensitivity before amputation", y = "Count") + scale_x_continuous(breaks = seq(from = 0.4, to = 1, by = 0.05))

ggplot(etpf90, aes(x=etpf90)) + geom_histogram(color = "black", fill = "#E5E4E2") +
  theme( axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
         text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black"), axis.line.x = element_line(color = "black")) +
  labs(x = "(Empirical) sensitivity before amputation", y = "Count") + scale_x_continuous(breaks = seq(from = 0.4, to = 1, by = 0.05))



etnf70 <- as.data.frame(results$eTNF[results$specificity == 0.7])
colnames(etnf70) <- 'etnf70'

etnf80 <- as.data.frame(results$eTNF[results$specificity == 0.8])
colnames(etnf80) <- 'etnf80'

etnf90 <- as.data.frame(results$eTNF[results$specificity == 0.9])
colnames(etnf90) <- 'etnf90'


ggplot(etnf70, aes(x=etnf70)) + geom_histogram(color = "black", fill = "#E5E4E2") +
  theme( axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
         text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black"), axis.line.x = element_line(color = "black")) +
  labs(x = "(Empirical) specificity before amputation", y = "Count") + scale_x_continuous(breaks = seq(from = 0.4, to = 1, by = 0.05))

ggplot(etnf80, aes(x=etnf80)) + geom_histogram(color = "black", fill = "#E5E4E2") +
  theme( axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
         text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black"), axis.line.x = element_line(color = "black")) +
  labs(x = "(Empirical) specificity before amputation", y = "Count") + scale_x_continuous(breaks = seq(from = 0.4, to = 1, by = 0.05))

ggplot(etnf90, aes(x=etnf90)) + geom_histogram(color = "black", fill = "#E5E4E2") +
  theme( axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
         text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black"), axis.line.x = element_line(color = "black")) +
  labs(x = "(Empirical) specificity before amputation", y = "Count") + scale_x_continuous(breaks = seq(from = 0.4, to = 1, by = 0.05))

## Confidence Interval Lengths ##


results$SensWaldU[results$SensWaldU > 1] <- 1
results$SpecWaldU[results$SpecWaldU > 1] <- 1
results$SensWaldL[results$SensWaldL < 0] <- 0
results$SpecWaldL[results$SpecWaldL < 0] <- 0

results$SensWaldlength <- results$SensWaldU - results$SensWaldL
results$SensLogitlength <- results$SensLogitU - results$SensLogitL

results$SpecWaldlength <- results$SpecWaldU - results$SpecWaldL
results$SpecLogitlength <- results$SpecLogitU - results$SpecLogitL

mean(results$SensWaldlength[results$Method == 'CCA' & !is.na(results$SensWaldlength)])
mean(results$SensWaldlength[results$Method == 'WC' & !is.na(results$SensWaldlength)])
mean(results$SensWaldlength[results$Method == 'RHD' & !is.na(results$SensWaldlength)])
mean(results$SensWaldlength[results$Method == 'MICE' & !is.na(results$SensWaldlength)])
mean(results$SensWaldlength[results$Method == 'MLMCAR' & !is.na(results$SensWaldlength)])
mean(results$SensWaldlength[results$Method == 'MLMAR' & !is.na(results$SensWaldlength)])
mean(results$SensWaldlength[results$Method == 'WLSMCAR' & !is.na(results$SensWaldlength)])

median(results$SensWaldlength[results$Method == 'CCA' & !is.na(results$SensWaldlength)])
median(results$SensWaldlength[results$Method == 'WC' & !is.na(results$SensWaldlength)])
median(results$SensWaldlength[results$Method == 'RHD' & !is.na(results$SensWaldlength)])
median(results$SensWaldlength[results$Method == 'MICE' & !is.na(results$SensWaldlength)])
median(results$SensWaldlength[results$Method == 'MLMCAR' & !is.na(results$SensWaldlength)])
median(results$SensWaldlength[results$Method == 'MLMAR' & !is.na(results$SensWaldlength)])
median(results$SensWaldlength[results$Method == 'WLSMCAR' & !is.na(results$SensWaldlength)])

max(results$SensWaldlength[results$Method == 'CCA' & !is.na(results$SensWaldlength)])
max(results$SensWaldlength[results$Method == 'WC' & !is.na(results$SensWaldlength)])
max(results$SensWaldlength[results$Method == 'RHD' & !is.na(results$SensWaldlength)])
max(results$SensWaldlength[results$Method == 'MICE' & !is.na(results$SensWaldlength)])
max(results$SensWaldlength[results$Method == 'MLMCAR' & !is.na(results$SensWaldlength)])
max(results$SensWaldlength[results$Method == 'MLMAR' & !is.na(results$SensWaldlength)])
max(results$SensWaldlength[results$Method == 'WLSMCAR' & !is.na(results$SensWaldlength)])

min(results$SensWaldlength[results$Method == 'CCA' & !is.na(results$SensWaldlength)])
min(results$SensWaldlength[results$Method == 'WC' & !is.na(results$SensWaldlength)])
min(results$SensWaldlength[results$Method == 'RHD' & !is.na(results$SensWaldlength)])
min(results$SensWaldlength[results$Method == 'MICE' & !is.na(results$SensWaldlength)])
min(results$SensWaldlength[results$Method == 'MLMCAR' & !is.na(results$SensWaldlength)])
min(results$SensWaldlength[results$Method == 'MLMAR' & !is.na(results$SensWaldlength)])
min(results$SensWaldlength[results$Method == 'WLSMCAR' & !is.na(results$SensWaldlength)])


#COVERAGE




colnames(average_Logitcoverage_sens_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 



senscoverLogit <- rbind(
  average_Logitcoverage_sens_mice_df
)

senscoverLogit
senscoverLogit$stat <- "cover"
senscoverLogit$mcse <- NA
senscoverLogit$Sensitivity <- rep(head(results$sensitivity, n = 729), 1)
senscoverLogit$Specificity <- rep(head(results$specificity, n = 729), 1)
senscoverLogit$Method <- c(rep("MICE", 729))
senscoverLogit$`Missingness mechanism` <- senscoverLogit$Missingness
senscoverLogit$`Prevalence` <- senscoverLogit$proportion
senscoverLogit$`Sample size` <- senscoverLogit$samplesize
senscoverLogit$`Proportion of missings` <- senscoverLogit$proportionmissing

senscoverLogit <- senscoverLogit[, -c(2, 3, 4, 5, 6)]

senscoverLogitMCAR <- senscoverLogit[senscoverLogit$Missingness == "MCAR", ]
senscoverLogitMAR <- senscoverLogit[senscoverLogit$Missingness == "MAR", ]
senscoverLogitMNAR <- senscoverLogit[senscoverLogit$Missingness == "MNAR", ]




resultsmcar <- results %>% filter(Missingness == "MCAR")

senssimresmcar <- simsum(data = resultsmcar, estvarname = "Sens", true = "sensitivity", methodvar = "Method", 
                         by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

resultsmar <- results %>% filter(Missingness == "MAR")

senssimresmar <- simsum(data = resultsmar, estvarname = "Sens", true = "sensitivity", methodvar = "Method", 
                        by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

resultsmnar <- results %>% filter(Missingness == "MNAR")

senssimresmnar <- simsum(data = resultsmnar, estvarname = "Sens", true = "sensitivity", methodvar = "Method", 
                         by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

senssimresmcar$summ <- rbind(senssimresmcar$summ, senscoverLogitMCAR)
senssimresmar$summ <- rbind(senssimresmar$summ, senscoverLogitMAR)
senssimresmnar$summ <- rbind(senssimresmnar$summ, senscoverLogitMNAR)

autoplot(senssimresmcar, type = "nlp", stats = "cover")
autoplot(senssimresmar, type = "nlp", stats = "cover")
autoplot(senssimresmnar, type = "nlp", stats = "cover")



logitsenscovermar <- autoplot(senssimresmar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#1192E9")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.01)) + labs(y = "Coverage")

ggsave(file="logitsenscovermarONLYMICE.pdf", plot=logitsenscovermar, width=5262, height=3720, units = "px")


logitsenscovermnar <- autoplot(senssimresmnar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#1192E9")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="logitsenscovermnarONLYMICE.pdf", plot=logitsenscovermnar, width=5262, height=3720, units = "px")

senslogitmcse <- sqrt((senscoverLogit$est * (1-senscoverLogit$est))/1000)
min(senslogitmcse)
max(senslogitmcse)
mean(senslogitmcse)
median(senslogitmcse)


#SPEC

colnames(average_Logitcoverage70_spec_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage80_spec_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage90_spec_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 



speccoverLogit <- rbind(
  average_Logitcoverage70_spec_mice_df, 
  average_Logitcoverage80_spec_mice_df, 
  average_Logitcoverage90_spec_mice_df 
  )

speccoverLogit
speccoverLogit$stat <- "cover"
speccoverLogit$mcse <- NA
speccoverLogit$Sensitivity <- rep(head(results$sensitivity, n = 729), 1)
speccoverLogit$Specificity <- rep(head(results$specificity, n = 729), 1)
speccoverLogit$Method <- c(rep("MICE", 729))
speccoverLogit$`Missingness mechanism` <- speccoverLogit$Missingness
speccoverLogit$`Prevalence` <- speccoverLogit$proportion
speccoverLogit$`Sample size` <- speccoverLogit$samplesize
speccoverLogit$`Proportion of missings` <- speccoverLogit$proportionmissing

speccoverLogit <- speccoverLogit[, -c(2, 3, 4, 5, 6)]


speccoverLogitMAR <- speccoverLogit[speccoverLogit$Missingness == "MAR", ]
speccoverLogitMNAR <- speccoverLogit[speccoverLogit$Missingness == "MNAR", ]




resultsmar <- results %>% filter(Missingness == "MAR")

specsimresmar <- simsum(data = resultsmar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                        by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

resultsmnar <- results %>% filter(Missingness == "MNAR")

specsimresmnar <- simsum(data = resultsmnar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                         by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))


specsimresmar$summ <- rbind(specsimresmar$summ, speccoverLogitMAR)
specsimresmnar$summ <- rbind(specsimresmnar$summ, speccoverLogitMNAR)



logitspeccovermar <- autoplot(specsimresmar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#1192E9")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.01)) + labs(y = "Coverage")

ggsave(file="logitspeccovermarONLYMICE.pdf", plot=logitspeccovermar, width=5262, height=3720, units = "px")


logitspeccovermnar <- autoplot(specsimresmnar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#1192E9")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="logitspeccovermnarONLYMICE.pdf", plot=logitspeccovermnar, width=5262, height=3720, units = "px")




speclogitmcse <- sqrt((speccoverLogit$est * (1-speccoverLogit$est))/1000)
min(speclogitmcse)
max(speclogitmcse)
mean(speclogitmcse)
median(speclogitmcse)



