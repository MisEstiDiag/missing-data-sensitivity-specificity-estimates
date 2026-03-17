library(svglite)
library(rsimsum)
library(tidyverse)
library(ggfortify)
library(jtools)


### Load Data
load("mice50.Rdata")

nsim <- 200

spec70 <- c(1:162)
spec80 <- c(163:324)
spec90 <- c(325:486)

mice_50_result_df$Method <- "MICE"

results <- mice_50_result_df

for(i in 0:8){
  results$Missingness[results$Scenario == c(1:27)+i*54] <- "MAR"
}

results$Missingness[is.na(results$Missingness)] <- "MNAR"

for (i in 0:17) {
  results$proportionmissing[results$Scenario == c(1:9) + i*27] <- 0.1
  results$proportionmissing[results$Scenario == c(10:18) + i*27] <- 0.3
  results$proportionmissing[results$Scenario == c(19:27) + i*27] <- 0.5
}

for (i in 0:53) {
  results$proportion[results$Scenario == c(1:3) + i*9] <- 0.1
  results$proportion[results$Scenario == c(4:6) + i*9] <- 0.2
  results$proportion[results$Scenario == c(7:9) + i*9] <- 0.4
}

for (i in 0:161) {
  results$samplesize[results$Scenario == 1 + i*3] <- 400
  results$samplesize[results$Scenario == 2 + i*3] <- 800
  results$samplesize[results$Scenario == 3 + i*3] <- 1600
}

for (i in 0:2){
  results$sensitivity[results$Scenario == c(1:54) + i*162] <- 0.9
  results$sensitivity[results$Scenario == c(55:108) + i*162] <- 0.8
  results$sensitivity[results$Scenario == c(109:162) + i*162] <- 0.7
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


senssimres <- simsum(data = results, estvarname = "Sens", true = "Sensitivity", methodvar = "Method", 
                        by = c("Missingness mechanism", "Specificity", "Sample size",  "Sensitivity", "Prevalence",  "Proportion of missings"))

specsimres <- simsum(data = results, estvarname = "Spec", true = "Specificity", methodvar = "Method", 
                        by = c("Missingness mechanism", "Specificity", "Sample size",  "Sensitivity", "Prevalence",  "Proportion of missings"))

#MAR

resultsmar <- results %>% filter(Missingness == "MAR")

senssimresmar <- simsum(data = resultsmar, estvarname = "Sens", true = "Sensitivity", methodvar = "Method", 
                        by = c("Missingness mechanism", "Specificity", "Sample size",  "Sensitivity", "Prevalence",  "Proportion of missings"))

sensbiasmar <- autoplot(senssimresmar, type = "nlp", stats = "bias") +
  scale_color_manual(values=c("#1192E9")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.01)) + labs(y = "Bias")

ggsave(file="sensbiasmar.pdf", plot=sensbiasmar, width=5262, height=3720, units = "px")


specsimresmar <- simsum(data = resultsmar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                        by = c("Missingness mechanism", "Specificity", "Sample size",  "Sensitivity", "Prevalence",  "Proportion of missings"))


specbiasmar <- autoplot(specsimresmar, type = "nlp", stats = "bias") + 
  scale_color_manual(values=c("#1192E9")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.01)) + labs(y = "Bias")

ggsave(file="specbiasmar.pdf", plot=specbiasmar, width=5262, height=3720, units = "px")


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

ggsave(file="sensbiasmnar.pdf", plot=sensbiasmnar, width=5262, height=3720, units = "px")



specsimresmnar <- simsum(data = resultsmnar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                         by = c("Missingness mechanism", "Specificity", "Sample size",  "Sensitivity", "Prevalence",  "Proportion of missings"))


specbiasmnar <- autoplot(specsimresmnar, type = "nlp", stats = "bias") + 
  scale_color_manual(values=c("#1192E9")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Bias")

ggsave(file="specbiasmnar.pdf", plot=specbiasmnar, width=5262, height=3720, units = "px")


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




