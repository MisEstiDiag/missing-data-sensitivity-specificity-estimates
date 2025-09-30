library(svglite)
library(rsimsum)
library(tidyverse)
library(ggfortify)
library(jtools)


### Load Data
load("cca.Rdata")
load("wc.Rdata")
load("rhd.Rdata")
load("mice.Rdata")
load("mlmcar.Rdata")
load("mlmar.Rdata")
load("wlsmcar.Rdata")

nsim <- 1000

spec70 <- c(1:243)
spec80 <- c(244:486)
spec90 <- c(487:729)

cca_result_df$Method <- "CCA"
wc_result_df$Method <- "WC"
rhd_result_df$Method <- "RHD"
mice_result_df$Method <- "MICE"
mlmcar_result_df$Method <- "MLMCAR"
mlmar_result_df$Method <- "MLMAR"
wlsmcar_result_df$Method <- "WLSMCAR"


results <- rbind(cca_result_df, wc_result_df, rhd_result_df, mice_result_df, 
                 mlmcar_result_df, mlmar_result_df, wlsmcar_result_df)

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




### ALL MISSINGNESS MECHANISMS -------


senssimres <- simsum(data = results, estvarname = "Sens", true = "sensitivity", methodvar = "Method", ci.limits = c("SensWaldL", "SensWaldU"),
                     by = c("missingness mechanism", "samplesize",  "specificity", "sensitivity", "condition proportion",  "proportion of missings"))

specsimres <- simsum(data = results, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                     by = c("missingness mechanism", "samplesize", "sensitivity", "specificity", "condition proportion", "proportion of missings"))

sensbiasgesamt <- autoplot(senssimres, type = "nlp", stats = "rbias") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) +
  theme(legend.key.width = unit(3, 'cm'), legend.text = element_text(size=20), axis.text = element_text(size = 20), legend.title = element_text(size=20),
        text = element_text(size = 25), plot.caption = element_text(size = 25), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), line = element_line(linewidth = 0.3)) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Bias")

ggsave(file="sensbiasgesamt.pdf", plot=sensbiasgesamt, width=5262, height=3720, units = "px")


dev.off()
?autoplot
?theme

png("C:/Users/juljugin/Desktop/neue nested loop plots/1 Sensitivity MSE.png", 
    width = 1900, height = 1080, res = 110 )

autoplot(senssimres, type = "nlp", stats = "mse") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=12),          panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + ggtitle("MSE Sensitivity") + scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) 

dev.off()

png("C:/Users/juljugin/Desktop/neue nested loop plots/1 Sensitivity EmpSE.png", 
    width = 1900, height = 1080, res = 110 )

autoplot(senssimres, type = "nlp", stats = "empse") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=12),          panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + ggtitle("EmpSE Sensitivity") + scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) 

dev.off()

png("C:/Users/juljugin/Desktop/neue nested loop plots/1 Specificity Bias.png", 
    width = 1900, height = 1080, res = 110 )

autoplot(specsimres, type = "nlp", stats = "bias") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=12),          
        panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  ggtitle("Bias Specificity") + scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Bias")

dev.off()

png("C:/Users/juljugin/Desktop/neue nested loop plots/1 Specificity MSE.png", 
    width = 1900, height = 1080, res = 110 )

autoplot(specsimres, type = "nlp", stats = "mse") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=12),          panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + ggtitle("MSE Specificity") + scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) 

dev.off()  

png("C:/Users/juljugin/Desktop/neue nested loop plots/1 Specificity EmpSE.png", 
    width = 1900, height = 1080, res = 110 )

autoplot(specsimres, type = "nlp", stats = "empse") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=12),          panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + ggtitle("EmpSE Specificity") + scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) 

dev.off()  

### MCAR ONLY -----

resultsmcar <- results %>% filter(Missingness == "MCAR")

senssimresmcar <- simsum(data = resultsmcar, estvarname = "Sens", true = "Sensitivity", methodvar = "Method", 
                         by = c("Missingness mechanism", "Specificity", "Sample size",  "Sensitivity", "Prevalence",  "Proportion of missings"))

sensbiasmcar <- autoplot(senssimresmcar, type = "nlp", stats = "bias") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Bias")

ggsave(file="sensbiasmcar.pdf", plot=sensbiasmcar, width=5262, height=3720, units = "px")


sensmsemcar <- autoplot(senssimresmcar, type = "nlp", stats = "mse") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Mean Squared Error")

ggsave(file="sensmsemcar.pdf", plot=sensmsemcar, width=5262, height=3720, units = "px")


senssimresmcar <- simsum(data = resultsmcar, estvarname = "Sens", true = "sensitivity", methodvar = "Method", 
                         by = c("Missingness mechanism", "Specificity", "Sensitivity", "Prevalence", "Sample size", "Proportion of missings"))

sensempmcar <- autoplot(senssimresmcar, type = "nlp", stats = "empse") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.01)) + labs(y = "Empirical Standard Error")

ggsave(file="sensempmcar.pdf", plot=sensempmcar, width=5262, height=3720, units = "px")


specsimresmcar <- simsum(data = resultsmcar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                         by = c("Missingness mechanism", "Sample size",  "Specificity", "Sensitivity", "Prevalence",  "Proportion of missings"))


specbiasmcar <- autoplot(specsimresmcar, type = "nlp", stats = "bias") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Bias")

ggsave(file="specbiasmcar.pdf", plot=specbiasmcar, width=5262, height=3720, units = "px")



specmsemcar <- autoplot(specsimresmcar, type = "nlp", stats = "mse") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Mean Squared Error")

ggsave(file="specmsemcar.pdf", plot=specmsemcar, width=5262, height=3720, units = "px")

specsimresmcar <- simsum(data = resultsmcar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                         by = c("Missingness mechanism", "Specificity", "Sensitivity", "Prevalence", "Sample size", "Proportion of missings"))


specempmcar <- autoplot(specsimresmcar, type = "nlp", stats = "empse") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.01)) + labs(y = "Empirical Standard Error")

ggsave(file="specempmcar.pdf", plot=specempmcar, width=5262, height=3720, units = "px")

### BIAS MAR ### ----

resultsmar <- results %>% filter(Missingness == "MAR")

senssimresmar <- simsum(data = resultsmar, estvarname = "Sens", true = "Sensitivity", methodvar = "Method", 
                        by = c("Missingness mechanism", "Specificity", "Sample size",  "Sensitivity", "Prevalence",  "Proportion of missings"))

sensbiasmar <- autoplot(senssimresmar, type = "nlp", stats = "bias") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Bias")

ggsave(file="sensbiasmar.pdf", plot=sensbiasmar, width=5262, height=3720, units = "px")


sensmsemar <- autoplot(senssimresmar, type = "nlp", stats = "mse") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Mean Squared Error")

ggsave(file="sensmsemar.pdf", plot=sensmsemar, width=5262, height=3720, units = "px")


senssimresmar <- simsum(data = resultsmar, estvarname = "Sens", true = "sensitivity", methodvar = "Method", 
                        by = c("Missingness mechanism", "Specificity", "Sensitivity", "Prevalence", "Sample size", "Proportion of missings"))

sensempmar <- autoplot(senssimresmar, type = "nlp", stats = "empse") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.01)) + labs(y = "Empirical Standard Error")

ggsave(file="sensempmar.pdf", plot=sensempmar, width=5262, height=3720, units = "px")


specsimresmar <- simsum(data = resultsmar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                        by = c("Missingness mechanism", "Sample size",  "Specificity", "Sensitivity", "Prevalence",  "Proportion of missings"))


specbiasmar <- autoplot(specsimresmar, type = "nlp", stats = "bias") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Bias")

ggsave(file="specbiasmar.pdf", plot=specbiasmar, width=5262, height=3720, units = "px")



specmsemar <- autoplot(specsimresmar, type = "nlp", stats = "mse") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Mean Squared Error")

ggsave(file="specmsemar.pdf", plot=specmsemar, width=5262, height=3720, units = "px")

specsimresmar <- simsum(data = resultsmar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                        by = c("Missingness mechanism", "Specificity", "Sensitivity", "Prevalence", "Sample size", "Proportion of missings"))


specempmar <- autoplot(specsimresmar, type = "nlp", stats = "empse") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.01)) + labs(y = "Empirical Standard Error")

ggsave(file="specempmar.pdf", plot=specempmar, width=5262, height=3720, units = "px")


### BIAS MNAR ### ----

resultsmnar <- results %>% filter(Missingness == "MNAR")

senssimresmnar <- simsum(data = resultsmnar, estvarname = "Sens", true = "Sensitivity", methodvar = "Method", 
                         by = c("Missingness mechanism", "Specificity", "Sample size",  "Sensitivity", "Prevalence",  "Proportion of missings"))

sensbiasmnar <- autoplot(senssimresmnar, type = "nlp", stats = "bias") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Bias")

ggsave(file="sensbiasmnar.pdf", plot=sensbiasmnar, width=5262, height=3720, units = "px")


sensmsemnar <- autoplot(senssimresmnar, type = "nlp", stats = "mse") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Mean Squared Error")

ggsave(file="sensmsemnar.pdf", plot=sensmsemnar, width=5262, height=3720, units = "px")


senssimresmnar <- simsum(data = resultsmnar, estvarname = "Sens", true = "sensitivity", methodvar = "Method", 
                         by = c("Missingness mechanism", "Specificity", "Sensitivity", "Prevalence", "Sample size", "Proportion of missings"))

sensempmnar <- autoplot(senssimresmnar, type = "nlp", stats = "empse") +
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.01)) + labs(y = "Empirical Standard Error")

ggsave(file="sensempmnar.pdf", plot=sensempmnar, width=5262, height=3720, units = "px")


specsimresmnar <- simsum(data = resultsmnar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                         by = c("Missingness mechanism", "Sample size",  "Specificity", "Sensitivity", "Prevalence",  "Proportion of missings"))


specbiasmnar <- autoplot(specsimresmnar, type = "nlp", stats = "bias") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Bias")

ggsave(file="specbiasmnar.pdf", plot=specbiasmnar, width=5262, height=3720, units = "px")



specmsemnar <- autoplot(specsimresmnar, type = "nlp", stats = "mse") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Mean Squared Error")

ggsave(file="specmsemnar.pdf", plot=specmsemnar, width=5262, height=3720, units = "px")

specsimresmnar <- simsum(data = resultsmnar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                         by = c("Missingness mechanism", "Specificity", "Sensitivity", "Prevalence", "Sample size", "Proportion of missings"))


specempmnar <- autoplot(specsimresmnar, type = "nlp", stats = "empse") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.01)) + labs(y = "Empirical Standard Error")

ggsave(file="specempmnar.pdf", plot=specempmnar, width=5262, height=3720, units = "px")


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
