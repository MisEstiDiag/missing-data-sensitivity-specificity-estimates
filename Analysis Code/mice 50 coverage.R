## Calculation of Coverage ##

library(foreach)
library(tidyverse)

grid = expand.grid(
  N = c(400, 800, 1600)
  , p = c(0.1, 0.2, 0.4)
  , korr = c(sin(pi*0.2))
  , pm = c(0.1, 0.3, 0.5)
  , mech = c("MAR", "MNAR")
  , stringsAsFactors = FALSE
)

## COVERAGE CALCULATION

# SPECIFICITY ----

results$Spec_Waldcoverage70[results$SpecWaldL <= 0.7 & results$SpecWaldU >= 0.7 & results$Scenario == c(1:162)] <- 1 
results$Spec_Waldcoverage80[results$SpecWaldL <= 0.8 & results$SpecWaldU >= 0.8 & results$Scenario == c(163:324)] <- 1
results$Spec_Waldcoverage90[results$SpecWaldL <= 0.9 & results$SpecWaldU >= 0.9 & results$Scenario == c(325:486)] <- 1 

results$Spec_Logitcoverage70[results$SpecLogitL <= 0.7 & results$SpecLogitU >= 0.7 & results$Scenario == c(1:162)] <- 1 
results$Spec_Logitcoverage80[results$SpecLogitL <= 0.8 & results$SpecLogitU >= 0.8 & results$Scenario == c(163:324)] <- 1
results$Spec_Logitcoverage90[results$SpecLogitL <= 0.9 & results$SpecLogitU >= 0.9 & results$Scenario == c(325:486)] <- 1

# Average Coverages Specificity

# Wald


average_Waldcoverage70_spec_mice <- foreach(i = 1:162, .combine = c) %do% {
  sum(results$Spec_Waldcoverage70[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}

average_Waldcoverage80_spec_mice <- foreach(i = 163:324, .combine = c) %do% {
  sum(results$Spec_Waldcoverage80[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}

average_Waldcoverage90_spec_mice <- foreach(i = 325:486, .combine = c) %do% {
  sum(results$Spec_Waldcoverage90[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}


# Logit


average_Logitcoverage70_spec_mice <- foreach(i = 1:162, .combine = c) %do% {
  sum(results$Spec_Logitcoverage70[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}

average_Logitcoverage80_spec_mice <- foreach(i = 163:324, .combine = c) %do% {
  sum(results$Spec_Logitcoverage80[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}

average_Logitcoverage90_spec_mice <- foreach(i = 325:486, .combine = c) %do% {
  sum(results$Spec_Logitcoverage90[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}



grid <- rbind(grid, grid, grid)
colnames(grid) <- c("samplesize", "proportion", "korr", "proportionmissing", "Missingness")

average_Logitcoverage70_spec_mice_df <- data.frame(average_Logitcoverage70_spec_mice, grid)
average_Logitcoverage80_spec_mice_df <- data.frame(average_Logitcoverage80_spec_mice, grid)
average_Logitcoverage90_spec_mice_df <- data.frame(average_Logitcoverage90_spec_mice, grid)

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



average_Waldcoverage_sens_mice <- foreach(i = 1:486, .combine = c) %do% {
  sum(results$Sens_Waldcoverage[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}

average_Logitcoverage_sens_mice <- foreach(i = 1:486, .combine = c) %do% {
  sum(results$Sens_Logitcoverage[results$Scenario == i & results$Method == "MICE"], na.rm = TRUE)/nsim
}

grid729 <- rbind(grid, grid, grid)


average_Logitcoverage_sens_mice_df <- data.frame(average_Logitcoverage_sens_mice, grid729)


average_Waldcoverage_sens_mice_df <- data.frame(average_Waldcoverage_sens_mice, grid729)



#NLP

#Logit


colnames(average_Logitcoverage_sens_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 

senscoverLogit <- rbind(average_Logitcoverage_sens_mice_df)

senscoverLogit
senscoverLogit$stat <- "cover"
senscoverLogit$mcse <- NA
senscoverLogit$Sensitivity <- rep(head(results$sensitivity, n = 486), 1)
senscoverLogit$Specificity <- rep(head(results$specificity, n = 486), 1)
senscoverLogit$Method <- c(rep("MICE", 486))
senscoverLogit$`Missingness mechanism` <- senscoverLogit$Missingness
senscoverLogit$`Prevalence` <- senscoverLogit$proportion
senscoverLogit$`Sample size` <- senscoverLogit$samplesize
senscoverLogit$`Proportion of missings` <- senscoverLogit$proportionmissing

senscoverLogit <- senscoverLogit[, -c(2, 3, 4, 5, 6)]


senscoverLogitMAR <- senscoverLogit[senscoverLogit$Missingness == "MAR", ]
senscoverLogitMNAR <- senscoverLogit[senscoverLogit$Missingness == "MNAR", ]





resultsmar <- results %>% filter(Missingness == "MAR")

senssimresmar <- simsum(data = resultsmar, estvarname = "Sens", true = "sensitivity", methodvar = "Method", 
                        by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

resultsmnar <- results %>% filter(Missingness == "MNAR")

senssimresmnar <- simsum(data = resultsmnar, estvarname = "Sens", true = "sensitivity", methodvar = "Method", 
                         by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

senssimresmar$summ <- rbind(senssimresmar$summ, senscoverLogitMAR)
senssimresmnar$summ <- rbind(senssimresmnar$summ, senscoverLogitMNAR)


autoplot(senssimresmar, type = "nlp", stats = "cover")
autoplot(senssimresmnar, type = "nlp", stats = "cover")


logitsenscovermar <- autoplot(senssimresmar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#1192E9")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.01)) + labs(y = "Coverage")

ggsave(file="logitsenscovermar50.pdf", plot=logitsenscovermar, width=5262, height=3720, units = "px")


logitsenscovermnar <- autoplot(senssimresmnar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#1192E9")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="logitsenscovermnar50.pdf", plot=logitsenscovermnar, width=5262, height=3720, units = "px")

senslogitmcse <- sqrt((senscoverLogit$est * (1-senscoverLogit$est))/1000)
min(senslogitmcse)
max(senslogitmcse)
mean(senslogitmcse)
median(senslogitmcse)

#Wald

colnames(average_Waldcoverage_sens_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 

senscoverWald <- rbind(average_Waldcoverage_sens_mice_df)

senscoverWald
senscoverWald$stat <- "cover"
senscoverWald$mcse <- NA
senscoverWald$Sensitivity <- rep(head(results$sensitivity, n = 486), 1)
senscoverWald$Specificity <- rep(head(results$specificity, n = 486), 1)
senscoverWald$Method <- c(rep("MICE", 486))
senscoverWald$`Missingness mechanism` <- senscoverWald$Missingness
senscoverWald$`Prevalence` <- senscoverWald$proportion
senscoverWald$`Sample size` <- senscoverWald$samplesize
senscoverWald$`Proportion of missings` <- senscoverWald$proportionmissing

senscoverWald <- senscoverWald[, -c(2, 3, 4, 5, 6)]


senscoverWaldMAR <- senscoverWald[senscoverWald$Missingness == "MAR", ]
senscoverWaldMNAR <- senscoverWald[senscoverWald$Missingness == "MNAR", ]





resultsmar <- results %>% filter(Missingness == "MAR")

senssimresmar <- simsum(data = resultsmar, estvarname = "Sens", true = "sensitivity", methodvar = "Method", 
                        by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

resultsmnar <- results %>% filter(Missingness == "MNAR")

senssimresmnar <- simsum(data = resultsmnar, estvarname = "Sens", true = "sensitivity", methodvar = "Method", 
                         by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

senssimresmar$summ <- rbind(senssimresmar$summ, senscoverWaldMAR)
senssimresmnar$summ <- rbind(senssimresmnar$summ, senscoverWaldMNAR)


autoplot(senssimresmar, type = "nlp", stats = "cover")
autoplot(senssimresmnar, type = "nlp", stats = "cover")


Waldsenscovermar <- autoplot(senssimresmar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#1192E9")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="Waldsenscovermar50.pdf", plot=Waldsenscovermar, width=5262, height=3720, units = "px")


Waldsenscovermnar <- autoplot(senssimresmnar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#1192E9")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="Waldsenscovermnar50.pdf", plot=Waldsenscovermnar, width=5262, height=3720, units = "px")

sensWaldmcse <- sqrt((senscoverWald$est * (1-senscoverWald$est))/1000)
min(sensWaldmcse)
max(sensWaldmcse)
mean(sensWaldmcse)
median(sensWaldmcse)

#Spec

colnames(average_Logitcoverage70_spec_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage80_spec_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage90_spec_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 


speccoverLogit <- rbind(average_Logitcoverage70_spec_mice_df,
                        average_Logitcoverage80_spec_mice_df,
                        average_Logitcoverage90_spec_mice_df)

speccoverLogit
speccoverLogit$stat <- "cover"
speccoverLogit$mcse <- NA
speccoverLogit$Sensitivity <- rep(head(results$sensitivity, n = 486), 1)
speccoverLogit$Specificity <- rep(head(results$specificity, n = 486), 1)
speccoverLogit$Method <- c(rep("MICE", 486))
speccoverLogit$`Missingness mechanism` <- speccoverLogit$Missingness
speccoverLogit$`Prevalence` <- speccoverLogit$proportion
speccoverLogit$`Sample size` <- speccoverLogit$samplesize
speccoverLogit$`Proportion of missings` <- speccoverLogit$proportionmissing

speccoverLogit <- speccoverLogit[, -c(2, 3, 4, 5, 6)]


speccoverLogitMAR <- speccoverLogit[speccoverLogit$Missingness == "MAR", ]
speccoverLogitMNAR <- speccoverLogit[speccoverLogit$Missingness == "MNAR", ]





resultsmar <- results %>% filter(Missingness == "MAR")

specsimresmar <- simsum(data = resultsmar, estvarname = "Spec", true = "Specificity", methodvar = "Method", 
                        by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

resultsmnar <- results %>% filter(Missingness == "MNAR")

specsimresmnar <- simsum(data = resultsmnar, estvarname = "Spec", true = "Specificity", methodvar = "Method", 
                         by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

specsimresmar$summ <- rbind(specsimresmar$summ, speccoverLogitMAR)
specsimresmnar$summ <- rbind(specsimresmnar$summ, speccoverLogitMNAR)


autoplot(specsimresmar, type = "nlp", stats = "cover")
autoplot(specsimresmnar, type = "nlp", stats = "cover")


logitspeccovermar <- autoplot(specsimresmar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#1192E9")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.05)) + labs(y = "Coverage")

ggsave(file="logitspeccovermar50.pdf", plot=logitspeccovermar, width=5262, height=3720, units = "px")


logitspeccovermnar <- autoplot(specsimresmnar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#1192E9")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="logitspeccovermnar50.pdf", plot=logitspeccovermnar, width=5262, height=3720, units = "px")

speclogitmcse <- sqrt((speccoverLogit$est * (1-speccoverLogit$est))/1000)
min(speclogitmcse)
max(speclogitmcse)
mean(speclogitmcse)
median(speclogitmcse)



