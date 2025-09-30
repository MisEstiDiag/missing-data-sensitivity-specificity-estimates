colnames(average_Logitcoverage70_spec_cca_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness")
colnames(average_Logitcoverage80_spec_cca_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage90_spec_cca_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage70_spec_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage80_spec_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage90_spec_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage70_spec_mlmar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage80_spec_mlmar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage90_spec_mlmar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage70_spec_mlmcar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage80_spec_mlmcar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage90_spec_mlmcar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness")
colnames(average_Logitcoverage70_spec_rhd_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage80_spec_rhd_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage90_spec_rhd_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage70_spec_wc_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage80_spec_wc_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage90_spec_wc_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage70_spec_wlsmcar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage80_spec_wlsmcar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Logitcoverage90_spec_wlsmcar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness")


speccoverLogit <- rbind(
  average_Logitcoverage70_spec_cca_df,
  average_Logitcoverage80_spec_cca_df, 
  average_Logitcoverage90_spec_cca_df, 
  average_Logitcoverage70_spec_mice_df, 
  average_Logitcoverage80_spec_mice_df, 
  average_Logitcoverage90_spec_mice_df, 
  average_Logitcoverage70_spec_mlmar_df, 
  average_Logitcoverage80_spec_mlmar_df, 
  average_Logitcoverage90_spec_mlmar_df, 
  average_Logitcoverage70_spec_mlmcar_df, 
  average_Logitcoverage80_spec_mlmcar_df, 
  average_Logitcoverage90_spec_mlmcar_df,
  average_Logitcoverage70_spec_rhd_df, 
  average_Logitcoverage80_spec_rhd_df, 
  average_Logitcoverage90_spec_rhd_df, 
  average_Logitcoverage70_spec_wc_df, 
  average_Logitcoverage80_spec_wc_df, 
  average_Logitcoverage90_spec_wc_df, 
  average_Logitcoverage70_spec_wlsmcar_df, 
  average_Logitcoverage80_spec_wlsmcar_df, 
  average_Logitcoverage90_spec_wlsmcar_df
)

speccoverLogit
speccoverLogit$stat <- "cover"
speccoverLogit$mcse <- NA
speccoverLogit$Sensitivity <- rep(head(results$sensitivity, n = 729), 7)
speccoverLogit$Specificity <- rep(head(results$specificity, n = 729), 7)
speccoverLogit$Method <- c(rep("CCA", 729), rep("MICE", 729), rep("MLMAR", 729), rep("MLMCAR", 729),
                          rep("RHD", 729), rep("WC", 729), rep("WLSMCAR", 729))
speccoverLogit$`Missingness mechanism` <- speccoverLogit$Missingness
speccoverLogit$`Prevalence` <- speccoverLogit$proportion
speccoverLogit$`Sample size` <- speccoverLogit$samplesize
speccoverLogit$`Proportion of missings` <- speccoverLogit$proportionmissing

speccoverLogit <- speccoverLogit[, -c(2, 3, 4, 5, 6)]

speccoverLogitMCAR <- speccoverLogit[speccoverLogit$Missingness == "MCAR", ]
speccoverLogitMAR <- speccoverLogit[speccoverLogit$Missingness == "MAR", ]
speccoverLogitMNAR <- speccoverLogit[speccoverLogit$Missingness == "MNAR", ]


resultsmcar <- results %>% filter(Missingness == "MCAR")

specsimresmcar <- simsum(data = resultsmcar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                         by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

resultsmar <- results %>% filter(Missingness == "MAR")

specsimresmar <- simsum(data = resultsmar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                         by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

resultsmnar <- results %>% filter(Missingness == "MNAR")

specsimresmnar <- simsum(data = resultsmnar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                         by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

specsimresmcar$summ <- rbind(specsimresmcar$summ, speccoverLogitMCAR)
specsimresmar$summ <- rbind(specsimresmar$summ, speccoverLogitMAR)
specsimresmnar$summ <- rbind(specsimresmnar$summ, speccoverLogitMNAR)

autoplot(specsimresmcar, type = "nlp", stats = "cover")
autoplot(specsimresmar, type = "nlp", stats = "cover")
autoplot(specsimresmnar, type = "nlp", stats = "cover")

logitspeccovermcar <- autoplot(specsimresmcar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="logitspeccovermcar.pdf", plot=logitspeccovermcar, width=5262, height=3720, units = "px")



logitspeccovermar <- autoplot(specsimresmar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="logitspeccovermar.pdf", plot=logitspeccovermar, width=5262, height=3720, units = "px")


logitspeccovermnar <- autoplot(specsimresmnar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="logitspeccovermnar.pdf", plot=logitspeccovermnar, width=5262, height=3720, units = "px")




speclogitmcse <- sqrt((speccoverLogit$est * (1-speccoverLogit$est))/1000)
min(speclogitmcse)
max(speclogitmcse)
mean(speclogitmcse)
median(speclogitmcse)
