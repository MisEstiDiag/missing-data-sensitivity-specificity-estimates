colnames(average_Waldcoverage_sens_cca_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness")
colnames(average_Waldcoverage_sens_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage_sens_mlmar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage_sens_mlmcar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage_sens_rhd_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage_sens_wc_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage_sens_wlsmcar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 


senscoverWald <- rbind(
  average_Waldcoverage_sens_cca_df,
  average_Waldcoverage_sens_mice_df, 
  average_Waldcoverage_sens_mlmar_df, 
  average_Waldcoverage_sens_mlmcar_df, 
  average_Waldcoverage_sens_rhd_df, 
  average_Waldcoverage_sens_wc_df, 
  average_Waldcoverage_sens_wlsmcar_df
)

senscoverWald
senscoverWald$stat <- "cover"
senscoverWald$mcse <- NA
senscoverWald$Sensitivity <- rep(head(results$sensitivity, n = 729), 7)
senscoverWald$Specificity <- rep(head(results$specificity, n = 729), 7)
senscoverWald$Method <- c(rep("CCA", 729), rep("MICE", 729), rep("MLMAR", 729), rep("MLMCAR", 729),
                           rep("RHD", 729), rep("WC", 729), rep("WLSMCAR", 729))
senscoverWald$`Missingness mechanism` <- senscoverWald$Missingness
senscoverWald$`Prevalence` <- senscoverWald$proportion
senscoverWald$`Sample size` <- senscoverWald$samplesize
senscoverWald$`Proportion of missings` <- senscoverWald$proportionmissing

senscoverWald <- senscoverWald[, -c(2, 3, 4, 5, 6)]


senscoverWaldMCAR <- senscoverWald[senscoverWald$Missingness == "MCAR", ]
senscoverWaldMAR <- senscoverWald[senscoverWald$Missingness == "MAR", ]
senscoverWaldMNAR <- senscoverWald[senscoverWald$Missingness == "MNAR", ]

resultsmcar <- results %>% filter(Missingness == "MCAR")

senssimresmcar <- simsum(data = resultsmcar, estvarname = "Sens", true = "sensitivity", methodvar = "Method", 
                         by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

resultsmar <- results %>% filter(Missingness == "MAR")

senssimresmar <- simsum(data = resultsmar, estvarname = "Sens", true = "sensitivity", methodvar = "Method", 
                        by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

resultsmnar <- results %>% filter(Missingness == "MNAR")

senssimresmnar <- simsum(data = resultsmnar, estvarname = "Sens", true = "sensitivity", methodvar = "Method", 
                         by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

senssimresmcar$summ <- rbind(senssimresmcar$summ, senscoverWaldMCAR)
senssimresmar$summ <- rbind(senssimresmar$summ, senscoverWaldMAR)
senssimresmnar$summ <- rbind(senssimresmnar$summ, senscoverWaldMNAR)

autoplot(senssimresmcar, type = "nlp", stats = "cover")
autoplot(senssimresmar, type = "nlp", stats = "cover")
autoplot(senssimresmnar, type = "nlp", stats = "cover")


Waldsenscovermcar <- autoplot(senssimresmcar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="waldsenscovermcar.pdf", plot=Waldsenscovermcar, width=5262, height=3720, units = "px")


Waldsenscovermar <- autoplot(senssimresmar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="waldsenscovermar.pdf", plot=Waldsenscovermar, width=5262, height=3720, units = "px")


Waldsenscovermnar <- autoplot(senssimresmnar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="waldsenscovermnar.pdf", plot=Waldsenscovermnar, width=5262, height=3720, units = "px")


