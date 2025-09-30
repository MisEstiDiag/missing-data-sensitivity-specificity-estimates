colnames(average_Waldcoverage70_spec_cca_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness")
colnames(average_Waldcoverage80_spec_cca_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage90_spec_cca_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage70_spec_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage80_spec_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage90_spec_mice_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage70_spec_mlmar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage80_spec_mlmar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage90_spec_mlmar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage70_spec_mlmcar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage80_spec_mlmcar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage90_spec_mlmcar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness")
colnames(average_Waldcoverage70_spec_rhd_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage80_spec_rhd_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage90_spec_rhd_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage70_spec_wc_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage80_spec_wc_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage90_spec_wc_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage70_spec_wlsmcar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage80_spec_wlsmcar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness") 
colnames(average_Waldcoverage90_spec_wlsmcar_df) <- c("est", "samplesize", "proportion", "korr", "proportionmissing", "Missingness")


speccoverWald <- rbind(
  average_Waldcoverage70_spec_cca_df,
  average_Waldcoverage80_spec_cca_df, 
  average_Waldcoverage90_spec_cca_df, 
  average_Waldcoverage70_spec_mice_df, 
  average_Waldcoverage80_spec_mice_df, 
  average_Waldcoverage90_spec_mice_df, 
  average_Waldcoverage70_spec_mlmar_df, 
  average_Waldcoverage80_spec_mlmar_df, 
  average_Waldcoverage90_spec_mlmar_df, 
  average_Waldcoverage70_spec_mlmcar_df, 
  average_Waldcoverage80_spec_mlmcar_df, 
  average_Waldcoverage90_spec_mlmcar_df,
  average_Waldcoverage70_spec_rhd_df, 
  average_Waldcoverage80_spec_rhd_df, 
  average_Waldcoverage90_spec_rhd_df, 
  average_Waldcoverage70_spec_wc_df, 
  average_Waldcoverage80_spec_wc_df, 
  average_Waldcoverage90_spec_wc_df, 
  average_Waldcoverage70_spec_wlsmcar_df, 
  average_Waldcoverage80_spec_wlsmcar_df, 
  average_Waldcoverage90_spec_wlsmcar_df
)

speccoverWald
speccoverWald$stat <- "cover"
speccoverWald$mcse <- NA
speccoverWald$Sensitivity <- rep(head(results$sensitivity, n = 729), 7)
speccoverWald$Specificity <- rep(head(results$specificity, n = 729), 7)
speccoverWald$Method <- c(rep("CCA", 729), rep("MICE", 729), rep("MLMAR", 729), rep("MLMCAR", 729),
                           rep("RHD", 729), rep("WC", 729), rep("WLSMCAR", 729))
speccoverWald$`Missingness mechanism` <- speccoverWald$Missingness
speccoverWald$`Prevalence` <- speccoverWald$proportion
speccoverWald$`Sample size` <- speccoverWald$samplesize
speccoverWald$`Proportion of missings` <- speccoverWald$proportionmissing

speccoverWald <- speccoverWald[, -c(2, 3, 4, 5, 6)]

speccoverWaldMCAR <- speccoverWald[speccoverWald$Missingness == "MCAR", ]
speccoverWaldMAR <- speccoverWald[speccoverWald$Missingness == "MAR", ]
speccoverWaldMNAR <- speccoverWald[speccoverWald$Missingness == "MNAR", ]

resultsmcar <- results %>% filter(Missingness == "MCAR")

specsimresmcar <- simsum(data = resultsmcar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                         by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

resultsmar <- results %>% filter(Missingness == "MAR")

specsimresmar <- simsum(data = resultsmar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                        by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

resultsmnar <- results %>% filter(Missingness == "MNAR")

specsimresmnar <- simsum(data = resultsmnar, estvarname = "Spec", true = "specificity", methodvar = "Method", 
                         by = c("Missingness mechanism",  "Sensitivity", "Prevalence", "Specificity", "Sample size", "Proportion of missings"))

specsimresmcar$summ <- rbind(specsimresmcar$summ, speccoverWaldMCAR)
specsimresmar$summ <- rbind(specsimresmar$summ, speccoverWaldMAR)
specsimresmnar$summ <- rbind(specsimresmnar$summ, speccoverWaldMNAR)

autoplot(specsimresmcar, type = "nlp", stats = "cover")
autoplot(specsimresmar, type = "nlp", stats = "cover")
autoplot(specsimresmnar, type = "nlp", stats = "cover")

waldspeccovermcar <- autoplot(specsimresmcar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="waldspeccovermcar.pdf", plot=waldspeccovermcar, width=5262, height=3720, units = "px")



waldspeccovermar <- autoplot(specsimresmar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="waldspeccovermar.pdf", plot=waldspeccovermar, width=5262, height=3720, units = "px")


waldspeccovermnar <- autoplot(specsimresmnar, type = "nlp", stats = "cover") + 
  scale_color_manual(values=c("#6929C5", "#1192E9", "#B28500","#9F1853", "#FA4E56", "#002D9D", "#197F38")) + 
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black", linewidth = 0.5),          
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) + labs(y = "Coverage")

ggsave(file="waldspeccovermnar.pdf", plot=waldspeccovermnar, width=5262, height=3720, units = "px")