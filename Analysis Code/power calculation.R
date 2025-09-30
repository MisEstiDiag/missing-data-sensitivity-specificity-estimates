resultspowermice <- results[results$Method == 'MICE' & results$Missingness == "MCAR" & results$samplesize == 800 & results$proportion == 0.2 & 
                              results$proportionmissing == 0.5 & results$sensitivity == 0.8, ]


resultspowermice$senspowerat50[resultspowermice$SensLogitL > 0.5] <- 1
resultspowermice$senspowerat52.5[resultspowermice$SensLogitL > 0.525] <- 1
resultspowermice$senspowerat55[resultspowermice$SensLogitL > 0.55] <- 1
resultspowermice$senspowerat57.5[resultspowermice$SensLogitL > 0.575] <- 1
resultspowermice$senspowerat60[resultspowermice$SensLogitL > 0.6] <- 1
resultspowermice$senspowerat62.5[resultspowermice$SensLogitL > 0.625] <- 1
resultspowermice$senspowerat65[resultspowermice$SensLogitL > 0.65] <- 1
resultspowermice$senspowerat67.5[resultspowermice$SensLogitL > 0.675] <- 1
resultspowermice$senspowerat70[resultspowermice$SensLogitL > 0.7] <- 1
resultspowermice$senspowerat72.5[resultspowermice$SensLogitL > 0.725] <- 1
resultspowermice$senspowerat75[resultspowermice$SensLogitL > 0.75] <- 1
resultspowermice$senspowerat77.5[resultspowermice$SensLogitL > 0.775] <- 1
resultspowermice$senspowerat80[resultspowermice$SensLogitL > 0.8] <- 1


resultspowermice$senspowerat50[is.na(resultspowermice$senspowerat50)] <- 0
resultspowermice$senspowerat52.5[is.na(resultspowermice$senspowerat52.5)] <- 0
resultspowermice$senspowerat55[is.na(resultspowermice$senspowerat55)] <- 0
resultspowermice$senspowerat57.5[is.na(resultspowermice$senspowerat57.5)] <- 0
resultspowermice$senspowerat60[is.na(resultspowermice$senspowerat60)] <- 0
resultspowermice$senspowerat62.5[is.na(resultspowermice$senspowerat62.5)] <- 0
resultspowermice$senspowerat65[is.na(resultspowermice$senspowerat65)] <- 0
resultspowermice$senspowerat67.5[is.na(resultspowermice$senspowerat67.5)] <- 0
resultspowermice$senspowerat70[is.na(resultspowermice$senspowerat70)] <- 0
resultspowermice$senspowerat72.5[is.na(resultspowermice$senspowerat72.5)] <- 0
resultspowermice$senspowerat75[is.na(resultspowermice$senspowerat75)] <- 0
resultspowermice$senspowerat77.5[is.na(resultspowermice$senspowerat77.5)] <- 0
resultspowermice$senspowerat80[is.na(resultspowermice$senspowerat80)] <- 0



senspower <- c(
  mean(resultspowermice$senspowerat80, na.rm = TRUE),  
  mean(resultspowermice$senspowerat77.5, na.rm = TRUE),
  mean(resultspowermice$senspowerat75, na.rm = TRUE),
  mean(resultspowermice$senspowerat72.5, na.rm = TRUE),
  mean(resultspowermice$senspowerat70, na.rm = TRUE),
  mean(resultspowermice$senspowerat67.5, na.rm = TRUE),
  mean(resultspowermice$senspowerat65, na.rm = TRUE),
  mean(resultspowermice$senspowerat62.5, na.rm = TRUE),
  mean(resultspowermice$senspowerat60, na.rm = TRUE),
  mean(resultspowermice$senspowerat57.5, na.rm = TRUE),
  mean(resultspowermice$senspowerat55, na.rm = TRUE),
  mean(resultspowermice$senspowerat52.5, na.rm = TRUE),
  mean(resultspowermice$senspowerat50, na.rm = TRUE)
)

difference <- c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30)


plot(difference, senspower)

senspowerdf <- data.frame(difference, senspower)

powersens <- ggplot(data = senspowerdf, aes(x = difference, y = senspower)) +
  geom_point(size=2, shape=1) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black"), axis.line.x = element_line(color = "black")) +
  labs(x = "Difference between Null Hypothesis and True Value", y = "Power") + scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5))

ggsave(file="powersens.pdf", plot=powersens, width=2000, height=2000, units = "px")



resultspowermice <- results[results$Method == 'MICE' & results$Missingness == "MCAR" & results$samplesize == 800 & results$proportion == 0.2 & 
                              results$proportionmissing == 0.5 & results$specificity == 0.8, ]


resultspowermice$specpowerat50[resultspowermice$SpecLogitL > 0.5] <- 1
resultspowermice$specpowerat52.5[resultspowermice$SpecLogitL > 0.525] <- 1
resultspowermice$specpowerat55[resultspowermice$SpecLogitL > 0.55] <- 1
resultspowermice$specpowerat57.5[resultspowermice$SpecLogitL > 0.575] <- 1
resultspowermice$specpowerat60[resultspowermice$SpecLogitL > 0.6] <- 1
resultspowermice$specpowerat62.5[resultspowermice$SpecLogitL > 0.625] <- 1
resultspowermice$specpowerat65[resultspowermice$SpecLogitL > 0.65] <- 1
resultspowermice$specpowerat67.5[resultspowermice$SpecLogitL > 0.675] <- 1
resultspowermice$specpowerat70[resultspowermice$SpecLogitL > 0.7] <- 1
resultspowermice$specpowerat72.5[resultspowermice$SpecLogitL > 0.725] <- 1
resultspowermice$specpowerat75[resultspowermice$SpecLogitL > 0.75] <- 1
resultspowermice$specpowerat77.5[resultspowermice$SpecLogitL > 0.775] <- 1
resultspowermice$specpowerat80[resultspowermice$SpecLogitL > 0.8] <- 1


resultspowermice$specpowerat50[is.na(resultspowermice$specpowerat50)] <- 0
resultspowermice$specpowerat52.5[is.na(resultspowermice$specpowerat52.5)] <- 0
resultspowermice$specpowerat55[is.na(resultspowermice$specpowerat55)] <- 0
resultspowermice$specpowerat57.5[is.na(resultspowermice$specpowerat57.5)] <- 0
resultspowermice$specpowerat60[is.na(resultspowermice$specpowerat60)] <- 0
resultspowermice$specpowerat62.5[is.na(resultspowermice$specpowerat62.5)] <- 0
resultspowermice$specpowerat65[is.na(resultspowermice$specpowerat65)] <- 0
resultspowermice$specpowerat67.5[is.na(resultspowermice$specpowerat67.5)] <- 0
resultspowermice$specpowerat70[is.na(resultspowermice$specpowerat70)] <- 0
resultspowermice$specpowerat72.5[is.na(resultspowermice$specpowerat72.5)] <- 0
resultspowermice$specpowerat75[is.na(resultspowermice$specpowerat75)] <- 0
resultspowermice$specpowerat77.5[is.na(resultspowermice$specpowerat77.5)] <- 0
resultspowermice$specpowerat80[is.na(resultspowermice$specpowerat80)] <- 0



specpower <- c(
  mean(resultspowermice$specpowerat80, na.rm = TRUE),  
  mean(resultspowermice$specpowerat77.5, na.rm = TRUE),
  mean(resultspowermice$specpowerat75, na.rm = TRUE),
  mean(resultspowermice$specpowerat72.5, na.rm = TRUE),
  mean(resultspowermice$specpowerat70, na.rm = TRUE),
  mean(resultspowermice$specpowerat67.5, na.rm = TRUE),
  mean(resultspowermice$specpowerat65, na.rm = TRUE),
  mean(resultspowermice$specpowerat62.5, na.rm = TRUE),
  mean(resultspowermice$specpowerat60, na.rm = TRUE),
  mean(resultspowermice$specpowerat57.5, na.rm = TRUE),
  mean(resultspowermice$specpowerat55, na.rm = TRUE),
  mean(resultspowermice$specpowerat52.5, na.rm = TRUE),
  mean(resultspowermice$specpowerat50, na.rm = TRUE)
)

difference <- c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30)


plot(difference, specpower)

specpowerdf <- data.frame(difference, specpower)

powerspec <- ggplot(data = specpowerdf, aes(x = difference, y = specpower)) +
  geom_point(size=2, shape=1) +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black"), axis.line.x = element_line(color = "black")) +
  labs(x = "Difference between Null Hypothesis and True Value", y = "Power") + scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5))

ggsave(file="powerspec.pdf", plot=powerspec, width=2000, height=2000, units = "px")


specpowerdf$mcse <- sqrt((specpowerdf$specpower * (1 - specpowerdf$specpower))/1000)

mean(specpowerdf$mcse)
median(specpowerdf$mcse)
max(specpowerdf$mcse)
min(specpowerdf$mcse)

senspowerdf <- data.frame(difference, senspower)


senspowerdf$mcse <- sqrt((senspowerdf$senspower * (1 - senspowerdf$senspower))/1000)

mean(senspowerdf$mcse)
median(senspowerdf$mcse)
max(senspowerdf$mcse)
min(senspowerdf$mcse)

