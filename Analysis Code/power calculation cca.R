#MCAR

resultspowercca <- results[results$Method == 'CCA' & results$Missingness == "MCAR" & results$samplesize == 800 & results$proportion == 0.2 & 
                              results$proportionmissing == 0.5 & results$sensitivity == 0.8, ]


resultspowercca$senspowerat50[resultspowercca$SensLogitL > 0.5] <- 1
resultspowercca$senspowerat52.5[resultspowercca$SensLogitL > 0.525] <- 1
resultspowercca$senspowerat55[resultspowercca$SensLogitL > 0.55] <- 1
resultspowercca$senspowerat57.5[resultspowercca$SensLogitL > 0.575] <- 1
resultspowercca$senspowerat60[resultspowercca$SensLogitL > 0.6] <- 1
resultspowercca$senspowerat62.5[resultspowercca$SensLogitL > 0.625] <- 1
resultspowercca$senspowerat65[resultspowercca$SensLogitL > 0.65] <- 1
resultspowercca$senspowerat67.5[resultspowercca$SensLogitL > 0.675] <- 1
resultspowercca$senspowerat70[resultspowercca$SensLogitL > 0.7] <- 1
resultspowercca$senspowerat72.5[resultspowercca$SensLogitL > 0.725] <- 1
resultspowercca$senspowerat75[resultspowercca$SensLogitL > 0.75] <- 1
resultspowercca$senspowerat77.5[resultspowercca$SensLogitL > 0.775] <- 1
resultspowercca$senspowerat80[resultspowercca$SensLogitL > 0.8] <- 1


resultspowercca$senspowerat50[is.na(resultspowercca$senspowerat50)] <- 0
resultspowercca$senspowerat52.5[is.na(resultspowercca$senspowerat52.5)] <- 0
resultspowercca$senspowerat55[is.na(resultspowercca$senspowerat55)] <- 0
resultspowercca$senspowerat57.5[is.na(resultspowercca$senspowerat57.5)] <- 0
resultspowercca$senspowerat60[is.na(resultspowercca$senspowerat60)] <- 0
resultspowercca$senspowerat62.5[is.na(resultspowercca$senspowerat62.5)] <- 0
resultspowercca$senspowerat65[is.na(resultspowercca$senspowerat65)] <- 0
resultspowercca$senspowerat67.5[is.na(resultspowercca$senspowerat67.5)] <- 0
resultspowercca$senspowerat70[is.na(resultspowercca$senspowerat70)] <- 0
resultspowercca$senspowerat72.5[is.na(resultspowercca$senspowerat72.5)] <- 0
resultspowercca$senspowerat75[is.na(resultspowercca$senspowerat75)] <- 0
resultspowercca$senspowerat77.5[is.na(resultspowercca$senspowerat77.5)] <- 0
resultspowercca$senspowerat80[is.na(resultspowercca$senspowerat80)] <- 0



senspower <- c(
  mean(resultspowercca$senspowerat80, na.rm = TRUE),  
  mean(resultspowercca$senspowerat77.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat75, na.rm = TRUE),
  mean(resultspowercca$senspowerat72.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat70, na.rm = TRUE),
  mean(resultspowercca$senspowerat67.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat65, na.rm = TRUE),
  mean(resultspowercca$senspowerat62.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat60, na.rm = TRUE),
  mean(resultspowercca$senspowerat57.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat55, na.rm = TRUE),
  mean(resultspowercca$senspowerat52.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat50, na.rm = TRUE)
)

difference <- c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30)


senspowerdfccamcar <- data.frame(difference, senspower)

resultspowercca <- results[results$Method == 'CCA' & results$Missingness == "MCAR" & results$samplesize == 800 & results$proportion == 0.2 & 
                              results$proportionmissing == 0.5 & results$specificity == 0.8, ]


resultspowercca$specpowerat50[resultspowercca$SpecLogitL > 0.5] <- 1
resultspowercca$specpowerat52.5[resultspowercca$SpecLogitL > 0.525] <- 1
resultspowercca$specpowerat55[resultspowercca$SpecLogitL > 0.55] <- 1
resultspowercca$specpowerat57.5[resultspowercca$SpecLogitL > 0.575] <- 1
resultspowercca$specpowerat60[resultspowercca$SpecLogitL > 0.6] <- 1
resultspowercca$specpowerat62.5[resultspowercca$SpecLogitL > 0.625] <- 1
resultspowercca$specpowerat65[resultspowercca$SpecLogitL > 0.65] <- 1
resultspowercca$specpowerat67.5[resultspowercca$SpecLogitL > 0.675] <- 1
resultspowercca$specpowerat70[resultspowercca$SpecLogitL > 0.7] <- 1
resultspowercca$specpowerat72.5[resultspowercca$SpecLogitL > 0.725] <- 1
resultspowercca$specpowerat75[resultspowercca$SpecLogitL > 0.75] <- 1
resultspowercca$specpowerat77.5[resultspowercca$SpecLogitL > 0.775] <- 1
resultspowercca$specpowerat80[resultspowercca$SpecLogitL > 0.8] <- 1


resultspowercca$specpowerat50[is.na(resultspowercca$specpowerat50)] <- 0
resultspowercca$specpowerat52.5[is.na(resultspowercca$specpowerat52.5)] <- 0
resultspowercca$specpowerat55[is.na(resultspowercca$specpowerat55)] <- 0
resultspowercca$specpowerat57.5[is.na(resultspowercca$specpowerat57.5)] <- 0
resultspowercca$specpowerat60[is.na(resultspowercca$specpowerat60)] <- 0
resultspowercca$specpowerat62.5[is.na(resultspowercca$specpowerat62.5)] <- 0
resultspowercca$specpowerat65[is.na(resultspowercca$specpowerat65)] <- 0
resultspowercca$specpowerat67.5[is.na(resultspowercca$specpowerat67.5)] <- 0
resultspowercca$specpowerat70[is.na(resultspowercca$specpowerat70)] <- 0
resultspowercca$specpowerat72.5[is.na(resultspowercca$specpowerat72.5)] <- 0
resultspowercca$specpowerat75[is.na(resultspowercca$specpowerat75)] <- 0
resultspowercca$specpowerat77.5[is.na(resultspowercca$specpowerat77.5)] <- 0
resultspowercca$specpowerat80[is.na(resultspowercca$specpowerat80)] <- 0



specpower <- c(
  mean(resultspowercca$specpowerat80, na.rm = TRUE),  
  mean(resultspowercca$specpowerat77.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat75, na.rm = TRUE),
  mean(resultspowercca$specpowerat72.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat70, na.rm = TRUE),
  mean(resultspowercca$specpowerat67.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat65, na.rm = TRUE),
  mean(resultspowercca$specpowerat62.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat60, na.rm = TRUE),
  mean(resultspowercca$specpowerat57.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat55, na.rm = TRUE),
  mean(resultspowercca$specpowerat52.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat50, na.rm = TRUE)
)

difference <- c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30)

specpowerdfccamcar <- data.frame(difference, specpower)


#MAR


resultspowercca <- results[results$Method == 'CCA' & results$Missingness == "MAR" & results$samplesize == 800 & results$proportion == 0.2 & 
                             results$proportionmissing == 0.5 & results$sensitivity == 0.8, ]


resultspowercca$senspowerat50[resultspowercca$SensLogitL > 0.5] <- 1
resultspowercca$senspowerat52.5[resultspowercca$SensLogitL > 0.525] <- 1
resultspowercca$senspowerat55[resultspowercca$SensLogitL > 0.55] <- 1
resultspowercca$senspowerat57.5[resultspowercca$SensLogitL > 0.575] <- 1
resultspowercca$senspowerat60[resultspowercca$SensLogitL > 0.6] <- 1
resultspowercca$senspowerat62.5[resultspowercca$SensLogitL > 0.625] <- 1
resultspowercca$senspowerat65[resultspowercca$SensLogitL > 0.65] <- 1
resultspowercca$senspowerat67.5[resultspowercca$SensLogitL > 0.675] <- 1
resultspowercca$senspowerat70[resultspowercca$SensLogitL > 0.7] <- 1
resultspowercca$senspowerat72.5[resultspowercca$SensLogitL > 0.725] <- 1
resultspowercca$senspowerat75[resultspowercca$SensLogitL > 0.75] <- 1
resultspowercca$senspowerat77.5[resultspowercca$SensLogitL > 0.775] <- 1
resultspowercca$senspowerat80[resultspowercca$SensLogitL > 0.8] <- 1


resultspowercca$senspowerat50[is.na(resultspowercca$senspowerat50)] <- 0
resultspowercca$senspowerat52.5[is.na(resultspowercca$senspowerat52.5)] <- 0
resultspowercca$senspowerat55[is.na(resultspowercca$senspowerat55)] <- 0
resultspowercca$senspowerat57.5[is.na(resultspowercca$senspowerat57.5)] <- 0
resultspowercca$senspowerat60[is.na(resultspowercca$senspowerat60)] <- 0
resultspowercca$senspowerat62.5[is.na(resultspowercca$senspowerat62.5)] <- 0
resultspowercca$senspowerat65[is.na(resultspowercca$senspowerat65)] <- 0
resultspowercca$senspowerat67.5[is.na(resultspowercca$senspowerat67.5)] <- 0
resultspowercca$senspowerat70[is.na(resultspowercca$senspowerat70)] <- 0
resultspowercca$senspowerat72.5[is.na(resultspowercca$senspowerat72.5)] <- 0
resultspowercca$senspowerat75[is.na(resultspowercca$senspowerat75)] <- 0
resultspowercca$senspowerat77.5[is.na(resultspowercca$senspowerat77.5)] <- 0
resultspowercca$senspowerat80[is.na(resultspowercca$senspowerat80)] <- 0



senspower <- c(
  mean(resultspowercca$senspowerat80, na.rm = TRUE),  
  mean(resultspowercca$senspowerat77.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat75, na.rm = TRUE),
  mean(resultspowercca$senspowerat72.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat70, na.rm = TRUE),
  mean(resultspowercca$senspowerat67.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat65, na.rm = TRUE),
  mean(resultspowercca$senspowerat62.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat60, na.rm = TRUE),
  mean(resultspowercca$senspowerat57.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat55, na.rm = TRUE),
  mean(resultspowercca$senspowerat52.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat50, na.rm = TRUE)
)

difference <- c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30)


senspowerdfccamar <- data.frame(difference, senspower)

resultspowercca <- results[results$Method == 'CCA' & results$Missingness == "MAR" & results$samplesize == 800 & results$proportion == 0.2 & 
                             results$proportionmissing == 0.5 & results$specificity == 0.8, ]


resultspowercca$specpowerat50[resultspowercca$SpecLogitL > 0.5] <- 1
resultspowercca$specpowerat52.5[resultspowercca$SpecLogitL > 0.525] <- 1
resultspowercca$specpowerat55[resultspowercca$SpecLogitL > 0.55] <- 1
resultspowercca$specpowerat57.5[resultspowercca$SpecLogitL > 0.575] <- 1
resultspowercca$specpowerat60[resultspowercca$SpecLogitL > 0.6] <- 1
resultspowercca$specpowerat62.5[resultspowercca$SpecLogitL > 0.625] <- 1
resultspowercca$specpowerat65[resultspowercca$SpecLogitL > 0.65] <- 1
resultspowercca$specpowerat67.5[resultspowercca$SpecLogitL > 0.675] <- 1
resultspowercca$specpowerat70[resultspowercca$SpecLogitL > 0.7] <- 1
resultspowercca$specpowerat72.5[resultspowercca$SpecLogitL > 0.725] <- 1
resultspowercca$specpowerat75[resultspowercca$SpecLogitL > 0.75] <- 1
resultspowercca$specpowerat77.5[resultspowercca$SpecLogitL > 0.775] <- 1
resultspowercca$specpowerat80[resultspowercca$SpecLogitL > 0.8] <- 1


resultspowercca$specpowerat50[is.na(resultspowercca$specpowerat50)] <- 0
resultspowercca$specpowerat52.5[is.na(resultspowercca$specpowerat52.5)] <- 0
resultspowercca$specpowerat55[is.na(resultspowercca$specpowerat55)] <- 0
resultspowercca$specpowerat57.5[is.na(resultspowercca$specpowerat57.5)] <- 0
resultspowercca$specpowerat60[is.na(resultspowercca$specpowerat60)] <- 0
resultspowercca$specpowerat62.5[is.na(resultspowercca$specpowerat62.5)] <- 0
resultspowercca$specpowerat65[is.na(resultspowercca$specpowerat65)] <- 0
resultspowercca$specpowerat67.5[is.na(resultspowercca$specpowerat67.5)] <- 0
resultspowercca$specpowerat70[is.na(resultspowercca$specpowerat70)] <- 0
resultspowercca$specpowerat72.5[is.na(resultspowercca$specpowerat72.5)] <- 0
resultspowercca$specpowerat75[is.na(resultspowercca$specpowerat75)] <- 0
resultspowercca$specpowerat77.5[is.na(resultspowercca$specpowerat77.5)] <- 0
resultspowercca$specpowerat80[is.na(resultspowercca$specpowerat80)] <- 0



specpower <- c(
  mean(resultspowercca$specpowerat80, na.rm = TRUE),  
  mean(resultspowercca$specpowerat77.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat75, na.rm = TRUE),
  mean(resultspowercca$specpowerat72.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat70, na.rm = TRUE),
  mean(resultspowercca$specpowerat67.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat65, na.rm = TRUE),
  mean(resultspowercca$specpowerat62.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat60, na.rm = TRUE),
  mean(resultspowercca$specpowerat57.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat55, na.rm = TRUE),
  mean(resultspowercca$specpowerat52.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat50, na.rm = TRUE)
)

difference <- c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30)

specpowerdfccamar <- data.frame(difference, specpower)

#MNAR


resultspowercca <- results[results$Method == 'CCA' & results$Missingness == "MNAR" & results$samplesize == 800 & results$proportion == 0.2 & 
                             results$proportionmissing == 0.5 & results$sensitivity == 0.8, ]


resultspowercca$senspowerat50[resultspowercca$SensLogitL > 0.5] <- 1
resultspowercca$senspowerat52.5[resultspowercca$SensLogitL > 0.525] <- 1
resultspowercca$senspowerat55[resultspowercca$SensLogitL > 0.55] <- 1
resultspowercca$senspowerat57.5[resultspowercca$SensLogitL > 0.575] <- 1
resultspowercca$senspowerat60[resultspowercca$SensLogitL > 0.6] <- 1
resultspowercca$senspowerat62.5[resultspowercca$SensLogitL > 0.625] <- 1
resultspowercca$senspowerat65[resultspowercca$SensLogitL > 0.65] <- 1
resultspowercca$senspowerat67.5[resultspowercca$SensLogitL > 0.675] <- 1
resultspowercca$senspowerat70[resultspowercca$SensLogitL > 0.7] <- 1
resultspowercca$senspowerat72.5[resultspowercca$SensLogitL > 0.725] <- 1
resultspowercca$senspowerat75[resultspowercca$SensLogitL > 0.75] <- 1
resultspowercca$senspowerat77.5[resultspowercca$SensLogitL > 0.775] <- 1
resultspowercca$senspowerat80[resultspowercca$SensLogitL > 0.8] <- 1


resultspowercca$senspowerat50[is.na(resultspowercca$senspowerat50)] <- 0
resultspowercca$senspowerat52.5[is.na(resultspowercca$senspowerat52.5)] <- 0
resultspowercca$senspowerat55[is.na(resultspowercca$senspowerat55)] <- 0
resultspowercca$senspowerat57.5[is.na(resultspowercca$senspowerat57.5)] <- 0
resultspowercca$senspowerat60[is.na(resultspowercca$senspowerat60)] <- 0
resultspowercca$senspowerat62.5[is.na(resultspowercca$senspowerat62.5)] <- 0
resultspowercca$senspowerat65[is.na(resultspowercca$senspowerat65)] <- 0
resultspowercca$senspowerat67.5[is.na(resultspowercca$senspowerat67.5)] <- 0
resultspowercca$senspowerat70[is.na(resultspowercca$senspowerat70)] <- 0
resultspowercca$senspowerat72.5[is.na(resultspowercca$senspowerat72.5)] <- 0
resultspowercca$senspowerat75[is.na(resultspowercca$senspowerat75)] <- 0
resultspowercca$senspowerat77.5[is.na(resultspowercca$senspowerat77.5)] <- 0
resultspowercca$senspowerat80[is.na(resultspowercca$senspowerat80)] <- 0



senspower <- c(
  mean(resultspowercca$senspowerat80, na.rm = TRUE),  
  mean(resultspowercca$senspowerat77.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat75, na.rm = TRUE),
  mean(resultspowercca$senspowerat72.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat70, na.rm = TRUE),
  mean(resultspowercca$senspowerat67.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat65, na.rm = TRUE),
  mean(resultspowercca$senspowerat62.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat60, na.rm = TRUE),
  mean(resultspowercca$senspowerat57.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat55, na.rm = TRUE),
  mean(resultspowercca$senspowerat52.5, na.rm = TRUE),
  mean(resultspowercca$senspowerat50, na.rm = TRUE)
)

difference <- c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30)


senspowerdfccamnar <- data.frame(difference, senspower)

resultspowercca <- results[results$Method == 'CCA' & results$Missingness == "MNAR" & results$samplesize == 800 & results$proportion == 0.2 & 
                             results$proportionmissing == 0.5 & results$specificity == 0.8, ]


resultspowercca$specpowerat50[resultspowercca$SpecLogitL > 0.5] <- 1
resultspowercca$specpowerat52.5[resultspowercca$SpecLogitL > 0.525] <- 1
resultspowercca$specpowerat55[resultspowercca$SpecLogitL > 0.55] <- 1
resultspowercca$specpowerat57.5[resultspowercca$SpecLogitL > 0.575] <- 1
resultspowercca$specpowerat60[resultspowercca$SpecLogitL > 0.6] <- 1
resultspowercca$specpowerat62.5[resultspowercca$SpecLogitL > 0.625] <- 1
resultspowercca$specpowerat65[resultspowercca$SpecLogitL > 0.65] <- 1
resultspowercca$specpowerat67.5[resultspowercca$SpecLogitL > 0.675] <- 1
resultspowercca$specpowerat70[resultspowercca$SpecLogitL > 0.7] <- 1
resultspowercca$specpowerat72.5[resultspowercca$SpecLogitL > 0.725] <- 1
resultspowercca$specpowerat75[resultspowercca$SpecLogitL > 0.75] <- 1
resultspowercca$specpowerat77.5[resultspowercca$SpecLogitL > 0.775] <- 1
resultspowercca$specpowerat80[resultspowercca$SpecLogitL > 0.8] <- 1


resultspowercca$specpowerat50[is.na(resultspowercca$specpowerat50)] <- 0
resultspowercca$specpowerat52.5[is.na(resultspowercca$specpowerat52.5)] <- 0
resultspowercca$specpowerat55[is.na(resultspowercca$specpowerat55)] <- 0
resultspowercca$specpowerat57.5[is.na(resultspowercca$specpowerat57.5)] <- 0
resultspowercca$specpowerat60[is.na(resultspowercca$specpowerat60)] <- 0
resultspowercca$specpowerat62.5[is.na(resultspowercca$specpowerat62.5)] <- 0
resultspowercca$specpowerat65[is.na(resultspowercca$specpowerat65)] <- 0
resultspowercca$specpowerat67.5[is.na(resultspowercca$specpowerat67.5)] <- 0
resultspowercca$specpowerat70[is.na(resultspowercca$specpowerat70)] <- 0
resultspowercca$specpowerat72.5[is.na(resultspowercca$specpowerat72.5)] <- 0
resultspowercca$specpowerat75[is.na(resultspowercca$specpowerat75)] <- 0
resultspowercca$specpowerat77.5[is.na(resultspowercca$specpowerat77.5)] <- 0
resultspowercca$specpowerat80[is.na(resultspowercca$specpowerat80)] <- 0



specpower <- c(
  mean(resultspowercca$specpowerat80, na.rm = TRUE),  
  mean(resultspowercca$specpowerat77.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat75, na.rm = TRUE),
  mean(resultspowercca$specpowerat72.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat70, na.rm = TRUE),
  mean(resultspowercca$specpowerat67.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat65, na.rm = TRUE),
  mean(resultspowercca$specpowerat62.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat60, na.rm = TRUE),
  mean(resultspowercca$specpowerat57.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat55, na.rm = TRUE),
  mean(resultspowercca$specpowerat52.5, na.rm = TRUE),
  mean(resultspowercca$specpowerat50, na.rm = TRUE)
)

difference <- c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30)

specpowerdfccamnar <- data.frame(difference, specpower)
