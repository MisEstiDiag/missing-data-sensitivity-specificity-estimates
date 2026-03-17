specpowerdfmicemcar$Method <- "MICE"
specpowerdfccamcar$Method <- "CCA"

senspowerdfmicemcar$Method <- "MICE"
senspowerdfccamcar$Method <- "CCA"

specpowerdfmicemar$Method <- "MICE"
specpowerdfccamar$Method <- "CCA"

senspowerdfmicemar$Method <- "MICE"
senspowerdfccamar$Method <- "CCA"

specpowerdfmicemnar$Method <- "MICE"
specpowerdfccamnar$Method <- "CCA"

senspowerdfmicemnar$Method <- "MICE"
senspowerdfccamnar$Method <- "CCA"

specpowerdfallmcar <- rbind.data.frame(specpowerdfmicemcar, specpowerdfccamcar)

senspowerdfallmcar <- rbind.data.frame(senspowerdfmicemcar, senspowerdfccamcar)

powersens <- ggplot(data = senspowerdfallmcar, aes(x = difference, y = senspower, color = Method)) +
  geom_point(size=3, shape=1) + geom_line() +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black"), axis.line.x = element_line(color = "black")) +
  labs(x = "Difference between Null Hypothesis and True Value", y = "Power") + scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5))

ggsave(file="powersensALLlinesMCAR.pdf", plot=powersens, width=2000, height=2000, units = "px")


powerspec <- ggplot(data = specpowerdfallmcar, aes(x = difference, y = specpower, color = Method)) +
  geom_point(size=3, shape=1) + geom_line() +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black"), axis.line.x = element_line(color = "black")) +
  labs(x = "Difference between Null Hypothesis and True Value", y = "Power") + scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5))

ggsave(file="powerspecALLlinesMCAR.pdf", plot=powerspec, width=2000, height=2000, units = "px")


specpowerdfallmar <- rbind.data.frame(specpowerdfmicemar, specpowerdfccamar)

senspowerdfallmar <- rbind.data.frame(senspowerdfmicemar, senspowerdfccamar)

powersens <- ggplot(data = senspowerdfallmar, aes(x = difference, y = senspower, color = Method)) +
  geom_point(size=3, shape=1) + geom_line() +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black"), axis.line.x = element_line(color = "black")) +
  labs(x = "Difference between Null Hypothesis and True Value", y = "Power") + scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5))

ggsave(file="powersensALLlinesMAR.pdf", plot=powersens, width=2000, height=2000, units = "px")


powerspec <- ggplot(data = specpowerdfallmar, aes(x = difference, y = specpower, color = Method)) +
  geom_point(size=3, shape=1) + geom_line() +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black"), axis.line.x = element_line(color = "black")) +
  labs(x = "Difference between Null Hypothesis and True Value", y = "Power") + scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5))

ggsave(file="powerspecALLlinesMAR.pdf", plot=powerspec, width=2000, height=2000, units = "px")


specpowerdfallmnar <- rbind.data.frame(specpowerdfmicemnar, specpowerdfccamnar)

senspowerdfallmnar <- rbind.data.frame(senspowerdfmicemnar, senspowerdfccamnar)

powersens <- ggplot(data = senspowerdfallmnar, aes(x = difference, y = senspower, color = Method)) +
  geom_point(size=3, shape=1) + geom_line() +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black"), axis.line.x = element_line(color = "black")) +
  labs(x = "Difference between Null Hypothesis and True Value", y = "Power") + scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5)) +
 scale_y_continuous(breaks = seq(from = 0, to = 1, by =0.25), limits = c(0, 1))

ggsave(file="powersensALLlinesMNAR.pdf", plot=powersens, width=2000, height=2000, units = "px")


powerspec <- ggplot(data = specpowerdfallmnar, aes(x = difference, y = specpower, color = Method)) +
  geom_point(size=3, shape=1) + geom_line() +
  theme(legend.key.width = unit(4, 'cm'), legend.text = element_text(size=14), legend.title = element_text(size=14), axis.title.x = element_text(size = 14),  axis.title.y = element_text(size = 14),         
        text = element_text(size = 12), panel.background = element_rect(fill = "white"), axis.line.y = element_line(color = "black"), axis.line.x = element_line(color = "black")) +
  labs(x = "Difference between Null Hypothesis and True Value", y = "Power") + scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5)) +
 scale_y_continuous(breaks = seq(from = 0, to = 1, by =0.25), limits = c(0, 1))

ggsave(file="powerspecALLlinesMNAR.pdf", plot=powerspec, width=2000, height=2000, units = "px")





