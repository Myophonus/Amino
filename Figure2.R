setwd("C:/Users/mglar/Desktop/Doctorado/R/Cromatogramas/23-03-09")
library(ggplot2)
data <- read.table("Joined.txt", header = T)

my_colors <- c("#56B4E9", "#009E73", "#D55E00" )

p <- ggplot(data,aes(x=Aa, y=log2(Ratio), fill = Condition, color ="black")) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = log2(Ratio-SD), ymax = log2(Ratio+SD)), width=.4,
                position = position_dodge(width = 0.9), color = "black") +
  geom_vline(xintercept = (0:15)+0.5) +
  scale_fill_manual(values = my_colors) +
  scale_color_manual(values=c("black"))

p + labs(title = "Amino acid preservation dependence on temperature and regolith interaction",
         x = "Amino acid", y = "Abundance compared to an unirradiated control - Log2") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title=element_text(size=20), legend.key.size = unit(1.5, 'cm'), 
        legend.text=element_text(size=12), legend.title=element_text(size=14))+
  labs(y = Relative ~ abundance ~ (Log[2])) + 
  scale_fill_discrete(name = "Condition", labels = c("BSA + MMS-2 (-80ºC)", "BSA + MMS-2 (UV) ", "BSA + UV"))




#--------------------------------

p <- ggplot(data,aes(x=Condition, y=Ratio, fill=Aa)) +
  geom_point(aes(color = Aa)) +
  geom_line(aes(color = Aa, group = Aa), size = 1) +
  ylim(0.5,1.25) +
  facet_wrap(~Aa) +
  geom_errorbar(aes(ymin = Ratio-SD, ymax = Ratio+SD), width=.1)

p + labs(title = "Survival of amino acids adsorbed to different MMS-2 particle sizes under UV irradiation - BSA/MMS-2 controlled", 
         x = "Particle size (mm)", y = "Abundace compared to an unirradiated control for each particle size") + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=14),
        plot.title=element_text(size=16))


