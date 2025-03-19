setwd("C:/Users/mglar/Desktop/Doctorado/R/Placas/DTNB")
library(ggplot2)
data <- read.table("DTNB_MGSMMS.txt", header = T)

p <- ggplot(data = data, aes(x = Size, y = Consumption, fill = Substrate, color = "black")) +
  geom_col() +
  geom_errorbar(aes(ymin = Consumption-SD, ymax = Consumption+SD), width=.2) +
  ylim(0,0.81) +
  facet_wrap("Substrate") +
  scale_fill_manual(values=c("lightblue", "orange")) + theme(legend.position = "none") +
  scale_color_manual(values=c("black")) +
  scale_x_discrete(limits = c("<0.045","0.045-0.125","0.125-0.250","0.250-0.500",">0.500"))

p + labs(title = ""
           , x = "Particle size range (mm)", y = "DTT oxidized by each simulant (nmol DTT / mg)") + 
  guides(fill=guide_legend(title="Simulant")) +
  theme(text = element_text(size = 20))
        

        labs(title = "Determination of the oxidative pressure of each particle size", 
         x = "Particle size range (mm)", y = "DTT oxidized by the substrate (nmol DTT / mg MMS-2)") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title=element_text(size=20), legend.key.size = unit(1.5, 'cm'), 
        legend.text=element_text(size=12), legend.title=element_text(size=14)) 


