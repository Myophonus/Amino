setwd("C:/Users/mglar/Desktop/Doctorado/Articulos mios/Prot-Aa HPLC y Raman/Revision 1/Análisis/Plots Raman nuevo preUV/")
library(ggplot2)
library(dplyr)
library(ggpubr)

data <- read.table("Datos2.txt", header = TRUE)
data <- subset(data, Aa != "Ser")
data$Size_Regolith <- paste(data$Size, data$Regolith, sep = "_")
data$Size <- factor(data$Size, levels = c("<0.045", "0.125-0.250", ">0.500"))



ggplot(data, aes(x = Size, y = Area, fill = Size_Regolith)) +
  geom_col() +
  facet_grid(cols = vars(Regolith), rows = vars(Aa) ) +
  scale_y_continuous() +
  scale_alpha_manual() +
  geom_errorbar(aes(ymin = Area-SD, ymax = Area+SD), width=.2) +
  labs(
    title = "", 
    x = "Particle size range", 
    y = "Peak area (LU*s)",
    fill = "Particle Size Range"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14), 
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 22),
    legend.key.size = unit(1, 'cm'), 
    legend.text = element_text(size = 14), 
    legend.title = element_text(size = 16)
  ) + 
  theme(strip.text = element_text(size = 14)) +
  scale_fill_manual(values = c(
    "<0.045_MGS-1" = "darkblue",
    "0.125-0.250_MGS-1" = "blue",
    ">0.500_MGS-1" = "lightblue",
    "<0.045_MMS-2" = "red",
    "0.125-0.250_MMS-2" = "orange",
    ">0.500_MMS-2" = "yellow"
  ))

