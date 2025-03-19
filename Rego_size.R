setwd("C:/Users/mglar/Desktop/Doctorado/Regolitos marcianos")
library(ggplot2)
library(tidyverse)
library(dplyr)
library("RColorBrewer")
library(vctrs)
library(zoo)
library(rstatix)
library(ggpubr)
library(zoo)

data <- read.table("Fracciones_rego.txt", header = T)

g <- ggplot(data,aes(x=Range, y=Fraction, fill = Simulant, color = "black")) +
     geom_col() +
     facet_wrap(~Simulant) +
     scale_x_discrete(limits = c("<0.045","0.045-0.125","0.125-0.250","0.250-0.500",">0.500")) +
     scale_fill_manual(values=c("darkblue","darkorange")) +
     scale_color_manual(values=c("black"))


g  + labs(title = ""
       , x = "Particle size range (mm)", y = "Fraction (weight %)") + 
  guides(fill=guide_legend(title="Simulant")) +
  theme(text = element_text(size = 20)) + theme(legend.position = "none")
