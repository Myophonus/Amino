setwd("C:/Users/mglar/Desktop/Doctorado/Articulos mios/Prot-Aa HPLC y Raman/Revision 1/Análisis/Rehacer Raman Previo")
library(ggplot2)
library(dplyr)
library(ggpubr)

data <- read.table("Datos.txt", header = TRUE)
data <- subset(data, Aminoacid != "His")

# Clean and prepare
data <- data %>%
  filter(cma > 0) %>%
  mutate(
    Substrate = factor(Substrate, levels = c("Olivine", "MGS-1", "PRT", "MMS-2")),
    UV = factor(UV)
  )

levels(data$UV) <- c("0", "24")
  
# Main plot
ggplot(data, aes(x = Substrate, y = cma, fill = Substrate)) +
  geom_boxplot(aes(alpha = UV), position = position_dodge(width = 0.8)) +
  facet_grid(rows = vars(Aminoacid)) +
  scale_y_continuous(limits = c(1, 1000), trans = "log10") +
  scale_alpha_manual(values = c(0.6, 1)) +
  guides(alpha = guide_legend(override.aes = list(fill = "gray"))) +
  stat_compare_means(
    aes(group = UV),
    method = "t.test",
    label = "p.signif",
    hide.ns = TRUE
  ) +
  labs(
    title = "", 
    x = "Martian Regolith Simulant (MRS)", 
    y = "Raman intensity (a.u.)"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14), 
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 22),
    legend.key.size = unit(1, 'cm'), 
    legend.text = element_text(size = 14), 
    legend.title = element_text(size = 16)
  ) + theme(strip.text = element_text(size = 14)) + 
  labs(fill = "Simulant", alpha  = expression("UV Dose (MJ/" * m^2 * ")")) + 
  guides(fill = "none")

