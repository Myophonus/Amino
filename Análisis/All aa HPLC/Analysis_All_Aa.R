library(ggplot2)
library(dplyr)
data <- read.table("Datos.txt", header = T)
data$Size <- factor(data$Size, levels = c("<0.045", "0.125-0.250", ">0.500"))

# Function to compute t-test from summary stats
summary_ttest <- function(m1, sd1, n1, m2, sd2, n2) {
  se <- sqrt((sd1^2 / n1) + (sd2^2 / n2))
  tval <- (m1 - m2) / se
  df <- ((sd1^2 / n1 + sd2^2 / n2)^2) / (((sd1^2 / n1)^2 / (n1 - 1)) + ((sd2^2 / n2)^2 / (n2 - 1)))
  p <- 2 * pt(-abs(tval), df)
  return(p)
}

# Add sample size (adjust if different per group)
data$n <- 3

# Filter for MGS-1
dataG <- subset(data, Regolith == "MMS-2")

# Run t-tests for each Aa + Size
comparisons <- dataG %>%
  group_by(Aa, Size) %>%
  filter(n_distinct(UV) == 2) %>%
  summarise(
    pval = summary_ttest(
      Ratio[UV == unique(UV)[1]], SD[UV == unique(UV)[1]], n[UV == unique(UV)[1]],
      Ratio[UV == unique(UV)[2]], SD[UV == unique(UV)[2]], n[UV == unique(UV)[2]]
    ),
    .groups = "drop"
  ) %>%
  mutate(label = case_when(
    pval < 0.001 ~ "***",
    pval < 0.01 ~ "**",
    pval < 0.05 ~ "*",
    TRUE ~ "ns"
  ))

# Merge labels back to data for plotting
dataG_plot <- left_join(dataG, comparisons, by = c("Aa", "Size"))

# Plot with manual text annotations
q <- ggplot(dataG, aes(x = Size, y = Ratio, color = UV)) +
  facet_wrap(~Aa, ncol = 5) +
  geom_point(size = 2, shape = 16, position = position_dodge(0)) +
  geom_line(aes(group = interaction(UV, Aa)), size = 1, position = position_dodge(0)) +
  geom_errorbar(aes(ymin = Ratio - SD, ymax = Ratio + SD), width = 0.3, size = 0.75, position = position_dodge(0)) + 
  scale_color_manual(values = c("No" = "#56B4E9", "Yes" = "#D55E00")) +
  scale_x_discrete(labels = c("<0.045", "0.125–0.250", ">0.500")) +
  scale_x_discrete(labels = c(
    "<0.045" = "< 45",
    "0.125-0.250" = "125 – 250",
    ">0.500" = "> 500"
  )) +
  labs(x = "Particle size (μm)", y = "Relative abundance", color = "UV Irradiation") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_text(
    data = comparisons,
    aes(x = Size, y = 1.2 * max(dataG$Ratio), label = label),
    color = "black",
    inherit.aes = FALSE,
    size = 4
  )

print(q)

