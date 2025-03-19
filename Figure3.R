setwd("C:/Users/mglar/Desktop/Doctorado/R/Cromatogramas/23-02-27")
library(ggplot2)
data <- read.table("Aggregatedata_MMS.txt", header = T)
#Selects all the data from irradiated and unirradiated amino acids in MGS-1 and MMS-2
data1 <- read.table("MMSNo_Final.txt", header = T)
data2 <- read.table("MMSUV_Final.txt", header = T)
data3 <- read.table("MGSNo_Final.txt", header = T)
data4 <- read.table("MGSUV_Final.txt", header = T)
C <- rep(c(">500","250-500","150-250","45-150","<45"), each = 12)

data1$Condition <- C
data2$Condition <- C
data3$Condition <- C
data4$Condition <- C

q <- ggplot() +
  geom_point(data = data1,aes(x=factor(Condition, levels=c(">500","250-500","150-250","45-150","<45")), y=Ratio)) +
  geom_line(data = data1, aes(x=factor(Condition, levels=c(">500","250-500","150-250","45-150","<45")), y=Ratio, group = 1), size = 1.5, color = "#56B4E9") +
  geom_errorbar(data = data1, aes(x =factor(Condition, levels=c(">500","250-500","150-250","45-150","<45")), ymin = Ratio-SD, ymax = Ratio+SD), width=.1) +
  ylim(0,1.2) +
  facet_wrap(~Aa) +
  geom_point(data =data2,aes(x=factor(Condition, levels=c(">500","250-500","150-250","45-150","<45")), y=Ratio)) +
  geom_line(data = data2,aes(x=factor(Condition, levels=c(">500","250-500","150-250","45-150","<45")), y=Ratio, group = 1), size = 1.5, color = "#D55E00") +
  geom_errorbar(data = data2, aes(x = factor(Condition, levels=c(">500","250-500","150-250","45-150","<45")), ymin = Ratio-SD, ymax = Ratio+SD), width=.1)

q + labs(title = "Amino acid preservation in irradiated (red) and unirradiated (blue) MMS-2 particle size ranges", 
         x = "Particle size (um)", y = "Relative abundance") + 
  theme(axis.text=element_text(size=10), axis.title=element_text(size=14),
        plot.title=element_text(size=16))

q <- ggplot() +
  geom_point(data = data3,aes(x=factor(Condition, levels=c(">500","250-500","150-250","45-150","<45")), y=Ratio)) +
  geom_line(data = data3, aes(x=factor(Condition, levels=c(">500","250-500","150-250","45-150","<45")), y=Ratio, group = 1), size = 1.5, color = "#56B4E9") +
  geom_errorbar(data = data3, aes(x =factor(Condition, levels=c(">500","250-500","150-250","45-150","<45")), ymin = Ratio-SD, ymax = Ratio+SD), width=.1) +
  ylim(0,1.2) +
  facet_wrap(~Aa) +
  geom_point(data =data4,aes(x=factor(Condition, levels=c(">500","250-500","150-250","45-150","<45")), y=Ratio)) +
  geom_line(data = data4,aes(x=factor(Condition, levels=c(">500","250-500","150-250","45-150","<45")), y=Ratio, group = 1), size = 1.5, color = "#D55E00") +
  geom_errorbar(data = data4, aes(x = factor(Condition, levels=c(">500","250-500","150-250","45-150","<45")), ymin = Ratio-SD, ymax = Ratio+SD), width=.1)

q + labs(title = "Amino acid preservation in irradiated (red) and unirradiated (blue) MGS-1 particle size ranges", 
         x = "Particle size (um)", y = "Relative abundance") + 
  theme(axis.text=element_text(size=10), axis.title=element_text(size=14),
        plot.title=element_text(size=16))





p <- ggplot(data,aes(x=Aa, y=Ratio, fill=Condition)) +
  geom_bar(stat="identity", position=position_dodge()) +
  #facet_wrap(~Sample) +
  geom_errorbar(aes(ymin = Ratio-SD, ymax = Ratio+SD), width=.4,
                position = position_dodge(width = 0.9)) +
  ylim(0,1.2) + 
  guides(fill=guide_legend(title="Temperature"))

p + labs(title = "Survival of amino acids in MMS-2 at either RT or -80ºC",
         x = "Amino acid", y = "Abundace compared to an unirradiated control") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title=element_text(size=20), legend.key.size = unit(1.5, 'cm'), 
        legend.text=element_text(size=12), legend.title=element_text(size=14))




#--------------------------------



