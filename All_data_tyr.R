setwd("C:/Users/mglar/Desktop/Doctorado/Raman/2024-03-12 Tyr")
library(ggplot2)
library(tidyverse)
library(dplyr)
library("RColorBrewer")
library(vctrs)
library(zoo)
setwd("C:/Users/mglar/Desktop/Doctorado/Raman/2024-03-12 Tyr")
library(rstatix)
library(ggpubr)

#Oli
IO1 <- read.table("Oli_Irr_1_Copy.txt", header = F)
IO2 <- read.table("Oli_Irr_2_Copy.txt", header = F)
IO3 <- read.table("Oli_Irr_3_Copy.txt", header = F)
IO4 <- read.table("Oli_Irr_4_Copy.txt", header = F)
IO5 <- read.table("Oli_Irr_5_Copy.txt", header = F)
IO6 <- read.table("Oli_Irr_6_Copy.txt", header = F)
IO7 <- read.table("Oli_Irr_7_Copy.txt", header = F)
IO8 <- read.table("Oli_Irr_8_Copy.txt", header = F)
IO9 <- read.table("Oli_Irr_9_Copy.txt", header = F)
IO10 <- read.table("Oli_Irr_10_Copy.txt", header = F)

NO1 <- read.table("Oli_No_Irr_1_Copy.txt", header = F)
NO2 <- read.table("Oli_No_Irr_2_Copy.txt", header = F)
NO3 <- read.table("Oli_No_Irr_3_Copy.txt", header = F)
NO4 <- read.table("Oli_No_Irr_4_Copy.txt", header = F)
NO5 <- read.table("Oli_No_Irr_5_Copy.txt", header = F)
NO6 <- read.table("Oli_No_Irr_6_Copy.txt", header = F)
NO7 <- read.table("Oli_No_Irr_7_Copy.txt", header = F)
NO8 <- read.table("Oli_No_Irr_8_Copy.txt", header = F)
NO9 <- read.table("Oli_No_Irr_9_Copy.txt", header = F)
NO10 <- read.table("Oli_No_Irr_10_Copy.txt", header = F)

#MGS
IG1 <- read.table("MGS_Irr_1_Copy.txt", header = F)
IG2 <- read.table("MGS_Irr_2_Copy.txt", header = F)
IG3 <- read.table("MGS_Irr_3_Copy.txt", header = F)
IG4 <- read.table("MGS_Irr_4_Copy.txt", header = F)
IG5 <- read.table("MGS_Irr_5_Copy.txt", header = F)
IG6 <- read.table("MGS_Irr_6_Copy.txt", header = F)
IG7 <- read.table("MGS_Irr_7_Copy.txt", header = F)
IG8 <- read.table("MGS_Irr_8_Copy.txt", header = F)
IG9 <- read.table("MGS_Irr_9_Copy.txt", header = F)
IG10 <- read.table("MGS_Irr_10_Copy.txt", header = F)

NG1 <- read.table("MGS_No_Irr_1_Copy.txt", header = F)
NG2 <- read.table("MGS_No_Irr_2_Copy.txt", header = F)
NG3 <- read.table("MGS_No_Irr_3_Copy.txt", header = F)
NG4 <- read.table("MGS_No_Irr_4_Copy.txt", header = F)
NG5 <- read.table("MGS_No_Irr_5_Copy.txt", header = F)
NG6 <- read.table("MGS_No_Irr_6_Copy.txt", header = F)
NG7 <- read.table("MGS_No_Irr_7_Copy.txt", header = F)
NG8 <- read.table("MGS_No_Irr_8_Copy.txt", header = F)
NG9 <- read.table("MGS_No_Irr_9_Copy.txt", header = F)
NG10 <- read.table("MGS_No_Irr_10_Copy.txt", header = F)

#MMS
IM1 <- read.table("MMS_Irr_1_Copy.txt", header = F)
IM2 <- read.table("MMS_Irr_2_Copy.txt", header = F)
IM3 <- read.table("MMS_Irr_3_Copy.txt", header = F)
IM4 <- read.table("MMS_Irr_4_Copy.txt", header = F)
IM5 <- read.table("MMS_Irr_5_Copy.txt", header = F)
IM6 <- read.table("MMS_Irr_6_Copy.txt", header = F)
IM7 <- read.table("MMS_Irr_7_Copy.txt", header = F)
IM8 <- read.table("MMS_Irr_8_Copy.txt", header = F)
IM9 <- read.table("MMS_Irr_9_Copy.txt", header = F)
IM10 <- read.table("MMS_Irr_10_Copy.txt", header = F)

NM1 <- read.table("MMS_No_Irr_1_Copy.txt", header = F)
NM2 <- read.table("MMS_No_Irr_2_Copy.txt", header = F)
NM3 <- read.table("MMS_No_Irr_3_Copy.txt", header = F)
NM4 <- read.table("MMS_No_Irr_4_Copy.txt", header = F)
NM5 <- read.table("MMS_No_Irr_5_Copy.txt", header = F)
NM6 <- read.table("MMS_No_Irr_6_Copy.txt", header = F)
NM7 <- read.table("MMS_No_Irr_7_Copy.txt", header = F)
NM8 <- read.table("MMS_No_Irr_8_Copy.txt", header = F)
NM9 <- read.table("MMS_No_Irr_9_Copy.txt", header = F)
NM10 <- read.table("MMS_No_Irr_10_Copy.txt", header = F)

#PRT
IP1 <- read.table("PRT_Irr_1_Copy.txt", header = F)
IP2 <- read.table("PRT_Irr_2_Copy.txt", header = F)
IP3 <- read.table("PRT_Irr_3_Copy.txt", header = F)
IP4 <- read.table("PRT_Irr_4_Copy.txt", header = F)
IP5 <- read.table("PRT_Irr_5_Copy.txt", header = F)
IP6 <- read.table("PRT_Irr_6_Copy.txt", header = F)
IP7 <- read.table("PRT_Irr_7_Copy.txt", header = F)
IP8 <- read.table("PRT_Irr_8_Copy.txt", header = F)
IP9 <- read.table("PRT_Irr_9_Copy.txt", header = F)
IP10 <- read.table("PRT_Irr_10_Copy.txt", header = F)

NP1 <- read.table("PRT_No_Irr_1_Copy.txt", header = F)
NP2 <- read.table("PRT_No_Irr_2_Copy.txt", header = F)
NP3 <- read.table("PRT_No_Irr_3_Copy.txt", header = F)
NP4 <- read.table("PRT_No_Irr_4_Copy.txt", header = F)
NP5 <- read.table("PRT_No_Irr_5_Copy.txt", header = F)
NP6 <- read.table("PRT_No_Irr_6_Copy.txt", header = F)
NP7 <- read.table("PRT_No_Irr_7_Copy.txt", header = F)
NP8 <- read.table("PRT_No_Irr_8_Copy.txt", header = F)
NP9 <- read.table("PRT_No_Irr_9_Copy.txt", header = F)
NP10 <- read.table("PRT_No_Irr_10_Copy.txt", header = F)



#funciones para añadir columnas y cambiar nombres de columna

DEC = function(x){
  x$Peak = sub("\\.\\d+$", "", x$Peak)
  return(x)
}

AddUV <- function(x){
  x["UV"]="72h"
  return(x)
}
AddNo <- function(x){
  x["UV"]="0h"
  return(x)
}

AddSubs <- function(x,y){
  x["Substrate"]=y
  return(x)
}
ChangeNames <- function(x) {
  names(x) <- c("Rep", "Peak", "Intensity", "UV", "Substrate")
  return(x)
}

Divide <- function(x,y) {
  x %>%
    mutate(x, V3 = V3/y)
}

#generas las listas de dataframes, añades los cambios y las unes

#olivine

list_io <- list(IO1,IO2,IO3,IO4,IO5,IO6,IO7,IO8,IO9,IO10)
list_no <- list(NO1,NO2,NO3,NO4,NO5,NO6,NO7,NO8,NO9,NO10)
list_io_d <- lapply(list_io, Divide, y = 12)
list_io_d <- lapply(list_io_d, AddUV)
list_no <- lapply(list_no, AddNo)
list_so <- c(list_io_d, list_no)
list_so <- lapply(list_so, AddSubs, y = "Olivine")
list_so <- lapply(list_so, ChangeNames)
data_so <- do.call("rbind", list_so)
data_so = data_so %>%
  mutate(cma = rollmean(Intensity, k = 11, fill = NA))


#MGS-1

list_IG <- list(IG1,IG2,IG3,IG4,IG5,IG6,IG7,IG8,IG9,IG10)
list_NG <- list(NG1,NG2,NG3,NG4,NG5,NG6,NG7,NG8,NG9,NG10)
list_IG_d <- lapply(list_IG, Divide, y = 8)
list_IG_d <- lapply(list_IG_d, AddUV)
list_NG <- lapply(list_NG, AddNo)
list_sg <- c(list_IG_d, list_NG)
list_sg <- lapply(list_sg, AddSubs, y = "MGS-1")
list_sg <- lapply(list_sg, ChangeNames)
data_sg <- do.call("rbind", list_sg)
data_sg = data_sg %>%
  mutate(cma = rollmean(Intensity, k = 11, fill = NA))

#MMS-2

list_IM <- list(IM1,IM2,IM3,IM4,IM5,IM6,IM7,IM8,IM9,IM10)
list_NM <- list(NM1,NM2,NM3,NM4,NM5,NM6,NM7,NM8,NM9,NM10)
list_IM_d <- lapply(list_IM, Divide, y = 6)
list_IM_d <- lapply(list_IM_d, AddUV)
list_NM <- lapply(list_NM, AddNo)
list_sm <- c(list_IM_d, list_NM)
list_sm <- lapply(list_sm, AddSubs, y = "MMS-2")
list_sm <- lapply(list_sm, ChangeNames)
data_sm <- do.call("rbind", list_sm)
data_sm = data_sm %>%
  mutate(cma = rollmean(Intensity, k = 11, fill = NA))

#PRT

list_IP <- list(IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8,IP9,IP10)
list_NP <- list(NP1,NP2,NP3,NP4,NP5,NP6,NP7,NP8,NP9,NP10)
list_IP_d <- lapply(list_IP, Divide, y = 4)
list_IP_d <- lapply(list_IP_d, AddUV)
list_NP <- lapply(list_NP, AddNo)
list_sp <- c(list_IP_d, list_NP)
list_sp <- lapply(list_sp, AddSubs, y = "PRT")
list_sp <- lapply(list_sp, ChangeNames)
data_sp <- do.call("rbind", list_sp)
data_sp = data_sp %>%
  mutate(cma = rollmean(Intensity, k = 11, fill = NA))

data <- rbind(data_so, data_sg, data_sm, data_sp)



#seleccionas los picos de interés

data_1510 <- subset(data, Peak >= 828 & Peak <= 830)
data_1458 <- subset(data, Peak >= 1140 & Peak <= 1141)
data_1285 <- subset(data, Peak >= 1613 & Peak <= 1615)


#datos de superficie
data_final <- list(data_1510,data_1458,data_1285)
data_final <- lapply(data_final, DEC)
data_final <- do.call("rbind", data_final)


#para ver los 3 picos interesantes con p.valor.

y <- c(6.05,5.9,5.75,5.6,5.45,5.30,5.15,5,4.8,4.6,4.4,4.2,4,3.75,3.5,3.25,3,2.5,2,1.5,1)

symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), 
                    symbols = c("****", "***", "**", "*", "ns"))
symnum.args <- list(cutpoints = c(0, 0.00001,0.01, 0.05, Inf), 
                    symbols = c("***", "**", "*", "ns"))

g <- ggplot(data_final,aes(x=as.factor(Peak), y=cma)) +
  geom_boxplot(aes(x=as.factor(Peak), y=cma, fill = UV)) +
  scale_y_continuous(trans='log10', limits = c(1,10000)) +
  #geom_jitter(data = data_final_depth, aes(x=as.factor(Peak), y=Intensity_c, color = UV, 
  #                                         size = as.factor(Depth), fill = UV), 
  #           shape = 21,colour = "black") +
  scale_size_manual(values=y) +
  facet_wrap(~Substrate) +
  scale_fill_grey(start = 0.4, end = 0.7)

g + stat_compare_means(aes(x = as.factor(Peak), y = cma, group = UV), 
                       label =  c("p.signif"), label.x = 1.5, method = "t.test",
                       symnum.args = symnum.args) + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16),
        plot.title=element_text(size=22), legend.key.size = unit(1, 'cm'), 
        legend.text=element_text(size=10), legend.title=element_text(size=12)) +
  scale_y_continuous(trans='log10', limits = c(10,1000)) + 
  labs(title = "Tyrosine surface boxplots", x = "Band (cm-1)", y = "Intensity (a.u.)") +
  labs(caption = "*** = 0-0.00001, ** = 0.00001-0.01, * = 0.01-0.05, ns = 0.05-1")


