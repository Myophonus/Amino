setwd("C:/Users/mglar/Desktop/Doctorado/Raman/2024-04-05 His, Met, Leu Measurements/10%ww Leu")
library(ggplot2)
library(tidyverse)
library(dplyr)
library("RColorBrewer")
library(vctrs)
library(zoo)
library(rstatix)
library(ggpubr)
library(zoo)


DEC = function(x){
  x$Peak = sub("\\.\\d+$", "", x$Peak)
  return(x)
}

#funciones para añadir columnas y cambiar nombres de columna

AddMGS <- function(x,y){
  x["Simulant"]="MGS-1"
  return(x)
  }
  
AddOli <- function(x,y){
    x["Simulant"]="Olivine"
    return(x)
}

AddMMS <- function(x,y){
  x["Simulant"]="MMS-2"
  return(x)
}

AddPRT <- function(x,y){
  x["Simulant"]="PRT"
  return(x)
}

ChangeNames <- function(x) {
  names(x) <- c("Peak", "Intensity", "Simulant")
  return(x)
}
Divide <- function(x) {
  x %>%
    mutate(x, V3 = V3 / 1)
}



AG1 <- read.table("MGS_No_Irr_1_Copy.txt", header = F)
AG2 <- read.table("MGS_No_Irr_2_Copy.txt", header = F)
AG3 <- read.table("MGS_No_Irr_3_Copy.txt", header = F)
AG4 <- read.table("MGS_No_Irr_4_Copy.txt", header = F)
AG5 <- read.table("MGS_No_Irr_5_Copy.txt", header = F)
AG6 <- read.table("MGS_No_Irr_6_Copy.txt", header = F)
AG7 <- read.table("MGS_No_Irr_7_Copy.txt", header = F)
AG8 <- read.table("MGS_No_Irr_8_Copy.txt", header = F)
AG9 <- read.table("MGS_No_Irr_9_Copy.txt", header = F)
AG10 <- read.table("MGS_No_Irr_10_Copy.txt", header = F)

list_AG <- list(AG1,AG2,AG3,AG4,AG5,AG6,AG7,AG8,AG9,AG10)
list_AG <- lapply(list_AG, AddMGS)

AO1 <- read.table("Oli_No_Irr_1_Copy.txt", header = F)
AO2 <- read.table("Oli_No_Irr_2_Copy.txt", header = F)
AO3 <- read.table("Oli_No_Irr_3_Copy.txt", header = F)
AO4 <- read.table("Oli_No_Irr_4_Copy.txt", header = F)
AO5 <- read.table("Oli_No_Irr_5_Copy.txt", header = F)
AO6 <- read.table("Oli_No_Irr_6_Copy.txt", header = F)
AO7 <- read.table("Oli_No_Irr_7_Copy.txt", header = F)
AO8 <- read.table("Oli_No_Irr_8_Copy.txt", header = F)
AO9 <- read.table("Oli_No_Irr_9_Copy.txt", header = F)
AO10 <- read.table("Oli_No_Irr_10_Copy.txt", header = F)


list_AO <- list(AO1,AO2,AO3,AO4,AO5,AO6,AO7,AO8,AO9,AO10)
list_AO <- lapply(list_AO, AddOli)

AM1 <- read.table("MMS_No_Irr_1_Copy.txt", header = F)
AM2 <- read.table("MMS_No_Irr_2_Copy.txt", header = F)
AM3 <- read.table("MMS_No_Irr_3_Copy.txt", header = F)
AM4 <- read.table("MMS_No_Irr_4_Copy.txt", header = F)
AM5 <- read.table("MMS_No_Irr_5_Copy.txt", header = F)
AM6 <- read.table("MMS_No_Irr_6_Copy.txt", header = F)
AM7 <- read.table("MMS_No_Irr_7_Copy.txt", header = F)
AM8 <- read.table("MMS_No_Irr_8_Copy.txt", header = F)
AM9 <- read.table("MMS_No_Irr_9_Copy.txt", header = F)
AM10 <- read.table("MMS_No_Irr_10_Copy.txt", header = F)

list_AM <- list(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10)
list_AM <- lapply(list_AM, AddMMS)

AP1 <- read.table("PRT_No_Irr_1_Copy.txt", header = F)
AP2 <- read.table("PRT_No_Irr_2_Copy.txt", header = F)
AP3 <- read.table("PRT_No_Irr_3_Copy.txt", header = F)
AP4 <- read.table("PRT_No_Irr_4_Copy.txt", header = F)
AP5 <- read.table("PRT_No_Irr_5_Copy.txt", header = F)
AP6 <- read.table("PRT_No_Irr_6_Copy.txt", header = F)
AP7 <- read.table("PRT_No_Irr_7_Copy.txt", header = F)
AP8 <- read.table("PRT_No_Irr_8_Copy.txt", header = F)
AP9 <- read.table("PRT_No_Irr_9_Copy.txt", header = F)
AP10 <- read.table("PRT_No_Irr_10_Copy.txt", header = F)

list_AP <- list(AP1,AP2,AP3,AP4,AP5,AP6,AP7)
list_AP <- lapply(list_AP, AddPRT)



#generas las listas de dataframes, añades los cambios y las unes
list_s <- c(list_AG, list_AO, list_AM, list_AP)
list_s <- lapply(list_s, ChangeNames)
data_s <- do.call("rbind", list_s)

data_s = data_s %>%
  mutate(cma = rollmean(Intensity, k = 11, fill = NA))

#jitterplot

p <- ggplot(data_s) +
  geom_jitter(aes(x=Peak, y=cma, fill = Simulant), 
              shape = 21,colour = "black") +
  scale_y_continuous(trans='log10', limits = c(1,10000)) +
  scale_size(range = c(0.1,0.8)) +
  xlim(750,1900)
p

#seleccionas los picos de interés


data_1510 <- subset(data_s, Peak >= 838 & Peak <= 840)
data_1458 <- subset(data_s, Peak >= 849 & Peak <= 851)
data_1285 <- subset(data_s, Peak >= 923 & Peak <= 925)
data_1198 <- subset(data_s, Peak >= 965 & Peak <= 967)
data_1154 <- subset(data_s, Peak >= 1082 & Peak <= 1084)
data_1001 <- subset(data_s, Peak >= 1337 & Peak <= 1339)

data_1510 <- subset(data_s, Peak >= 1343 & Peak <= 1345)
data_1458 <- subset(data_s, Peak >= 1140 & Peak <= 1142)
data_1285 <- subset(data_s, Peak >= 1458 & Peak <= 1460)


data_1510 <- subset(data_s, Peak >= 1343 & Peak <= 1345)
data_1458 <- subset(data_s, Peak >= 1140 & Peak <= 1142)
data_1285 <- subset(data_s, Peak >= 1458 & Peak <= 1460)


#datos de superficie
data_final <- list(data_1510,data_1458,data_1285, data_1198)

data_final <- lapply(data_final, DEC)
data_final <- do.call("rbind", data_final)

level_order <- c('Olivine', 'MGS-1', 'PRT', "MMS-2") 

#para ver los 6 picos interesantes con p.valor.


g <- ggplot(data_final,aes(x=as.factor(Peak), y=cma)) +
  geom_boxplot(aes(x=as.factor(Peak), y=cma, fill = factor(Simulant, level = level_order)))
#geom_jitter(data = data_final_depth, aes(x=as.factor(Peak), y=Intensity_c, color = UV, 
#                                         size = as.factor(Depth), fill = UV), 
#           shape = 21,colour = "black") +
  #scale_size_manual(values=y)

g + scale_y_continuous(trans='log10', limits = c(10,2000)) + 
  labs(title = "Band intensity differences between regolith simulants - Leucine"
       , x = "Peak (cm-1)", y = "Intensity (a.u.)") + 
  theme(plot.title = element_text(size=22)) + geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5,6.5,7.5), linetype = "dashed") + 
  guides(fill=guide_legend(title="Simulant")) +
  theme(text = element_text(size = 20))



