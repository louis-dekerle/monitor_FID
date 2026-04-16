library(readxl)
library(dplyr)
library(ggplot2)
library(MASS) 
library(lmtest)
library(scales)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(lme4)
library(IOLS)
library(car)
library (DHARMa)
library(MuMIn)
library (nlme)
library(ade4)
library(psych)
library(ggplot2)
library(ggeffects)
library(sjPlot)
library(sjmisc)

setwd("C:/Users/LENOVO/Desktop/stage mahidol/stat")

/!\/!\/!\/!\/!\/!\
/!\ DO NOT FORGET TO UPDATE DATASET TO NEWEST VERSION /!\
/!\/!\/!\/!\/!\/!\

##### LOAD RAW DATASETS #####
data_monitor <- read.csv("C:/Users/LENOVO/Desktop/stage mahidol/stat/data_obs8.csv", header= T, sep = ";")
View(data_monitor)


###### EXCLUDE OBS OTHER THAN V_SALVA ######

data_monitor = data_monitor[(data_monitor$sp=="V_salva" ) ,]
View(data_monitor)

###### EXCLUDE OBS WITHOUT people_1Min  ###### 

data_monitor = data_monitor[!is.na(data_monitor$people_1min),]
View(data_monitor)

####### SQUARE ROOT TRANSFORMATION ######

data_monitor$people_1min_sqrt = sqrt(data_monitor$people_1min)
View (data_monitor)

#### n° of obs per size #######

table(data_monitor$site)

ggplot(data_monitor, aes(x=site)) + 
  geom_bar()

###### preliminary kruskal test and plot######

kruskal.test(data_monitor$people_1min_sqrt~as.factor(data_monitor$site), data=data_monitor) # p = 0.0005863

boxplot(data_monitor$people_1min_sqrt~data_monitor$site, xlab = "Site", ylab = "People 1 minutes after the observation")  






