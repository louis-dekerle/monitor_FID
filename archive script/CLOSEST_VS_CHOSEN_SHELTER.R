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

###### EXCLUDE OBS WITHOUT CLOSEST SHELTER  ###### 

data_monitor = data_monitor[(data_monitor$closest_ref_type!="" ) ,]
View(data_monitor)

###### EXCLUDE OBS WITHOUT GONE SHELTER  ###### 

data_monitor = data_monitor[(data_monitor$gone_ref_type!="" ) ,]
View(data_monitor)

#### n° of obs per chosen shelter  #######

table(data_monitor$gone_ref_type)

ggplot(data_monitor, aes(x=gone_ref_type)) + 
  geom_bar()

#### n° of obs per closest shelter ####

table(data_monitor$closest_ref_type)

ggplot(data_monitor, aes(x=closest_ref_type)) + 
  geom_bar()

#### Chi2 indenpendance test ####

chisq.test(x = data_monitor$closest_ref_type, y = data_monitor$gone_ref_type) # p =

#### contengency table ####

table = table(data_monitor$closest_ref_type, data_monitor$gone_ref_type)
addmargins(table)
