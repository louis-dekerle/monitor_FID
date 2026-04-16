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

###### EXCLUDE OBS WITHOUT SIZE  ###### 

data_monitor = data_monitor[(data_monitor$size!="" ) ,]
View(data_monitor)


#### n° of obs per size #######

table(data_monitor$size)

ggplot(data_monitor, aes(x=size)) + 
  geom_bar()

# take only obs in salaya
data_salaya =  data_monitor[(data_monitor$site=="salaya" ) ,]
View(data_salaya)

table(data_salaya$size)

ggplot(data_salaya, aes(x=size)) + 
  geom_bar()


# take only obs in phutthamonthon 
data_phuttha =  data_monitor[(data_monitor$site=="phutthamonthon" ) ,]
View(data_phuttha)

table(data_phuttha$size)

ggplot(data_phuttha, aes(x=size)) + 
  geom_bar()

#### Chi2 indenpendance test ####

chisq.test(x = data_monitor$size, y = data_monitor$site) # p = 0.03

#### contengency table ####

table(data_monitor$size, data_monitor$site)


