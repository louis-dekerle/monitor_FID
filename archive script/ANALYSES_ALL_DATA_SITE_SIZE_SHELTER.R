dev.off()  

##### LIBRARY #####
library(car)
library(lubridate)
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS) 
library(lmtest)
library(scales)
library(tidyverse)
library(broom)
library(lme4)
library (DHARMa)
library(graphics)

setwd("C:/Users/LENOVO/Desktop/stage mahidol/stat")

data_monitor <- read.csv("C:/Users/LENOVO/Desktop/stage mahidol/stat/project/DATA/pro/data_all.csv", header= T, sep = ";")
View(data_monitor)

###### EXCLUDE OBS OTHER THAN V_SALVA ######
data_monitor = data_monitor[(data_monitor$sp=="V_salva" ) ,]
View(data_monitor)

###### EXCLUDE OBS WITHOUT SEASON  ###### 
data_monitor = data_monitor[!is.na(data_monitor$season),]
View(data_monitor)

###### EXCLUDE OBS WITHOUT FID  ###### 
data_monitor = data_monitor[!is.na(data_monitor$FIDi),]
View(data_monitor)

###### EXCLUDE OBS WITHOUT SIZE  ###### 
data_monitor = data_monitor[!is.na(data_monitor$size),]
View(data_monitor)

###### EXCLUDE OBS WITHOUT DISTANCE TO SHELTER  ###### 
data_monitor = data_monitor[!is.na(data_monitor$d_fr_refug),]
View(data_monitor)

##### ROUND FID and D_FR_SHERLTER FOR DRY SEASON #####

data_monitor$FIDi = round(data_monitor$FIDi) # 0.5 is rounded down 
data_monitor$d_fr_refug = round(data_monitor$d_fr_refug) # 0.5 is rounded down 


View(data_monitor)

##### STANDARDIZE THE DATES ##### 

dates = data_monitor$date

# lubridate automatically detects format
standardized_dates = parse_date_time(dates, orders = "dmy")

# Convert to date class 
standardized_dates = as.Date(standardized_dates)

standardized_dates

data_monitor$date = standardized_dates


############
#### FID VS SEASON ####
#################

##### mean and median FID for dry  and rainy season ##### 
summary(data_monitor[(data_monitor$season== "dry" ) ,]) #mean FID = 5.521 10/04
summary(data_monitor[(data_monitor$season== "rainy" ) ,]) #mean FID = 3.144 10/04

plot

#### n° of obs per season #######

table(data_monitor$season) # almost balanced 

ggplot(data_monitor, aes(x=season)) + 
  geom_bar()

##### distribution across the months #####

ggplot(data_monitor, aes(x=month)) + 
  geom_bar()

data_monitor$month = month (data_monitor$date)

boxplot(data_monitor$FIDi ~ data_monitor$month)

data_monitor$month = as.factor(data_monitor$month)


mod_month <- aov(FIDi ~ month +site, data = data_monitor) #using aov function cause otherwise Tukey doesn't work 
summary(mod_month)

TukeyHSD(mod_month) 

boxplot(FIDi~month + site , data=data_monitor, xlab = "Month", ylab = "FID (m)", col=c("red","red", "red","green","green","green","green","green","red","red", "red","green","green","green","green","green"))


###### preliminary kruskal test and plot######
kruskal.test(data_monitor$FIDi~as.factor(data_monitor$season), data=data_monitor) #significant difference 

boxplot(FIDi~season, data=data_monitor, xlab = "Seasons", ylab = "FID (m)")

#########
#### FID VS SITE #####
#########

##### we do not convert FID in square root since we reduced the quantitative analyses to distnce to shelter 

##### mean and median FID for salaya and phutthamonthon ##### 
summary(data_monitor[(data_monitor$site== "salaya" ) ,]) #mean FID 3.222  10/04
summary(data_monitor[(data_monitor$site== "phutthamonthon" ) ,]) #mean FID 5.327 10/04

#### n° of obs per site #######

table(data_monitor$site) # almost balanced 

ggplot(data_monitor, aes(x=site)) + 
  geom_bar()

###### preliminary kruskal test and plot######
kruskal.test(data_monitor$FIDi~as.factor(data_monitor$site), data=data_monitor)

boxplot(data_monitor$FIDi~data_monitor$site, xlab = "Sites", ylab = "FID (m)")

#########
#### FID VS SIZE #####
#########

##### mean and median FID for salaya and phutthamonthon ##### 
summary(data_monitor[(data_monitor$size== "S" ) ,]) #mean FID 4.727
summary(data_monitor[(data_monitor$size== "M" ) ,]) #mean FID 4.104
summary(data_monitor[(data_monitor$size== "L" ) ,]) #mean FID 4.143

#### n° of obs per size #######

table(data_monitor$size) # not balanced 

ggplot(data_monitor, aes(x=size)) + 
  geom_bar()

###### preliminary kruskal test and plot######
kruskal.test(data_monitor$FIDi~as.factor(data_monitor$size), data=data_monitor) # nope

boxplot(data_monitor$FIDi~data_monitor$size, xlab = "Size", ylab = "Square root of FID (m)") # nope 


###### 2 WAY ANOVA SIZE and SITE #####

####a) Model ####
#mod_size_site_inter<-aov(FIDi~size*site, data=data_monitor)

mod_size_site <- lm(FIDi ~ size + site, data = data_monitor)

AIC(mod_size_site_inter, mod_size_site) #best AIC without interaction 

####b) Conditions of validity ###### 

res.mod_size_site = residuals(mod_size_site)

# independance 
dwtest(mod_size_site) # not OK 

# normalité 
qqnorm(res.mod_size_site)
qqline(res.mod_size_site)
shapiro.test(res.mod_size_site) # not ok probably need to take outliers off 


# homoscedaticité 

bartlett.test(mod_size_site$residuals,data_monitor$site) # not OK

bartlett.test(mod_size_site$residuals,data_monitor$size) # OK 

plot(mod_size_site)

boxplot(FIDi~size+site, data=data_monitor, col=c("green","red", "blue","green","red","blue"), 
        ylab="FID")

#DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharmaanova_inter = simulateResiduals(mod_size_site) 

testSpatialAutocorrelation(res_dharmaanova_inter, x =  data_monitor$long, y = data_monitor$lat) # not good blblblb

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_anova_day <- recalculateResiduals(res_dharmaanova_inter, group = data_monitor$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_anova_day, time = unique(data_monitor$date)) #not good either oupsies 

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_anova_hour <- recalculateResiduals(res_dharmaanova_inter, group = data_monitor$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_anova_hour, time = unique(data_monitor$hour)) #nopity nope 

####c) Results ###### 

Anova(mod_size_site, type=2) # Table d'ANOVA avec une décomposition de type 2
#avec libray car la table d'anova donne pas les mean square juste les fvalue qui coresspondent.
#blblb type deux conseille quand modele desequilibré meme si quand c'est pas desequilibre fort on peut faire type 1 mais c'est pas tres rigoureux. 
#chepa il parle de la methode des models emboités je crois c'est quanf ondecompose les SSR 
#On peut faire des tests post-hoc de comparaisons multiples

mod_size_site_aov <- aov(FIDi ~ size + site, data = data_monitor) #using aov function cause otherwise Tukey doesn't work 
TukeyHSD(mod_size_site_aov) # no effect of size IG 


#########
#### FID VS D_FR_SHELTER #####
#########

######## PRELIMINARY CORRELATION ANALYSES #############

plot(FIDi ~ d_fr_refug, data=data_monitor, col="blue", xlab = "Distance from chosen shelter (m)", ylab = "FID (m)") 

cor(data_monitor$FIDi, data_monitor$d_fr_refug)  

cor.test(data_monitor$FIDi, data_monitor$d_fr_refug) # even with the weird distribution it seems correlated ?

abline(a= -0.17, b= -0.025) 

####################################################################
###### CLASSIC LINEAR MODEL D_F_S + SITE + season #####
#################################################################

####a) Model ####

model_dfs_site_season <- lm(FIDi ~ d_fr_refug + site + season, data=data_monitor)


####b) Conditions of validity ###### 
model_dfs_site_season.res <- residuals(model_dfs_site_season); model_dfs_site_season.res

# independance of residuals 
dwtest(model_dfs_site_season) # nope ????? 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_dfs_site_season) # strong outliers that may need removable 

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_monitor, aes(model_dfs_site_season.res), bins = 12)
p # OK 

shapiro.test(model_dfs_site_season.res) # not OK

# homoskedasticity
bptest(model_dfs_site_season) #not OK 

# DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharmareg_inter = simulateResiduals(model_dfs_site_season)

testSpatialAutocorrelation(res_dharmareg_inter, x =  data_monitor$long, y = data_monitor$lat) # okay 

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_reg_day <- recalculateResiduals(res_dharmareg_inter, group = data_monitor$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_reg_day, time = unique(data_monitor$date)) # okay

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_reg_hour <- recalculateResiduals(res_dharmareg_inter, group = data_monitor$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_reg_hour, time = unique(data_monitor$hour)) # okay 

#### c) Results ####
summary(model_dfs_site_season) # distance from shelter seems explanatory but not condition is met so f me IG 
