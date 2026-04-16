dev.off()  

##### LIBRARY ##### 
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
library (DHARMa)
library(graphics)
library(MuMIn)

setwd("C:/Users/LENOVO/Desktop/stage mahidol/stat")

/!\/!\/!\/!\/!\/!\
/!\ DO NOT FORGET TO UPDATE DATASET TO NEWEST VERSION /!\
/!\/!\/!\/!\/!\/!\

##### LOAD RAW DATASET #####
data_monitor <- read.csv("C:/Users/LENOVO/Desktop/stage mahidol/stat/data_obs8.csv", header= T, sep = ";")
View(data_monitor)



###### EXCLUDE OBS OTHER THAN V_SALVA ######
data_monitor = data_monitor[(data_monitor$sp=="V_salva" ) ,]
View(data_monitor)

###### EXCLUDE OBS WITHOUT FID  ###### 


data_monitor = data_monitor[!is.na(data_monitor$FIDi),]
View(data_monitor)

####### SQUARE ROOT TRANSFORMATION ######
data_monitor$FIDi_sqrt = sqrt(data_monitor$FIDi)
View (data_monitor)

##### mean and median FID for salaya and phutthamonthon ##### 
summary(data_monitor[(data_monitor$site== "salaya" ) ,]) #mean FID 4,436 24/02
summary(data_monitor[(data_monitor$site== "phutthamonthon" ) ,]) #mean FID 8,387 24/02

#### n° of obs per site #######

table(data_monitor$site)

ggplot(data_monitor, aes(x=site)) + 
  geom_bar()


###### preliminary kruskal test and plot######
kruskal.test(data_monitor$FIDi_sqrt~as.factor(data_monitor$site), data=data_monitor)

boxplot(data_monitor$FIDi_sqrt~data_monitor$site, xlab = "Sites", ylab = "Square root of FID (m)")

###################################
########BASIC ANOVA ANALYSES #####
###################################

####1) MODEL #####

model_site <- lm(FIDi_sqrt ~ site, data=data_monitor)

plot(model_site)

res.model_site = residuals(model_site)

####2) CONDITION OF APPLICATION ####

# independance 
dwtest(model_site)

# normalité 
qqnorm(res.model_site)
qqline(res.model_site)
shapiro.test(res.model_site) 

# homoskedasticy 

# homoscedaticité 

bartlett.test(model_site$residuals,data_monitor$site) 

#or

bptest(model_site)

# outliers 

model.metrics <- augment(model_site) %>%
  select(-.hat, -.sigma, -.fitted) # Supprimer les détails
head(model.metrics, 3)
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>% #on cherche les residus studentizé (valeur standard non influencé par l'unité/échelle de mesure) supérieur à 3 car outliers 
  as.data.frame()


####3) RESULT #####

anova(model_site)

#################
######### MIXED MODEL WITH DATE#############
#################################


####1) MODEL #####
glmm = lmer(FIDi_sqrt ~ site + (1|date), data=data_monitor)

####2) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm)
Fit = fitted(glmm)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_monitor$date<- as.Date(data_monitor$date, format="%d/%m/%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_monitor)


data_monitor$date = as.factor(data_monitor$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_monitor)
abline(h=0,lty=2)

boxplot(Res ~ site, xlab = "site", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_monitor)
abline(h=0,lty=2)

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_monitor, aes(Res), bins = 12)
p #c'est effectivement le delire on a une belle normalité à gauche et trois outliers de merde à droite lol 

shapiro.test(Res)


# outliers 

model.metrics_glmm <- augment(glmm) %>%
  select(-.hat, -.sigma, -.fitted) # Supprimer les détails
head(model.metrics, 3)
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>% #on cherche les residus studentizé (valeur standard non influencé par l'unité/échelle de mesure) supérieur à 3 car outliers 
  as.data.frame()


#####3) RESULTS #####

summary(glmm) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 

r.squaredGLMM(glmm) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 

#### 4) COMPARISON OF MODELS ####

AIC(model_site, glmm) # classic model fits better ==> date doesn't have a significant effect 

# REMEMBER THAT WE USE DATE AS A PROXY TO PSEUDOREPLICATION BECAUSE WE HAVE SMALL CHANCE TO FIND THE SAME INDIVIDUALS SEVERAL TIME A DAY BUT IT CAN HAPPEN FROM A DAY TO ANOTHER 
# SUMMARY => IF THERE IS PSEUDOREPLICATION ACROSS THE DAYS, IT DOESN't HAVE A MAJOR IMPACT 


#################
######### MIXED MODEL WITH SIZE #############
#################################

####1) MODEL #####
glmm_size = lmer(FIDi_sqrt ~ site + (1|size), data=data_monitor)

####2) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_size)
Fit = fitted(glmm_size)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

data_monitor$size = as.factor(data_monitor$size)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ size, xlab = "size", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_monitor)
abline(h=0,lty=2)

boxplot(Res ~ site, xlab = "site", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_monitor)
abline(h=0,lty=2)

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_monitor, aes(Res), bins = 12)
p #c'est effectivement le delire on a une belle normalité à gauche et trois outliers de merde à droite lol 

shapiro.test(Res) # nope but looks good on the graph


# outliers 

model.metrics_glmm_size <- augment(glmm_size) %>%
  select(-.hat, -.sigma, -.fitted) # Supprimer les détails
head(model.metrics, 3)
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>% #on cherche les residus studentizé (valeur standard non influencé par l'unité/échelle de mesure) supérieur à 3 car outliers 
  as.data.frame()

#####3) RESULTS #####

summary(glmm_size) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 

r.squaredGLMM(glmm_size) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 

#### 4) COMPARISON OF MODELS ####

AICc(model_site, glmm, glmm_size) # classic model fits better ==> size doesn't have a significant effect 


############ SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS #################

res_dharmaglmm = simulateResiduals(glmm)

res_dharmaanova = simulateResiduals(model_site)

testSpatialAutocorrelation(res_dharmaglmm, x =  data_monitor$long, y = data_monitor$lat,)

testSpatialAutocorrelation(res_dharmaanova, x =  data_monitor$long, y = data_monitor$lat)

########### TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS ########## 



### on the scale of field days ###

data_monitor$date<- as.Date(data_monitor$date, format="%d-%m-%Y") # depending on the data set it may be "-" in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_glmm_day <- recalculateResiduals(res_dharmaglmm, group = data_monitor$date)
res_grouped_anova_day <- recalculateResiduals(res_dharmaanova, group = data_monitor$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_glmm_day, time = unique(data_monitor$date))


testTemporalAutocorrelation(res_grouped_anova_day, time = unique(data_monitor$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S")

data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_glmm_hour <- recalculateResiduals(res_dharmaglmm, group = data_monitor$hour)
res_grouped_anova_hour <- recalculateResiduals(res_dharmaanova, group = data_monitor$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_glmm_hour, time = unique(data_monitor$hour))


testTemporalAutocorrelation(res_grouped_anova_hour, time = unique(data_monitor$hour))










