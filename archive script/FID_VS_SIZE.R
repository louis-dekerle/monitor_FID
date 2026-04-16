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
library(car)

setwd("C:/Users/LENOVO/Desktop/stage mahidol/stat")

/!\/!\/!\/!\/!\/!\
/!\ DO NOT FORGET TO UPDATE DATASET TO NEWEST VERSION /!\
/!\/!\/!\/!\/!\/!\

##### LOAD RAW DATASET #####
data_monitor <- read.csv("C:/Users/LENOVO/Desktop/stage mahidol/stat/fid_monitor_rainyss_process.csv", header= T, sep = ";")
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
summary(data_monitor[(data_monitor$size== "S" ) ,]) #mean FID 5.909
summary(data_monitor[(data_monitor$size== "M" ) ,]) #mean FID 5.146
summary(data_monitor[(data_monitor$size== "L" ) ,]) #mean FID 5.552


#### n° of obs per size #######

table(data_monitor$size)

ggplot(data_monitor, aes(x=size)) + 
  geom_bar()

###### preliminary kruskal test and plot######
kruskal.test(data_monitor$FIDi_sqrt~as.factor(data_monitor$size), data=data_monitor) # nope

boxplot(data_monitor$FIDi_sqrt~data_monitor$size, xlab = "Size", ylab = "Square root of FID (m)") # nope nope nope 

#########################################
#######  A/ UNBALANCED DISTRIBUTION ###########
################################################


#############################
###### 1) BASIC ANOVA ANALYSES #####
###################################

####a) MODEL #####

model_size <- lm(FIDi_sqrt ~ size, data=data_monitor)

plot(model_size)

res.model_size = residuals(model_size)

####b) CONDITION OF APPLICATION ####

# independance 
dwtest(model_size) # no independance maybe because site we'll see with DHARMa

# normalité 
qqnorm(res.model_size)
qqline(res.model_size)
shapiro.test(res.model_size) # OK

# homoskedasticy 

bartlett.test(model_size$residuals,data_monitor$size) # OK 

#or

bptest(model_size) # OK 

# outliers 

model.metrics <- augment(model_size) %>%
  select(-.hat, -.sigma, -.fitted) # Supprimer les détails
head(model.metrics, 3)
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>% #on cherche les residus studentizé (valeur standard non influencé par l'unité/échelle de mesure) supérieur à 3 car outliers 
  as.data.frame()

#DHARMA test for autocorrelation ! 
  
# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 


res_dharmaanova = simulateResiduals(model_size)

testSpatialAutocorrelation(res_dharmaanova, x =  data_monitor$long, y = data_monitor$lat)

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

data_monitor$date<- as.Date(data_monitor$date, format="%d-%m-%Y") # depending on the data set it may be "-" or "/"in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_anova_day <- recalculateResiduals(res_dharmaanova, group = data_monitor$date)

# Test temporal autocorrelation 


testTemporalAutocorrelation(res_grouped_anova_day, time = unique(data_monitor$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_anova_hour <- recalculateResiduals(res_dharmaanova, group = data_monitor$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_anova_hour, time = unique(data_monitor$hour))

####c) RESULT #####

anova(model_size) # nope p = 0.54

summary(aov(FIDi_sqrt ~ size, data=data_monitor))

TukeyHSD(aov(FIDi_sqrt ~ size, data=data_monitor), conf.level=.95)

####################################################################
###### 2) 2 WAY ANOVA WITH INTERACTION SIZE * SITE #####
#################################################################

####a) Model ####
#mod_size_site_inter<-aov(FIDi_sqrt~size*site, data=data_monitor)

mod_size_site<-aov(FIDi_sqrt~size+site, data=data_monitor)

AIC(mod_size_site_inter, mod_size_site)

####b) Conditions of validity ###### 

res.mod_size_site = residuals(mod_size_site)

# independance 
dwtest(mod_size_site) # OK 

# normalité 
qqnorm(res.mod_size_site)
qqline(res.mod_size_site)
shapiro.test(res.mod_size_site) #presque OK 


# homoscedaticité 

bartlett.test(mod_size_site$residuals,data_monitor$site) # OK

bartlett.test(mod_size_site$residuals,data_monitor$size) # OK 

plot(mod_size_site)

boxplot(FIDi_sqrt~size+site, data=data_monitor, col=c("green","green", "red","red","blue","blue"), 
        ylab="FID_sqrt")

#DHARMA test for autocorrelation ! # apparently DHARMa might not be reliable for this model soooooooooo \_(^_^')_/

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharmaanova_inter = simulateResiduals(mod_size_site)

testSpatialAutocorrelation(res_dharmaanova_inter, x =  data_monitor$long, y = data_monitor$lat)

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

data_monitor$date<- as.Date(data_monitor$date, format="%d/%m/%Y") # depending on the data set it may be "-" or "/"in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_anova_day <- recalculateResiduals(res_dharmaanova_inter, group = data_monitor$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_anova_day, time = unique(data_monitor$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_anova_hour <- recalculateResiduals(res_dharmaanova_inter, group = data_monitor$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_anova_hour, time = unique(data_monitor$hour))

####c) Results ###### 

Anova(mod_size_site, type=2) # Table d'ANOVA avec une décomposition de type 2
#avec libray car la table d'anova donne pas les mean square juste les fvalue qui coresspondent.
#blblb type deux conseille quand modele desequilibré meme si quand c'est pas desequilibre fort on peut faire type 1 mais c'est pas tres rigoureux. 
#chepa il parle de la methode des models emboités je crois c'est quanf ondecompose les SSR 
#On peut faire des tests post-hoc de comparaisons multiples
TukeyHSD(mod_size_site) # no effect of size IG 

#################
#########3)  MIXED MODEL WITH DATE#############
#################################


####a) MODEL #####
glmm = lmer(FIDi_sqrt ~ size + (1|date), data=data_monitor)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm)
Fit = fitted(glmm)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # small pattern probably because of the sites 

# Residuals against covariates

#change the date format to %Y%M%D
data_monitor$date<- as.Date(data_monitor$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_monitor)

data_monitor$date = as.factor(data_monitor$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_monitor)
abline(h=0,lty=2)

boxplot(Res ~ size, xlab = "size", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_monitor)
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

plot(data_monitor$FIDi_sqrt) #OK 

#DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharma_glmm = simulateResiduals(glmm)

testSpatialAutocorrelation(res_dharma_glmm, x =  data_monitor$long, y = data_monitor$lat)

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

data_monitor$date<- as.Date(data_monitor$date, format="%d/%m/%Y") # depending on the data set it may be "-" or "/"in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_glmm_day <- recalculateResiduals(res_dharma_glmm, group = data_monitor$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_glmm_day, time = unique(data_monitor$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_glmm_hour <- recalculateResiduals(res_dharma_glmm, group = data_monitor$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_glmm_hour, time = unique(data_monitor$hour))

#####c) RESULTS #####

summary(glmm) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC : 0.36

r.squaredGLMM(glmm) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
# R2m = 0.00093 R2c = 0.358

#################
######### 4)  MIXED MODEL INTERACTION SITE*SIZE AND DATE AS RANDOM FACTOR #############
#################################


####a) MODEL #####
#glmm_size_site_inter = lmer(FIDi_sqrt ~ size*site + (1|date), data=data_monitor)

glmm_size_site = lmer(FIDi_sqrt ~ size+site + (1|date), data=data_monitor)

#AIC(glmm_size_site, glmm_size_site_inter)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_size_site)
Fit = fitted(glmm_size_site)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_monitor$date<- as.Date(data_monitor$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_monitor)

data_monitor$date = as.factor(data_monitor$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_monitor)
abline(h=0,lty=2)

boxplot(Res ~ site, xlab = "site", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_monitor)
abline(h=0,lty=2)

boxplot(Res ~ size, xlab = "site", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_monitor)
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
p  

shapiro.test(Res) # not ok but not shocking

#DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharma_glmm_site = simulateResiduals(glmm_size_site)

testSpatialAutocorrelation(res_dharma_glmm_site, x =  data_monitor$long, y = data_monitor$lat)

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

data_monitor$date<- as.Date(data_monitor$date, format="%d/%m/%Y") # depending on the data set it may be "-" or "/"in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_glmm_site_day <- recalculateResiduals(res_dharma_glmm_site, group = data_monitor$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_glmm_site_day, time = unique(data_monitor$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_glmm_site_hour <- recalculateResiduals(res_dharma_glmm_site, group = data_monitor$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_glmm_site_hour, time = unique(data_monitor$hour))

#####c) RESULTS #####

summary(glmm_size_site) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.19
r.squaredGLMM(glmm_size_site) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.201 et R2c = 0.356


################################################
###########MODEL COMPARISON#####################
################################################

AIC(model_size, mod_size_site, glmm, glmm_size_site)
min(AIC(model_size, mod_size_site, glmm, glmm_size_site)[,2])

#best model seems to be the 2 way anova 



#############################
###### 5) BASIC ANOVA ANALYSES FOR SALAYA #####
###################################

# take only obs in salaya
data_salaya =  data_monitor[(data_monitor$site=="salaya" ) ,]
View(data_salaya)


# square root transformation


data_salaya$FIDi_sqrt = sqrt(data_salaya$FIDi)

View(data_salaya)


#### n° of obs per size #######

table(data_salaya$size)

ggplot(data_salaya, aes(x=size)) + 
  geom_bar()

###### preliminary kruskal test and plot######
kruskal.test(data_salaya$FIDi_sqrt~as.factor(data_salaya$size), data=data_salaya) # nope

boxplot(data_salaya$FIDi_sqrt~data_salaya$size, xlab = "Size", ylab = "Square root of FID (m)") # nope nope nope 



####a) MODEL #####

model_size_s <- lm(FIDi_sqrt ~ size, data=data_salaya)

plot(model_size_s)

res.model_size_s = residuals(model_size_s)

####b) CONDITION OF APPLICATION ####

# independance 
dwtest(model_size_s) # no independance maybe because site we'll see with DHARMa

# normalité 
qqnorm(res.model_size_s)
qqline(res.model_size_s)
shapiro.test(res.model_size_s) # not OK

# homoskedasticy 

bartlett.test(model_size_s$residuals,data_salaya$size) # not OK 

#or

bptest(model_size_s) # OK 

# outliers 

model.metrics <- augment(model_size_s) %>%
  select(-.hat, -.sigma, -.fitted) # Supprimer les détails
head(model.metrics, 3)
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>% #on cherche les residus studentizé (valeur standard non influencé par l'unité/échelle de mesure) supérieur à 3 car outliers 
  as.data.frame()

#DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharma_anova_s = simulateResiduals(model_size_s)

testSpatialAutocorrelation(res_dharma_anova_s, x =  data_salaya$long, y = data_salaya$lat)

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

data_salaya$date<- as.Date(data_salaya$date, format="%d/%m/%Y") # depending on the data set it may be "-" or "/"in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_anova_s_day <- recalculateResiduals(res_dharma_anova_s, group = data_salaya$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_anova_s_day, time = unique(data_salaya$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_salaya$hour <- as.POSIXct(paste(data_salaya$date, data_salaya$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_anova_s_hour <- recalculateResiduals(res_dharma_anova_s, group = data_salaya$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_anova_s_hour, time = unique(data_salaya$hour))

####c) RESULT #####

anova(model_size_s) # nope p = 0.6


#############################
###### 5.5) BASIC ANOVA ANALYSES FOR PHUTTHAMONTHON #####
###################################

# take only obs in phuttha
data_phuttha =  data_monitor[(data_monitor$site=="phutthamonthon" ) ,]
View(data_phuttha)


# square root transformation


data_phuttha$FIDi_sqrt = sqrt(data_phuttha$FIDi)

View(data_phuttha)


#### n° of obs per size #######

table(data_phuttha$size)

ggplot(data_phuttha, aes(x=size)) + 
  geom_bar()

###### preliminary kruskal test and plot######
kruskal.test(data_phuttha$FIDi_sqrt~as.factor(data_phuttha$size), data=data_phuttha) # nope

boxplot(data_phuttha$FIDi_sqrt~data_phuttha$size, xlab = "Size", ylab = "Square root of FID (m)") # nope nope nope 



####a) MODEL #####

model_size_p <- lm(FIDi_sqrt ~ size, data=data_phuttha)

plot(model_size_p)

res.model_size_p = residuals(model_size_p)

####b) CONDITION OF APPLICATION ####

# independance 
dwtest(model_size_p) # no independance maybe because site we'll see with DHARMa

# normalité 
qqnorm(res.model_size_p)
qqline(res.model_size_p)
shapiro.test(res.model_size_p) # OK

# homoskedasticy 

bartlett.test(model_size_p$residuals,data_phuttha$size) # not OK 

#or

bptest(model_size_p) # OK 

# outliers 

model.metrics <- augment(model_size_p) %>%
  select(-.hat, -.sigma, -.fitted) # Supprimer les détails
head(model.metrics, 3)
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>% #on cherche les residus studentizé (valeur standard non influencé par l'unité/échelle de mesure) supérieur à 3 car outliers 
  as.data.frame()

#DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharma_anova_p = simulateResiduals(model_size_p)

testSpatialAutocorrelation(res_dharma_anova_p, x =  data_phuttha$long, y = data_phuttha$lat)

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

data_phuttha$date<- as.Date(data_phuttha$date, format="%d/%m/%Y") # depending on the data set it may be "-" or "/"in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_anova_p_day <- recalculateResiduals(res_dharma_anova_p, group = data_phuttha$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_anova_p_day, time = unique(data_phuttha$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_phuttha$hour <- as.POSIXct(paste(data_phuttha$date, data_phuttha$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_anova_p_hour <- recalculateResiduals(res_dharma_anova_p, group = data_phuttha$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_anova_p_hour, time = unique(data_phuttha$hour))

####c) RESULT #####

anova(model_size_p) # nope p = 0.73










#########################################
#######  B/ BALANCED DISTRIBUTION ###########
################################################

##### sort dataframes by size ####

M = data_monitor[data_monitor$size=="M",]
L = data_monitor[data_monitor$size=="L",]
S = data_monitor[data_monitor$size=="S",]

###### create random samples #### 

S_M = M[sample(1:49,23),]
S_L = L[sample(1:26,23),]

S_M_L = merge(S_M,S_L, all = T)
S_S_M_L = merge(S,S_M_L, all = T)

#### n° of obs per site #######

table(S_S_M_L$size)


#############################
###### 1) BASIC ANOVA ANALYSES #####
###################################



####a) MODEL #####


model_sizeb <- lm(FIDi_sqrt ~ size, data=S_S_M_L)

plot(model_sizeb)

res.model_sizeb = residuals(model_sizeb)

####b) CONDITION OF APPLICATION ####

# independance 
dwtest(model_sizeb) # OK ; OK ; not OK ; OK ; OK ; OK ; not OK ; OK ; not OK ; OK

# normalité 
qqnorm(res.model_sizeb)
qqline(res.model_sizeb)
shapiro.test(res.model_sizeb) # OK ; OK ; not OK ; OK ; OK ; OK ; OK ; OK ; OK ; OK

# homoskedasticy 

bartlett.test(model_sizeb$residuals,S_S_M_L$size) # OK ; OK ; OK ; OK ; OK ; OK ; OK ; OK ; OK ; OK

#or

bptest(model_sizeb) # not OK ; OK ; OK ; OK ; OK ; OK ; OK ; OK ; OK ; OK

# outliers 

model.metrics <- augment(model_sizeb) %>%
  select(-.hat, -.sigma, -.fitted) # Supprimer les détails
head(model.metrics, 3)
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>% #on cherche les residus studentizé (valeur standard non influencé par l'unité/échelle de mesure) supérieur à 3 car outliers 
  as.data.frame()

####c) RESULT #####

anova(model_sizeb) # nope p = 0.37 ; 0.72 ; 0.62 ; 0.27 ; 0.77 ; 0.54 ; 0.51 ; 0.45 ; 0.47 ; 0.47


####################################################################
###### 2) 2 WAY ANOVA WITH INTERACTION SIZE * SITE #####
#################################################################

####a) Model ####
#mod_size_site_interb<-aov(FIDi_sqrt~size*site, data=S_S_M_L)

mod_size_siteb<-aov(FIDi_sqrt~size+site, data=S_S_M_L)

#AIC(mod_size_site_interb, mod_size_siteb)

####b) Conditions of validity ###### 

res.mod_size_siteb = residuals(mod_size_siteb)

# independance 
dwtest(mod_size_siteb) # OK ; OK ; OK ; OK ; OK ; OK ; OK ; OK ; OK ; OK

# normalité 
qqnorm(res.mod_size_siteb)
qqline(res.mod_size_siteb)
shapiro.test(res.mod_size_siteb) # OK  ; OK ; OK ; not OK ; OK ; OK ; not OK ; not OK ; OK ; not ok


# homoscedaticité 

bartlett.test(mod_size_siteb$residuals,S_S_M_L$site) # OK  ; OK ; OK ; OK ; OK ; OK ; OK ; OK ; OK ; OK

bartlett.test(mod_size_siteb$residuals,S_S_M_L$size)  # OK  ; OK ; OK ; OK ; OK ; OK ; OK ; OK ; OK ; OK

plot(mod_size_siteb)

boxplot(FIDi_sqrt~size+site, data=S_S_M_L, col=c("green","green", "red","red","blue","blue"), 
        ylab="FID_sqrt")

####c) Results ###### 

Anova(mod_size_siteb, type=2) # Table d'ANOVA avec une décomposition de type 2
#avec libray car la table d'anova donne pas les mean square juste les fvalue qui coresspondent.
#blblb type deux conseille quand modele desequilibré meme si quand c'est pas desequilibre fort on peut faire type 1 mais c'est pas tres rigoureux. 
#chepa il parle de la methode des models emboités je crois c'est quanf ondecompose les SSR 
#On peut faire des tests post-hoc de comparaisons multiples
# p = 0.84 ; 0.99 ; 0.78 ; 0.92 ; 0.86 ; 0.55 ; 0.74 ; 0.96 ; 0.89 ; 0.97

TukeyHSD(mod_size_siteb) 
#p M-L : 0.95 ; 0.99 ; 0.9 ; 1 ; 0.83 ; 0.73 ; 0.42 ; 0.82 ; 0.88 ; 1
#p S-L : 0.60 ; 0.8 ; 0.4 ; 0.6 ; 0.37 ; 0.32 ; 0.49 ; 0.91 ; 0.58 ; 0.6
#p S-M : 0.41 ; 0.75 ; 0.21 ; 0.7 ; 0.72 ; 0.07 ; 0.99 ; 0.59 ; 0.31 ; 0.6

#################
#########3)  MIXED MODEL WITH DATE#############
#################################


####a) MODEL #####
glmmb = lmer(FIDi_sqrt ~ size + (1|date), data=S_S_M_L)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmmb)
Fit = fitted(glmmb)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # small pattern probably because of the sites 

# Residuals against covariates

#change the date format to %Y%M%D
S_S_M_L$date<- as.Date(S_S_M_L$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_monitor)

S_S_M_L$date = as.factor(S_S_M_L$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = S_S_M_L)
abline(h=0,lty=2)

boxplot(Res ~ size, xlab = "size", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = S_S_M_L)
abline(h=0,lty=2)

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = S_S_M_L, aes(Res), bins = 12)
p #c'est effectivement le delire on a une belle normalité à gauche et trois outliers de merde à droite lol 

shapiro.test(Res)# OK ; OK; OK; OK; OK; OK; OK; OK; OK

# outliers 

plot(S_S_M_L$FIDi_sqrt) #OK 

#####c) RESULTS #####

summary(glmmb) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC : 0.49

r.squaredGLMM(glmmb) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
# R2m = 0.01 ; 5e-7 ; 0.01 ; 0.002 ; 0.04 ; 0.002 ; 0.008 ; 0.0004 ; 0.02 ; 0.04
# R2c = 0.50 ; 0.41 ; 0.34 ; 0.3 ; 0.37 ; 0.42 ; 0.35 ; 0.30 ; 0.41 ; 0.37


#################
######### 4)  MIXED MODEL INTERACTION SITE*SIZE AND DATE AS RANDOM FACTOR #############
#################################


####a) MODEL #####
#glmm_size_site_inter = lmer(FIDi_sqrt ~ size*site + (1|date), data=data_monitor)

glmm_size_siteb = lmer(FIDi_sqrt ~ size+site + (1|date), data=S_S_M_L)

#AIC(glmm_size_site, glmm_size_site_inter)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_size_siteb)
Fit = fitted(glmm_size_siteb)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
S_S_M_L$date<- as.Date(S_S_M_L$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_monitor)

S_S_M_L$date = as.factor(S_S_M_L$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = S_S_M_L)
abline(h=0,lty=2)

boxplot(Res ~ site, xlab = "site", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = S_S_M_L)
abline(h=0,lty=2)

boxplot(Res ~ size, xlab = "site", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = S_S_M_L)
abline(h=0,lty=2)

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = S_S_M_L, aes(Res), bins = 12)
p  

shapiro.test(Res) # ok ; OK ; OK ; OK ; OK

#####c) RESULTS #####

summary(glmm_size_siteb) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.31
r.squaredGLMM(glmm_size_siteb) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.22 ; 0.21 ; 0.22 ; 0.12 ; 0.22
#R2c = 0.34 ; 0.41 ; 0.27 ; 0.2 ; 0.37

