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
library(car)
library (DHARMa)
library(MuMIn)
library (nlme)
library(ade4)
library(psych)


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

###### EXCLUDE OBS WITHOUT FID  ###### 


data_monitor = data_monitor[!is.na(data_monitor$FIDi),]
View(data_monitor)


###### EXCLUDE OBS WITHOUT DISTURBANCE  ###### 
data_monitor = data_monitor[!is.na(data_monitor$people_1min),]
View(data_monitor)

####### SQUARE ROOT TRANSFORMATION ######

data_monitor$FIDi_sqrt = sqrt(data_monitor$FIDi)
View (data_monitor)

data_monitor$people_1min_sqrt = sqrt(data_monitor$people_1min)
View (data_monitor)

data_monitor$people_1min_vehic2_sqrt = sqrt(data_monitor$people_1min_vehic2)
View (data_monitor)

####### enlever 2 outliers : 134 (autoroute) et 226 (marché) #######

data_monitor = data_monitor[-c(21,59),]
View(data_monitor)

##############################################
########## I/ VEHICULE = 1 #######################
########################################


######## PRELIMINARY CORRELATION ANALYSES #############

plot(FIDi_sqrt ~ people_1min_sqrt, data=data_monitor, col="blue") 


cor(data_monitor$FIDi_sqrt, data_monitor$people_1min_sqrt)  

cor.test(data_monitor$FIDi_sqrt, data_monitor$people_1min_sqrt)

hist(data_monitor$FIDi_sqrt)

hist(data_monitor$people_1min_sqrt) #pas normal 



#########################################################
######### 1) CLASSIC LINEAR MODEL WITH BOTH SITES ###########
######################################################

####a) Model ####
model <- lm(FIDi_sqrt ~ people_1min_sqrt, data=data_monitor)

####b) Conditions of validity ###### 
model.res <- residuals(model); model.res

# independance of residuals 
dwtest(model) # INDEPENDANCE !!!!!!  

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model) #clustered on the right a bit 

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_monitor, aes(model.res), bins = 12)
p # meh

shapiro.test(model.res) # OKKK 

# homoskedasticity
bptest(model) # pk 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model = simulateResiduals(model)
testSpatialAutocorrelation(res_dharma_model, x =  data_monitor$long, y = data_monitor$lat)
#  spatial autocorrelation of course 

# temporal 

data_monitor$day=-1

data_monitor$day[1]=1
data_monitor$day[2:6]=2
data_monitor$day[7:12]=3
data_monitor$day[13]=4
data_monitor$day[14:16]=5
data_monitor$day[17]=6
data_monitor$day[18:20]=7
data_monitor$day[21:23]=8
data_monitor$day[24:31]=9
data_monitor$day[32:36]=10
data_monitor$day[37:43]=11
data_monitor$day[44:48]=12
data_monitor$day[49:55]=13
data_monitor$day[56:57]=14


# aggregating residuals by time
res_dharma_model = recalculateResiduals(res_dharma_model, group = data_monitor$day)
testTemporalAutocorrelation(res_dharma_model, time = unique(data_monitor$day))
# no autocorralation on a field day scale 


####c) Results ####
summary(model) # almost significant (P = 0.001) but adjusted R-squared = 0.16 + unreliable because doesn't account for site 



####################################################################
###### 2) CLASSIC LINEAR MODEL WITH INTERACTION DISTURBANCE * SITE #####
#################################################################

####a) Model ####
model_inter <- lm(FIDi_sqrt ~ people_1min_sqrt * site, data=data_monitor)

####b) Conditions of validity ###### 
model_inter.res <- residuals(model_inter); model_inter.res

# independance of residuals 
dwtest(model_inter) # INDEPENDANCE !!!!!!  

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_inter) #2 cluters as usual

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_monitor, aes(model_inter.res), bins = 12)
p # OK 

shapiro.test(model_inter.res) # OK

# homoskedasticity
bptest(model_inter) #ok 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_inter = simulateResiduals(model_inter)
testSpatialAutocorrelation(res_dharma_model_inter, x =  data_monitor$long, y = data_monitor$lat)
#  no spatial autocorrelation of course 

# temporal 

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_monitor$day=-1

data_monitor$day[1]=1
data_monitor$day[2:6]=2
data_monitor$day[7:12]=3
data_monitor$day[13]=4
data_monitor$day[14:16]=5
data_monitor$day[17]=6
data_monitor$day[18:20]=7
data_monitor$day[21:23]=8
data_monitor$day[24:31]=9
data_monitor$day[32:36]=10
data_monitor$day[37:43]=11
data_monitor$day[44:48]=12
data_monitor$day[49:55]=13
data_monitor$day[56:57]=14


# aggregating residuals by time
res_dharma_model_inter = recalculateResiduals(res_dharma_model_inter, group = data_monitor$day)
testTemporalAutocorrelation(res_dharma_model_inter, time = unique(data_monitor$day))
# no autocorralation on a field day scale 

#### c) Results ####
summary(model_inter)# only site is explanatory not distance from shelter 

AICc(model,model_inter) #model_inter better than model tout court 


##################################
########    3) ANCOVA ##########
###############################

####a) Condition of validity  ####

# verify linearity between response variable and explanatory variable 
ggscatter(
  data_monitor, x = "people_1min_sqrt", y = "FIDi_sqrt",
  color = "site", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(after_stat(eq.label), ..rr.label.., sep = "~~~~"), color = site)
  ) #globally negative correlation with no interaction 

#Homogeneity of regression slopes (no effect of the explenatory variable on the grouping variable (site))

data_monitor %>% anova_test(FIDi_sqrt ~ site*people_1min_sqrt) # performs type II anova between FIDi, distance from shelter, site, and the interaction disturb + site 
# only site significant

# Normality of residuals  

# Calculer le modèle, la covariable passe en premier
model_anc <- lm(FIDi_sqrt ~ people_1min_sqrt + site, data = data_monitor)

model.metrics <- augment(model_anc) %>%
  select(-.hat, -.sigma, -.fitted)

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_monitor, aes(model.metrics$.resid), bins = 12)
p # OK

shapiro_test(model.metrics$.resid) #ok 

#Homogeneity of variances 

model.metrics %>% levene_test(.resid ~ site) 

# outliers #not sure about this one maybe find something more clear because I don't know where the 3 comes from 

model.metrics %>% 
  filter(abs(.std.resid) > 3) %>% 
  as.data.frame()

#### b) Model and Results ####

res.aov <- data_monitor %>% anova_test(FIDi_sqrt ~ people_1min_sqrt + site)

get_anova_table(res.aov) # only site significant 


#########################################################
####### 4) CLASSIC LINEAR MODEL WITH ONLY SALAYA ######
#########################################################

# take only obs in salaya
data_salaya =  data_monitor[(data_monitor$site=="salaya" ) ,]
View(data_salaya)


# square root transformation


data_salaya$FIDi_sqrt = sqrt(data_salaya$FIDi)

data_salaya$people_1min_sqrt = sqrt(data_salaya$people_1min)

View(data_salaya)


####a) Model ####
model_salaya <- lm(FIDi_sqrt ~ people_1min_sqrt, data=data_salaya)


####b) Conditions of validity ###### 
model_salaya.res <- residuals(model_salaya)

# independance of residuals 
dwtest(model_salaya) # NO INDEPENDANCE !!!!!! => test DHARMa stuff  

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_salaya)

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_salaya = simulateResiduals(model_salaya)
testSpatialAutocorrelation(res_dharma_salaya, x =  data_salaya$long, y = data_salaya$lat)
#  spatial autocorrelation !!!!! #makes sense regarding there are areas that are much more passed by 

# temporal 


### on the scale of field day 

# put a number for each day because the function don't like strings 

data_salaya$day=-1

data_salaya$day[1]=1
data_salaya$day[2]=2
data_salaya$day[3]=3
data_salaya$day[4:6]=4
data_salaya$day[7:14]=5
data_salaya$day[15:21]=6
data_salaya$day[22:26]=7
data_salaya$day[27:28]=8


# aggregating residuals by time
res_dharma_salaya = recalculateResiduals(res_dharma_salaya, group = data_salaya$day)
testTemporalAutocorrelation(res_dharma_salaya, time = unique(data_salaya$day))
# no autocorralation on a field day scale 


# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya, aes(model_salaya.res), bins = 12)
p # meh

shapiro.test(model_salaya.res) # OK

# homoskedasticity
bptest(model_salaya) # OK 

####c) Results ####
summary(model_salaya) # p = 0.18

dev.off()
plot(FIDi_sqrt ~ people_1min_sqrt, data=data_salaya, col="blue") 
abline(a= model_salaya$coefficients[1], b= model_salaya$coefficients[2]) #seems correlated BUT pulled by the 6 big data points 



##################################################################
####### 4.5) CLASSIC LINEAR MODEL WITH ONLY PHUTTHAMONTHON ######
##################################################################

# take only obs in phutthamonthon 
data_phuttha =  data_monitor[(data_monitor$site=="phutthamonthon" ) ,]
View(data_phuttha)


# square root transformation


data_phuttha$FIDi_sqrt = sqrt(data_phuttha$FIDi)

data_phuttha$people_1min_sqrt = sqrt(data_phuttha$people_1min)

View(data_phuttha)


####a) Model ####
model_phuttha <- lm(FIDi_sqrt ~ people_1min_sqrt, data=data_phuttha)


####b) Conditions of validity ###### 
model_phuttha.res <- residuals(model_phuttha)

# independance of residuals 
dwtest(model_phuttha) # INDEPENDANCE !!!!!! => OK 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_phuttha) # okayish

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha, aes(model_phuttha.res), bins = 12)
p # meh

shapiro.test(model_phuttha.res) # OK

# homoskedasticity
bptest(model_phuttha) # homoscedaticity  

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_phuttha = simulateResiduals(model_phuttha)
testSpatialAutocorrelation(res_dharma_phuttha, x =  data_phuttha$long, y = data_phuttha$lat)
# no spatial autocorrelation 

# temporal 


# put a number for each day because the function don't like strings 

data_phuttha$day=-1

data_phuttha$day[1:5]=1
data_phuttha$day[6:11]=2
data_phuttha$day[12:14]=3
data_phuttha$day[15:17]=4
data_phuttha$day[18:22]=5
data_phuttha$day[23:27]=6
data_phuttha$day[28:29]=7



# aggregating residuals by time
res_dharma_phuttha = recalculateResiduals(res_dharma_phuttha, group = data_phuttha$day)
testTemporalAutocorrelation(res_dharma_phuttha, time = unique(data_phuttha$day))
# no autocorralation on a field day scale 

####c) Results ####
summary(model_phuttha) # non significant 
cor.test(data_phuttha$FIDi_sqrt, data_phuttha$people_1min_sqrt) 

dev.off()
plot(FIDi_sqrt ~ people_1min_sqrt, data=data_phuttha, col="blue") 
abline(a= model_phuttha$coefficients[1], b= model_phuttha$coefficients[2]) 


#################################################
####### 5) GLMM TAKING DATE INTO ACCOUNT  ######
################################################

####a) MODEL #####
glmm_date = lmer(FIDi_sqrt ~ people_1min_sqrt + (1|date), data=data_monitor, REML = F)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date)
Fit = fitted(glmm_date)
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

plot (x = data_monitor$people_1min_sqrt,
      y = data_monitor$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "people_1min_sqrt",
      cex = 1.2 , pch = 16) #meh

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_monitor, aes(Res), bins = 12)
p  # meh

shapiro.test(Res) #OK 


# outliers 

plot(data_monitor$people_1min_sqrt) #meh 
plot(data_monitor$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date = simulateResiduals(glmm_date)
testSpatialAutocorrelation(res_dharma_date, x =  data_monitor$long, y = data_monitor$lat)
#  spatial autocorrelation logic 

# temporal 

# put a number for each day because the function don't like strings 


data_monitor$day=-1

data_monitor$day[1]=1
data_monitor$day[2:6]=2
data_monitor$day[7:12]=3
data_monitor$day[13]=4
data_monitor$day[14:16]=5
data_monitor$day[17]=6
data_monitor$day[18:20]=7
data_monitor$day[21:23]=8
data_monitor$day[24:31]=9
data_monitor$day[32:36]=10
data_monitor$day[37:43]=11
data_monitor$day[44:48]=12
data_monitor$day[49:55]=13
data_monitor$day[56:57]=14


# aggregating residuals by time
res_dharma_date = recalculateResiduals(res_dharma_date, group = data_monitor$day)
testTemporalAutocorrelation(res_dharma_date, time = unique(data_monitor$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_date) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
#ICC = 0.28
r.squaredGLMM(glmm_date) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.1 et R2c = 0.35 so nope for correlation lol 
# NONETHELESS !!!! SITE NOT TAKEN INTO ACCOUNT  !!!! 


#############################################
####### 6) GLMM DATE WITH ONLY SALAYA  ######
#############################################



# take only obs in salaya
data_salaya =  data_monitor[(data_monitor$site=="salaya" ) ,]
View(data_salaya)


# square root transformation


data_salaya$FIDi_sqrt = sqrt(data_salaya$FIDi)

data_salaya$people_1min_sqrt = sqrt(data_salaya$people_1min)

View(data_salaya)

####a) MODEL #####
glmm_date_s = lmer(FIDi_sqrt ~ people_1min_sqrt + (1|date), data=data_salaya, REML = F)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_s)
Fit = fitted(glmm_date_s)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_salaya$date<- as.Date(data_salaya$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_salaya)


data_salaya$date = as.factor(data_salaya$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_salaya)
abline(h=0,lty=2)

plot (x = data_monitor$people_1min_sqrt,
      y = data_monitor$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "people_1min_sqrt",
      cex = 1.2 , pch = 16) #meh

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya, aes(Res), bins = 12)
p  #meh 

shapiro.test(Res) #OK 


# outliers 

plot(data_salaya$people_1min_sqrt) #ok this one is weird 
plot(data_salaya$FIDi_sqrt) #OK 


# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_s = simulateResiduals(glmm_date_s)
testSpatialAutocorrelation(res_dharma_date_s, x =  data_salaya$long, y = data_salaya$lat)
# spatial autocorrelation 

# temporal 


# put a number for each day because the function don't like strings 

data_salaya$day=-1

data_salaya$day[1]=1
data_salaya$day[2]=2
data_salaya$day[3]=3
data_salaya$day[4:6]=4
data_salaya$day[7:14]=5
data_salaya$day[15:21]=6
data_salaya$day[22:26]=7
data_salaya$day[27:28]=8

# aggregating residuals by time
res_dharma_date_s = recalculateResiduals(res_dharma_date_s, group = data_salaya$day)
testTemporalAutocorrelation(res_dharma_date_s, time = unique(data_salaya$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_date_s) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.33 

r.squaredGLMM(glmm_date_s) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.05 et R2c = 0.36 so nope for correlation lol 


########################################################
####### 6.5) GLMM DATE WITH ONLY PHUTTHAMONTHON  ######
#######################################################


# take only obs in phutthamonthon 
data_phuttha =  data_monitor[(data_monitor$site=="phutthamonthon" ) ,]
View(data_phuttha)


# square root transformation


data_phuttha$FIDi_sqrt = sqrt(data_phuttha$FIDi)

data_phuttha$people_1min_sqrt = sqrt(data_phuttha$people_1min)

View(data_phuttha)

####a) MODEL #####
glmm_date_p = lmer(FIDi_sqrt ~ people_1min_sqrt + (1|date), data=data_phuttha, REML = F)
# singular is not good !!!!

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_p)
Fit = fitted(glmm_date_p)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # meh 

# Residuals against covariates

#change the date format to %Y%M%D
data_phuttha$date<- as.Date(data_phuttha$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_monitor)


data_phuttha$date = as.factor(data_phuttha$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_phuttha)
abline(h=0,lty=2)

plot (x = data_phuttha$people_1min_sqrt,
      y = data_phuttha$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "people_1min_sqrt",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha, aes(Res), bins = 12)
p  #meh 

shapiro.test(Res) #OK 


# outliers 

plot(data_phuttha$people_1min_sqrt) #OK 2 strong values but we'll make do 
plot(data_phuttha$FIDi_sqrt) #OK  same

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_p = simulateResiduals(glmm_date_p)
testSpatialAutocorrelation(res_dharma_date_p, x =  data_phuttha$long, y = data_phuttha$lat)
# no spatial autocorrelation 

# temporal 

# put a number for each day because the function don't like strings 

data_phuttha$day=-1

data_phuttha$day[1:5]=1
data_phuttha$day[6:11]=2
data_phuttha$day[12:14]=3
data_phuttha$day[15:17]=4
data_phuttha$day[18:22]=5
data_phuttha$day[23:27]=6
data_phuttha$day[28:29]=7


# aggregating residuals by time
res_dharma_date_p = recalculateResiduals(res_dharma_date_p, group = data_phuttha$day)
testTemporalAutocorrelation(res_dharma_date_p, time = unique(data_phuttha$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_date_p) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0 cause singularity 

r.squaredGLMM(glmm_date_p) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.02 et R2c = 0.02 so nope for correlation lol 


##################################
#### 8) GLMM WITH SITES AS FIXED FACTOR AND DATE AS RANDOM #######
###############################


####a) MODEL #####

#glmm_site_date_inter = lmer(FIDi_sqrt ~ people_1min_sqrt * site + (1|date), data=data_monitor, REML = F)

glmm_site_date = lmer(FIDi_sqrt ~ people_1min_sqrt + site + (1|date), data=data_monitor, REML = F)

AICc(glmm_site_date,glmm_site_date_inter) 

# AIC is smaller without accounting for the interaction so we only take the model without from now on

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_site_date)
Fit = fitted(glmm_site_date)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # 2 clusters 

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

plot (x = data_monitor$people_1min_sqrt,
      y = data_monitor$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "people_1min_sqrt",
      cex = 1.2 , pch = 16) #meh

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

shapiro.test(Res) #OK


# outliers 

plot(data_monitor$people_1min_sqrt) #there is some yeye 
plot(data_monitor$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_site_date = simulateResiduals(glmm_site_date)
testSpatialAutocorrelation(res_dharma_site_date, x =  data_monitor$long, y = data_monitor$lat)
# no spatial autocorrelation  

# temporal 

# put a number for each day because the function don't like strings 

data_monitor$day=-1

data_monitor$day[1]=1
data_monitor$day[2:6]=2
data_monitor$day[7:12]=3
data_monitor$day[13]=4
data_monitor$day[14:16]=5
data_monitor$day[17]=6
data_monitor$day[18:20]=7
data_monitor$day[21:23]=8
data_monitor$day[24:31]=9
data_monitor$day[32:36]=10
data_monitor$day[37:43]=11
data_monitor$day[44:48]=12
data_monitor$day[49:55]=13
data_monitor$day[56:57]=14


# aggregating residuals by time
res_dharma_site_date = recalculateResiduals(res_dharma_site_date, group = data_monitor$day)
testTemporalAutocorrelation(res_dharma_site_date, time = unique(data_monitor$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_site_date) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.04
r.squaredGLMM(glmm_site_date) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.32 et R2c = 0.36 

################################################
###########MODEL COMPARISON#####################
################################################

AICc(model, model_anc, model_inter, glmm_date, glmm_site_date)
min(AICc(model, model_anc, model_inter, glmm_date, glmm_site_date)[,2])

# model_anc is the best 

summary(model_anc)# only site is significant 
#validity ok 


# check salaya and phutthamonthon separatly 


#Salaya 
AICc(model_salaya, glmm_date_s)

summary(model_salaya) #not significant + spatial autocorrelation 


model_gls_exp <- gls( FIDi_sqrt ~ people_1min_sqrt, 
                      data = data_salaya, 
                      correlation = corExp(form = ~ long + lat, nugget = TRUE))
summary(model_gls_exp)

model_gls_gaus <- gls( FIDi_sqrt ~ people_1min_sqrt, 
                       data = data_salaya, 
                       correlation = corGaus(form = ~ long + lat, nugget = TRUE))
summary(model_gls_gaus)

model_gls_lin <- gls( FIDi_sqrt ~ people_1min_sqrt, 
                      data = data_salaya, 
                      correlation = corLin(form = ~ long + lat, nugget = TRUE))
summary(model_gls_lin)

model_gls_ratio <- gls( FIDi_sqrt ~ people_1min_sqrt, 
                        data = data_salaya, 
                        correlation = corRatio(form = ~ long + lat, nugget = TRUE))
summary(model_gls_ratio)

model_gls_spher <- gls( FIDi_sqrt ~ people_1min_sqrt, 
                        data = data_salaya, 
                        correlation = corSpher(form = ~ long + lat, nugget = TRUE))
summary(model_gls_spher)

# still non significant 

#Phutthamothon 
AICc(model_phuttha, glmm_date_p)

summary(model_phuttha) # p=0.45
# validity OK




##############################################
########## I/ VEHICULE = 2 #######################
########################################


######## PRELIMINARY CORRELATION ANALYSES #############

plot(FIDi_sqrt ~ people_1min_vehic2_sqrt, data=data_monitor, col="blue") 

cor(data_monitor$FIDi_sqrt, data_monitor$people_1min_vehic2_sqrt)  

cor.test(data_monitor$FIDi_sqrt, data_monitor$people_1min_vehic2_sqrt) #p= 0.001 

hist(data_monitor$FIDi_sqrt)

hist(data_monitor$people_1min_vehic2_sqrt) #pas normal 


#########################################################
######### 1) CLASSIC LINEAR MODEL WITH BOTH SITES ###########
######################################################

####a) Model ####
model <- lm(FIDi_sqrt ~ people_1min_vehic2_sqrt, data=data_monitor)

####b) Conditions of validity ###### 
model.res <- residuals(model); model.res

# independance of residuals 
dwtest(model) # INDEPENDANCE !!!!!!  

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model) #clustered on the right  

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_monitor, aes(model.res), bins = 12)
p # OK

shapiro.test(model.res) # OKKK 

# homoskedasticity
bptest(model) # Ok 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model = simulateResiduals(model)
testSpatialAutocorrelation(res_dharma_model, x =  data_monitor$long, y = data_monitor$lat)
#  spatial autocorrelation of course 

# temporal 

data_monitor$day=-1

data_monitor$day[1]=1
data_monitor$day[2:6]=2
data_monitor$day[7:12]=3
data_monitor$day[13]=4
data_monitor$day[14:16]=5
data_monitor$day[17]=6
data_monitor$day[18:20]=7
data_monitor$day[21:23]=8
data_monitor$day[24:31]=9
data_monitor$day[32:36]=10
data_monitor$day[37:43]=11
data_monitor$day[44:48]=12
data_monitor$day[49:55]=13
data_monitor$day[56:57]=14


# aggregating residuals by time
res_dharma_model = recalculateResiduals(res_dharma_model, group = data_monitor$day)
testTemporalAutocorrelation(res_dharma_model, time = unique(data_monitor$day))
# no autocorralation on a field day scale 


####c) Results ####
summary(model) # p= 0.002 but autocorrelated 



####################################################################
###### 2) CLASSIC LINEAR MODEL WITH INTERACTION DISTURBANCE * SITE #####
#################################################################

####a) Model ####
model_inter <- lm(FIDi_sqrt ~ people_1min_vehic2_sqrt * site, data=data_monitor)

####b) Conditions of validity ###### 
model_inter.res <- residuals(model_inter); model_inter.res

# independance of residuals 
dwtest(model_inter) # INDEPENDANCE !!!!!!  

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_inter) #2 cluters as usual

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_monitor, aes(model_inter.res), bins = 12)
p # OK 

shapiro.test(model_inter.res) # OK

# homoskedasticity
bptest(model_inter) #ok 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_inter = simulateResiduals(model_inter)
testSpatialAutocorrelation(res_dharma_model_inter, x =  data_monitor$long, y = data_monitor$lat)
#  no spatial autocorrelation of course 

# temporal 

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_monitor$day=-1

data_monitor$day[1]=1
data_monitor$day[2:6]=2
data_monitor$day[7:12]=3
data_monitor$day[13]=4
data_monitor$day[14:16]=5
data_monitor$day[17]=6
data_monitor$day[18:20]=7
data_monitor$day[21:23]=8
data_monitor$day[24:31]=9
data_monitor$day[32:36]=10
data_monitor$day[37:43]=11
data_monitor$day[44:48]=12
data_monitor$day[49:55]=13
data_monitor$day[56:57]=14


# aggregating residuals by time
res_dharma_model_inter = recalculateResiduals(res_dharma_model_inter, group = data_monitor$day)
testTemporalAutocorrelation(res_dharma_model_inter, time = unique(data_monitor$day))
# no autocorralation on a field day scale 

#### c) Results ####
summary(model_inter)# only site is explanatory 

AICc(model,model_inter) #model_inter better than model tout court 


##################################
########    3) ANCOVA ##########
###############################

####a) Condition of validity  ####

# verify linearity between response variable and explanatory variable 
ggscatter(
  data_monitor, x = "people_1min_vehic2_sqrt", y = "FIDi_sqrt",
  color = "site", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(after_stat(eq.label), ..rr.label.., sep = "~~~~"), color = site)
  ) #globally negative correlation with no interaction 

#Homogeneity of regression slopes (no effect of the explenatory variable on the grouping variable (site))

data_monitor %>% anova_test(FIDi_sqrt ~ site*people_1min_vehic2_sqrt) # performs type II anova between FIDi, distance from shelter, site, and the interaction disturb + site 
# only site significant

# Normality of residuals  

# Calculer le modèle, la covariable passe en premier
model_anc <- lm(FIDi_sqrt ~ people_1min_vehic2_sqrt + site, data = data_monitor)

model.metrics <- augment(model_anc) %>%
  select(-.hat, -.sigma, -.fitted)

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_monitor, aes(model.metrics$.resid), bins = 12)
p # OK

shapiro_test(model.metrics$.resid) #ok 

#Homogeneity of variances 

model.metrics %>% levene_test(.resid ~ site) # ok 

# outliers #not sure about this one maybe find something more clear because I don't know where the 3 comes from 

model.metrics %>% 
  filter(abs(.std.resid) > 3) %>% 
  as.data.frame()

#### b) Model and Results ####

res.aov <- data_monitor %>% anova_test(FIDi_sqrt ~ people_1min_vehic2_sqrt + site)

get_anova_table(res.aov) # only site significant 



#########################################################
####### 4) CLASSIC LINEAR MODEL WITH ONLY SALAYA ######
#########################################################

# take only obs in salaya
data_salaya =  data_monitor[(data_monitor$site=="salaya" ) ,]
View(data_salaya)


# square root transformation


data_salaya$FIDi_sqrt = sqrt(data_salaya$FIDi)

data_salaya$people_1min_vehic2_sqrt = sqrt(data_salaya$people_1min_vehic2)

View(data_salaya)


####a) Model ####
model_salaya <- lm(FIDi_sqrt ~ people_1min_vehic2_sqrt, data=data_salaya)


####b) Conditions of validity ###### 
model_salaya.res <- residuals(model_salaya)

# independance of residuals 
dwtest(model_salaya) # NO INDEPENDANCE !!!!!! => test DHARMa stuff  

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_salaya)#meh 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_salaya = simulateResiduals(model_salaya)
testSpatialAutocorrelation(res_dharma_salaya, x =  data_salaya$long, y = data_salaya$lat)
#  spatial autocorrelation !!!!! #makes sense regarding there are areas that are much more passed by 

# temporal 


### on the scale of field day 

# put a number for each day because the function don't like strings 

data_salaya$day=-1

data_salaya$day[1]=1
data_salaya$day[2]=2
data_salaya$day[3]=3
data_salaya$day[4:6]=4
data_salaya$day[7:14]=5
data_salaya$day[15:21]=6
data_salaya$day[22:26]=7
data_salaya$day[27:28]=8


# aggregating residuals by time
res_dharma_salaya = recalculateResiduals(res_dharma_salaya, group = data_salaya$day)
testTemporalAutocorrelation(res_dharma_salaya, time = unique(data_salaya$day))
# no autocorralation on a field day scale 


# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya, aes(model_salaya.res), bins = 12)
p # meh

shapiro.test(model_salaya.res) # OK

# homoskedasticity
bptest(model_salaya) # OK 

####c) Results ####
summary(model_salaya) # p = 0.2

dev.off()
plot(FIDi_sqrt ~ people_1min_vehic2_sqrt, data=data_salaya, col="blue") 
abline(a= model_salaya$coefficients[1], b= model_salaya$coefficients[2]) #seems correlated BUT pulled by the 6 big data points 


##################################################################
####### 4.5) CLASSIC LINEAR MODEL WITH ONLY PHUTTHAMONTHON ######
##################################################################

# take only obs in phutthamonthon 
data_phuttha =  data_monitor[(data_monitor$site=="phutthamonthon" ) ,]
View(data_phuttha)


# square root transformation


data_phuttha$FIDi_sqrt = sqrt(data_phuttha$FIDi)

data_phuttha$people_1min_vehic2_sqrt = sqrt(data_phuttha$people_1min_vehic2)

View(data_phuttha)


####a) Model ####
model_phuttha <- lm(FIDi_sqrt ~ people_1min_vehic2_sqrt, data=data_phuttha)


####b) Conditions of validity ###### 
model_phuttha.res <- residuals(model_phuttha)

# independance of residuals 
dwtest(model_phuttha) # INDEPENDANCE !!!!!! => OK 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_phuttha) # okayish

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha, aes(model_phuttha.res), bins = 12)
p # meh

shapiro.test(model_phuttha.res) # OK

# homoskedasticity
bptest(model_phuttha) # homoscedaticity  

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_phuttha = simulateResiduals(model_phuttha)
testSpatialAutocorrelation(res_dharma_phuttha, x =  data_phuttha$long, y = data_phuttha$lat)
# no spatial autocorrelation 

# temporal 


# put a number for each day because the function don't like strings 

data_phuttha$day=-1

data_phuttha$day[1:5]=1
data_phuttha$day[6:11]=2
data_phuttha$day[12:14]=3
data_phuttha$day[15:17]=4
data_phuttha$day[18:22]=5
data_phuttha$day[23:27]=6
data_phuttha$day[28:29]=7



# aggregating residuals by time
res_dharma_phuttha = recalculateResiduals(res_dharma_phuttha, group = data_phuttha$day)
testTemporalAutocorrelation(res_dharma_phuttha, time = unique(data_phuttha$day))
# no autocorralation on a field day scale 

####c) Results ####
summary(model_phuttha) # non significant 

dev.off()
plot(FIDi_sqrt ~ people_1min_vehic2_sqrt, data=data_phuttha, col="blue") 
abline(a= model_phuttha$coefficients[1], b= model_phuttha$coefficients[2]) 



#################################################
####### 5) GLMM TAKING DATE INTO ACCOUNT  ######
################################################

####a) MODEL #####
glmm_date = lmer(FIDi_sqrt ~ people_1min_vehic2_sqrt + (1|date), data=data_monitor, REML = F)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date)
Fit = fitted(glmm_date)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_monitor$date<- as.Date(data_monitor$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
View(data_monitor)


data_monitor$date = as.factor(data_monitor$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_monitor)
abline(h=0,lty=2)

plot (x = data_monitor$people_1min_vehic2_sqrt,
      y = data_monitor$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "people_1min_vehic2_sqrt",
      cex = 1.2 , pch = 16) #meh

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_monitor, aes(Res), bins = 12)
p  # meh

shapiro.test(Res) #OK 


# outliers 

plot(data_monitor$people_1min_vehic2_sqrt) #meh 
plot(data_monitor$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date = simulateResiduals(glmm_date)
testSpatialAutocorrelation(res_dharma_date, x =  data_monitor$long, y = data_monitor$lat)
#  spatial autocorrelation logic 

# temporal 

# put a number for each day because the function don't like strings 


data_monitor$day=-1

data_monitor$day[1]=1
data_monitor$day[2:6]=2
data_monitor$day[7:12]=3
data_monitor$day[13]=4
data_monitor$day[14:16]=5
data_monitor$day[17]=6
data_monitor$day[18:20]=7
data_monitor$day[21:23]=8
data_monitor$day[24:31]=9
data_monitor$day[32:36]=10
data_monitor$day[37:43]=11
data_monitor$day[44:48]=12
data_monitor$day[49:55]=13
data_monitor$day[56:57]=14


# aggregating residuals by time
res_dharma_date = recalculateResiduals(res_dharma_date, group = data_monitor$day)
testTemporalAutocorrelation(res_dharma_date, time = unique(data_monitor$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_date) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
#ICC = 0.28
r.squaredGLMM(glmm_date) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.1 et R2c = 0.35 so nope for correlation lol 
# NONETHELESS !!!! SITE NOT TAKEN INTO ACCOUNT  !!!! 


#############################################
####### 6) GLMM DATE WITH ONLY SALAYA  ######
#############################################



# take only obs in salaya
data_salaya =  data_monitor[(data_monitor$site=="salaya" ) ,]
View(data_salaya)


# square root transformation


data_salaya$FIDi_sqrt = sqrt(data_salaya$FIDi)

data_salaya$people_1min_vehic2_sqrt = sqrt(data_salaya$people_1min_vehic2)

View(data_salaya)

####a) MODEL #####
glmm_date_s = lmer(FIDi_sqrt ~ people_1min_vehic2_sqrt + (1|date), data=data_salaya, REML = F)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_s)
Fit = fitted(glmm_date_s)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_salaya$date<- as.Date(data_salaya$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_salaya)


data_salaya$date = as.factor(data_salaya$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_salaya)
abline(h=0,lty=2)

plot (x = data_monitor$people_1min_vehic2_sqrt,
      y = data_monitor$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "people_1min_vehic2_sqrt",
      cex = 1.2 , pch = 16) #meh

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya, aes(Res), bins = 12)
p  #meh 

shapiro.test(Res) #OK 


# outliers 

plot(data_salaya$people_1min_vehic2_sqrt) #ok this one is weird 
plot(data_salaya$FIDi_sqrt) #OK 


# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_s = simulateResiduals(glmm_date_s)
testSpatialAutocorrelation(res_dharma_date_s, x =  data_salaya$long, y = data_salaya$lat)
# spatial autocorrelation 

# temporal 


# put a number for each day because the function don't like strings 

data_salaya$day=-1

data_salaya$day[1]=1
data_salaya$day[2]=2
data_salaya$day[3]=3
data_salaya$day[4:6]=4
data_salaya$day[7:14]=5
data_salaya$day[15:21]=6
data_salaya$day[22:26]=7
data_salaya$day[27:28]=8

# aggregating residuals by time
res_dharma_date_s = recalculateResiduals(res_dharma_date_s, group = data_salaya$day)
testTemporalAutocorrelation(res_dharma_date_s, time = unique(data_salaya$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_date_s) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.33 

r.squaredGLMM(glmm_date_s) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.05 et R2c = 0.36 so nope for correlation lol 



########################################################
####### 6.5) GLMM DATE WITH ONLY PHUTTHAMONTHON  ######
#######################################################


# take only obs in phutthamonthon 
data_phuttha =  data_monitor[(data_monitor$site=="phutthamonthon" ) ,]
View(data_phuttha)


# square root transformation


data_phuttha$FIDi_sqrt = sqrt(data_phuttha$FIDi)

data_phuttha$people_1min_vehic2_sqrt = sqrt(data_phuttha$people_1min_vehic2)

View(data_phuttha)

####a) MODEL #####
glmm_date_p = lmer(FIDi_sqrt ~ people_1min_vehic2_sqrt + (1|date), data=data_phuttha, REML = F)
# singular is not good !!!!

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_p)
Fit = fitted(glmm_date_p)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # meh 

# Residuals against covariates

#change the date format to %Y%M%D
data_phuttha$date<- as.Date(data_phuttha$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_phuttha)


data_phuttha$date = as.factor(data_phuttha$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_phuttha)
abline(h=0,lty=2)

plot (x = data_phuttha$people_1min_vehic2_sqrt,
      y = data_phuttha$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "people_1min_vehic2_sqrt",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha, aes(Res), bins = 12)
p  #meh 

shapiro.test(Res) #OK 


# outliers 

plot(data_phuttha$people_1min_vehic2_sqrt) #few strong values 
plot(data_phuttha$FIDi_sqrt) #  same

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_p = simulateResiduals(glmm_date_p)
testSpatialAutocorrelation(res_dharma_date_p, x =  data_phuttha$long, y = data_phuttha$lat)
# no spatial autocorrelation 

# temporal 

# put a number for each day because the function don't like strings 

data_phuttha$day=-1

data_phuttha$day[1:5]=1
data_phuttha$day[6:11]=2
data_phuttha$day[12:14]=3
data_phuttha$day[15:17]=4
data_phuttha$day[18:22]=5
data_phuttha$day[23:27]=6
data_phuttha$day[28:29]=7


# aggregating residuals by time
res_dharma_date_p = recalculateResiduals(res_dharma_date_p, group = data_phuttha$day)
testTemporalAutocorrelation(res_dharma_date_p, time = unique(data_phuttha$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_date_p) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0 cause singularity 

r.squaredGLMM(glmm_date_p) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.01 et R2c = 0.01 so nope for correlation lol 



##################################
#### 8) GLMM WITH SITES AS FIXED FACTOR AND DATE AS RANDOM #######
###############################


####a) MODEL #####

#glmm_site_date_inter = lmer(FIDi_sqrt ~ people_1min_vehic2_sqrt * site + (1|date), data=data_monitor, REML = F)

glmm_site_date = lmer(FIDi_sqrt ~ people_1min_vehic2_sqrt + site + (1|date), data=data_monitor, REML = F)

AICc(glmm_site_date,glmm_site_date_inter) 

# AIC is smaller without accounting for the interaction so we only take the model without from now on

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_site_date)
Fit = fitted(glmm_site_date)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # 2 clusters 

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

plot (x = data_monitor$people_1min_vehic2_sqrt,
      y = data_monitor$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "people_1min_vehic2_sqrt",
      cex = 1.2 , pch = 16) #meh

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

shapiro.test(Res) #OK


# outliers 

plot(data_monitor$people_1min_vehic2_sqrt) #there is some yeye 
plot(data_monitor$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_site_date = simulateResiduals(glmm_site_date)
testSpatialAutocorrelation(res_dharma_site_date, x =  data_monitor$long, y = data_monitor$lat)
# no spatial autocorrelation  

# temporal 

# put a number for each day because the function don't like strings 

data_monitor$day=-1

data_monitor$day[1]=1
data_monitor$day[2:6]=2
data_monitor$day[7:12]=3
data_monitor$day[13]=4
data_monitor$day[14:16]=5
data_monitor$day[17]=6
data_monitor$day[18:20]=7
data_monitor$day[21:23]=8
data_monitor$day[24:31]=9
data_monitor$day[32:36]=10
data_monitor$day[37:43]=11
data_monitor$day[44:48]=12
data_monitor$day[49:55]=13
data_monitor$day[56:57]=14


# aggregating residuals by time
res_dharma_site_date = recalculateResiduals(res_dharma_site_date, group = data_monitor$day)
testTemporalAutocorrelation(res_dharma_site_date, time = unique(data_monitor$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_site_date) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.06
r.squaredGLMM(glmm_site_date) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.31 et R2c = 0.36 

################################################
###########MODEL COMPARISON#####################
################################################

AICc(model, model_anc, model_inter, glmm_date, glmm_site_date)
min(AICc(model, model_anc, model_inter, glmm_date, glmm_site_date)[,2])

# model_anc is the best 

summary(model_anc)# only site is significant 
#validity ok 


# check salaya and phutthamonthon separatly 


#Salaya 
AICc(model_salaya, glmm_date_s)

summary(model_salaya) #not significant + spatial autocorrelation 


model_gls_exp <- gls( FIDi_sqrt ~ people_1min_vehic2_sqrt, 
                      data = data_salaya, 
                      correlation = corExp(form = ~ long + lat, nugget = TRUE))
summary(model_gls_exp)

model_gls_gaus <- gls( FIDi_sqrt ~ people_1min_vehic2_sqrt, 
                       data = data_salaya, 
                       correlation = corGaus(form = ~ long + lat, nugget = TRUE))
summary(model_gls_gaus)

model_gls_lin <- gls( FIDi_sqrt ~ people_1min_vehic2_sqrt, 
                      data = data_salaya, 
                      correlation = corLin(form = ~ long + lat, nugget = TRUE))
summary(model_gls_lin)

model_gls_ratio <- gls( FIDi_sqrt ~ people_1min_vehic2_sqrt, 
                        data = data_salaya, 
                        correlation = corRatio(form = ~ long + lat, nugget = TRUE))
summary(model_gls_ratio)

model_gls_spher <- gls( FIDi_sqrt ~ people_1min_vehic2_sqrt, 
                        data = data_salaya, 
                        correlation = corSpher(form = ~ long + lat, nugget = TRUE))
summary(model_gls_spher)

# still non significant 

#Phutthamothon 
AICc(model_phuttha, glmm_date_p)

summary(model_phuttha) # p=0.45
# validity OK

# REMARK : no noticible effect when making cars and moto weigh 2 in the data => square root transformation flattens everything anyway. 

# SUMMARY : no linear effect of human disturbance on FID => I don't wanna believe it 
# 1) maybe not enough data 
# 2) not right model (people are poisson variable and not normal) => more likely 

########## ADD BINARY FOR PEOPLE #################

people01 = data_monitor$people_1min >0 
data_monitor$people01 = people01

boxplot(FIDi_sqrt ~ people01, data =data_monitor) 

########## A/ kruskal test keeping the zeros #############

boxplot(people_1min ~ site, data= data_monitor) 
kruskal.test(data_monitor$people_1min, data_monitor$site) # clear effect of site on people_1min 

boxplot(FIDi_sqrt ~ people01, data= data_monitor) 
kruskal.test(data_monitor$FIDi_sqrt, data_monitor$people01) # no effect of binary presence of people on FID

