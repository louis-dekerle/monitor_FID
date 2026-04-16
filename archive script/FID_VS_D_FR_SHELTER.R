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

###### EXCLUDE OBS WITHOUT DISTANCE FROM SHELTER  ###### 


data_monitor = data_monitor[!is.na(data_monitor$d_fr_refug),]
View(data_monitor)

####### SQUARE ROOT TRANSFORMATION ######

data_monitor$FIDi_sqrt = sqrt(data_monitor$FIDi)
View (data_monitor)

data_monitor$d_fr_refug_sqrt = sqrt(data_monitor$d_fr_refug)
View (data_monitor)

######## PRELIMINARY CORRELATION ANALYSES #############

plot(FIDi_sqrt ~ d_fr_refug_sqrt, data=data_monitor, col="blue", xlab = "Square root of distance from chosen shelter (m)", ylab = "Square root of FID (m)") 

cor(data_monitor$FIDi_sqrt, data_monitor$d_fr_refug_sqrt)  

cor.test(data_monitor$FIDi_sqrt, data_monitor$d_fr_refug_sqrt)

abline(a= 1.8941, b= 0.1917) #seems correlated BUT pulled by the 6 big data points 


#########################################################
######### 1) CLASSIC LINEAR MODEL WITH BOTH SITES ###########
######################################################

####a) Model ####
model <- lm(FIDi_sqrt ~ d_fr_refug_sqrt, data=data_monitor)

####b) Conditions of validity ###### 
model.res <- residuals(model); model.res

# independance of residuals 
dwtest(model) # NO INDEPENDANCE !!!!!! => effect of sites 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model)

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
bptest(model) 

# DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 


res_dharmareg = simulateResiduals(model)

testSpatialAutocorrelation(res_dharmareg, x =  data_monitor$long, y = data_monitor$lat)

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

data_monitor$date<- as.Date(data_monitor$date, format="%d/%m/%Y") # depending on the data set it may be "-" or "/"in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_reg_day <- recalculateResiduals(res_dharmareg, group = data_monitor$date)

# Test temporal autocorrelation 


testTemporalAutocorrelation(res_grouped_reg_day, time = unique(data_monitor$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_reg_hour <- recalculateResiduals(res_dharmareg, group = data_monitor$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_reg_hour, time = unique(data_monitor$hour))

####c) Results ####
summary(model) # almost significant (P = 0.06) but adjusted R-squared = 0.02 + unreliable because doesn't account for site 



####################################################################
###### 2) CLASSIC LINEAR MODEL WITH INTERACTION D_F_S * SITE #####
#################################################################

####a) Model ####
model_inter <- lm(FIDi_sqrt ~ d_fr_refug_sqrt * site, data=data_monitor)

####b) Conditions of validity ###### 
model_inter.res <- residuals(model_inter); model_inter.res

# independance of residuals 
dwtest(model_inter) # INDEPENDANCE !!!!!! => we took account of the effect of sites 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_inter)

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
bptest(model_inter) 

# DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharmareg_inter = simulateResiduals(model_inter)

testSpatialAutocorrelation(res_dharmareg_inter, x =  data_monitor$long, y = data_monitor$lat)

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

data_monitor$date<- as.Date(data_monitor$date, format="%d/%m/%Y") # depending on the data set it may be "-" or "/"in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_reg_day <- recalculateResiduals(res_dharmareg_inter, group = data_monitor$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_reg_day, time = unique(data_monitor$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_reg_hour <- recalculateResiduals(res_dharmareg_inter, group = data_monitor$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_reg_hour, time = unique(data_monitor$hour))

#### c) Results ####
summary(model_inter)# only site is explanatory not distance from shelter 

AIC(model,model_inter) #model_inter better than model tout court 

##################################
########    3) ANCOVA ##########
###############################

####a) Condition of validity  ####

# verify linearity between response variable and explanatory variable 
ggscatter(
  data_monitor, x = "d_fr_refug_sqrt", y = "FIDi_sqrt",
  color = "site", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(after_stat(eq.label), ..rr.label.., sep = "~~~~"), color = site)
  )

#Homogeneity of regression slopes (no effect of the explenatory variable on the grouping variable (site))

data_monitor %>% anova_test(FIDi_sqrt ~ site*d_fr_refug_sqrt) # performs type II anova between FIDi, distance from shelter, site, and the interaction d_f_s + site 
# => P-value for interaction > 0.05 => OK  
# => p-value distance from refuge < 0.05 => significatif ? 

# Normality of residuals  

# Calculer le modèle, la covariable passe en premier
model_anc <- lm(FIDi_sqrt ~ d_fr_refug_sqrt + site, data = data_monitor)

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

shapiro_test(model.metrics$.resid)

#Homogeneity of variances 

model.metrics %>% levene_test(.resid ~ site) 

# outliers #not sure about this one maybe find something more clear because I don't know where the 3 comes from 

model.metrics %>% 
  filter(abs(.std.resid) > 3) %>% 
  as.data.frame()


# DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharma_anc = simulateResiduals(model_anc)

testSpatialAutocorrelation(res_dharma_anc, x =  data_monitor$long, y = data_monitor$lat)

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

data_monitor$date<- as.Date(data_monitor$date, format="%d/%m/%Y") # depending on the data set it may be "-" or "/"in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_anc_day <- recalculateResiduals(res_dharma_anc, group = data_monitor$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_anc_day, time = unique(data_monitor$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_anc_hour <- recalculateResiduals(res_dharma_anc, group = data_monitor$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_anc_hour, time = unique(data_monitor$hour))

#### b) Model and Results ####

res.aov <- data_monitor %>% anova_test(FIDi_sqrt ~ d_fr_refug_sqrt + site)

get_anova_table(res.aov) #d_fr_shelter seems significant with ANCOVA 

#########################################################
####### 4) CLASSIC LINEAR MODEL WITH ONLY SALAYA ######
#########################################################

# take only obs in salaya
data_salaya =  data_monitor[(data_monitor$site=="salaya" ) ,]
View(data_salaya)


# square root transformation


data_salaya$FIDi_sqrt = sqrt(data_salaya$FIDi)

data_salaya$d_fr_refug_sqrt = sqrt(data_salaya$d_fr_refug)

View(data_salaya)


####a) Model ####
model_salaya <- lm(FIDi_sqrt ~ d_fr_refug_sqrt, data=data_salaya)


####b) Conditions of validity ###### 
model_salaya.res <- residuals(model_salaya)

# independance of residuals 
dwtest(model_salaya) # NO INDEPENDANCE !!!!!! => test DHARMa stuff  

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_salaya)

# DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharma_reg_s = simulateResiduals(model_salaya)

testSpatialAutocorrelation(res_dharma_reg_s, x =  data_salaya$long, y = data_salaya$lat)

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

data_salaya$date<- as.Date(data_salaya$date, format="%d/%m/%Y") # depending on the data set it may be "-" or "/"in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_reg_s_day <- recalculateResiduals(res_dharma_reg_s, group = data_salaya$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_reg_s_day, time = unique(data_salaya$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_salaya$hour <- as.POSIXct(paste(data_salaya$date, data_salaya$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_reg_s_hour <- recalculateResiduals(res_dharma_reg_s, group = data_salaya$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_reg_s_hour, time = unique(data_salaya$hour))


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
summary(model_salaya) # significant (P = 0.02) but adjusted R-squared = 0.08 + possibly autocorrelation

plot(FIDi_sqrt ~ d_fr_refug_sqrt, data=data_salaya, col="blue") 
abline(a= model_salaya$coefficients[1], b= model_salaya$coefficients[2]) #seems correlated BUT pulled by the 6 big data points 

##################################################################
####### 4.5) CLASSIC LINEAR MODEL WITH ONLY PHUTTHAMONTHON ######
##################################################################

# take only obs in phutthamonthon 
data_phuttha =  data_monitor[(data_monitor$site=="phutthamonthon" ) ,]
View(data_phuttha)


# square root transformation


data_phuttha$FIDi_sqrt = sqrt(data_phuttha$FIDi)

data_phuttha$d_fr_refug_sqrt = sqrt(data_phuttha$d_fr_refug)

View(data_phuttha)


####a) Model ####
model_phuttha <- lm(FIDi_sqrt ~ d_fr_refug_sqrt, data=data_phuttha)


####b) Conditions of validity ###### 
model_phuttha.res <- residuals(model_phuttha)

# independance of residuals 
dwtest(model_phuttha) # INDEPENDANCE !!!!!! => OK 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_phuttha) #big pattern in the residuals => variance increase with ffitted value => heteroscedasticity ?
# answer from tao => it's not the best but not worry too much
# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha, aes(model_phuttha.res), bins = 12)
p # OK 

shapiro.test(model_phuttha.res) # OK

# homoskedasticity
bptest(model_phuttha) # homoscedaticity contradicting the pattern in the plots ? 

# DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharma_reg_p = simulateResiduals(model_phuttha)

testSpatialAutocorrelation(res_dharma_reg_p, x =  data_phuttha$long, y = data_phuttha$lat)

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

data_phuttha$date<- as.Date(data_phuttha$date, format="%d/%m/%Y") # depending on the data set it may be "-" or "/"in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_reg_p_day <- recalculateResiduals(res_dharma_reg_p, group = data_phuttha$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_reg_p_day, time = unique(data_phuttha$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_phuttha$hour <- as.POSIXct(paste(data_phuttha$date, data_phuttha$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_reg_p_hour <- recalculateResiduals(res_dharma_reg_p, group = data_phuttha$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_reg_p_hour, time = unique(data_phuttha$hour))

####c) Results ####
summary(model_phuttha) # non significant 

plot(FIDi_sqrt ~ d_fr_refug_sqrt, data=data_phuttha, col="blue") 
abline(a= model_phuttha$coefficients[1], b= model_phuttha$coefficients[2]) 

#################################################
####### 5) GLMM TAKING DATE INTO ACCOUNT  ######
################################################

####a) MODEL #####
glmm_date = lmer(FIDi_sqrt ~ d_fr_refug_sqrt + (1|date), data=data_monitor, REML = F)

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

plot (x = data_monitor$d_fr_refug_sqrt,
      y = data_monitor$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "d_fr_refug_sqrt",
      cex = 1.2 , pch = 16) #OK 

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

plot(data_monitor$d_fr_refug_sqrt) #OK 
plot(data_monitor$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharma_glmm = simulateResiduals(glmm_date)

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

summary(glmm_date) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 

r.squaredGLMM(glmm_date) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.05 et R2c = 0.42 so nope for correlation lol 
# NONETHELESS !!!! SITE NOT TAKEN INTO ACCOUNT  !!!! HENCE THE EFFECT OF THE DATE SO STRONG PROBABLY 

#############################################
####### 6) GLMM DATE WITH ONLY SALAYA  ######
#############################################



# take only obs in salaya
data_salaya =  data_monitor[(data_monitor$site=="salaya" ) ,]
View(data_salaya)


# square root transformation


data_salaya$FIDi_sqrt = sqrt(data_salaya$FIDi)

data_salaya$d_fr_refug_sqrt = sqrt(data_salaya$d_fr_refug)

View(data_salaya)

####a) MODEL #####
glmm_date_s = lmer(FIDi_sqrt ~ d_fr_refug_sqrt + (1|date), data=data_salaya, REML = F)

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
#View(data_monitor)


data_salaya$date = as.factor(data_salaya$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_salaya)
abline(h=0,lty=2)

plot (x = data_salaya$d_fr_refug_sqrt,
      y = data_salaya$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "d_fr_refug_sqrt",
      cex = 1.2 , pch = 16) #OK 

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

plot(data_salaya$d_fr_refug_sqrt) #OK il y une petite valeur forte mais pas choquant
plot(data_salaya$FIDi_sqrt) #OK 


# DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharma_glmm_s = simulateResiduals(glmm_date_s)

testSpatialAutocorrelation(res_dharma_glmm_s, x =  data_salaya$long, y = data_salaya$lat)

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

data_salaya$date<- as.Date(data_salaya$date, format="%d/%m/%Y") # depending on the data set it may be "-" or "/"in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_glmm_s_day <- recalculateResiduals(res_dharma_glmm_s, group = data_salaya$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_glmm_s_day, time = unique(data_salaya$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_salaya$hour <- as.POSIXct(paste(data_salaya$date, data_salaya$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_glmm_s_hour <- recalculateResiduals(res_dharma_glmm_s, group = data_salaya$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_glmm_s_hour, time = unique(data_salaya$hour))

#####c) RESULTS #####

summary(glmm_date_s) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.33 c'est boucou 

r.squaredGLMM(glmm_date_s) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.09 et R2c = 0.397 so nope for correlation lol 
# Now weird why so much importance of date though  

########################################################
####### 6.5) GLMM DATE WITH ONLY PHUTTHAMONTHON  ######
#######################################################


# take only obs in phutthamonthon 
data_phuttha =  data_monitor[(data_monitor$site=="phutthamonthon" ) ,]
View(data_phuttha)


# square root transformation


data_phuttha$FIDi_sqrt = sqrt(data_phuttha$FIDi)

data_phuttha$d_fr_refug_sqrt = sqrt(data_phuttha$d_fr_refug)

View(data_phuttha)

####a) MODEL #####
glmm_date_p = lmer(FIDi_sqrt ~ d_fr_refug_sqrt + (1|date), data=data_phuttha, REML = F)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_p)
Fit = fitted(glmm_date_p)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_phuttha$date<- as.Date(data_phuttha$date, format="%d/%m/%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_monitor)


data_phuttha$date = as.factor(data_phuttha$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_phuttha)
abline(h=0,lty=2)

plot (x = data_phuttha$d_fr_refug_sqrt,
      y = data_phuttha$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "d_fr_refug_sqrt",
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
p  

shapiro.test(Res) #OK 


# outliers 

plot(data_phuttha$d_fr_refug_sqrt) #OK 2 strong values but we'll make do 
plot(data_phuttha$FIDi_sqrt) #OK  same

# DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharma_glmm_p = simulateResiduals(glmm_date_p)

testSpatialAutocorrelation(res_dharma_glmm_p, x =  data_phuttha$long, y = data_phuttha$lat)

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

data_phuttha$date<- as.Date(data_phuttha$date, format="%d/%m/%Y") # depending on the data set it may be "-" or "/"in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_glmm_p_day <- recalculateResiduals(res_dharma_glmm_p, group = data_phuttha$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_glmm_p_day, time = unique(data_phuttha$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_phuttha$hour <- as.POSIXct(paste(data_phuttha$date, data_phuttha$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_glmm_p_hour <- recalculateResiduals(res_dharma_glmm_p, group = data_phuttha$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_glmm_p_hour, time = unique(data_phuttha$hour))

#####c) RESULTS #####

summary(glmm_date_p) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.08 c'est moins

r.squaredGLMM(glmm_date_p) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.02 et R2c = 0.1 so nope for correlation lol 
# less weird I don't know why salaya importance of date like that.  

##################################
#### 7) GLMM WITH SITES AS RANDOM FACTOR #######
###############################

####a) MODEL #####
glmm_site = lmer(FIDi_sqrt ~ d_fr_refug_sqrt + (1|site), data=data_monitor, REML = F)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_site)
Fit = fitted(glmm_site)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # patterny pattern I guess

# Residuals against covariates

data_monitor$site = as.factor(data_monitor$site)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ site, xlab = "site", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_monitor)
abline(h=0,lty=2)

plot (x = data_monitor$d_fr_refug_sqrt,
      y = data_monitor$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "d_fr_refug_sqrt",
      cex = 1.2 , pch = 16) #OK 

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

plot(data_monitor$d_fr_refug_sqrt) #OK 
plot(data_monitor$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharma_glmm_site = simulateResiduals(glmm_site)

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

summary(glmm_site) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.39 assez logic
r.squaredGLMM(glmm_site) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.03 et R2c = 0.41 so nope for correlation lol 
# logic that site is so important 


##################################
#### 8) GLMM WITH SITES AS FIXED FACTOR AND DATE AS RANDOM #######
###############################



####a) MODEL #####

#glmm_site_date_inter = lmer(FIDi_sqrt ~ d_fr_refug_sqrt * site + (1|date), data=data_monitor, REML = F)

glmm_site_date = lmer(FIDi_sqrt ~ d_fr_refug_sqrt + site + (1|date), data=data_monitor, REML = F)

AIC(glmm_site_date,glmm_site_date_inter) 

# AIC is smaller without accounting for the interaction so we only take the model without from now on

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_site_date)
Fit = fitted(glmm_site_date)
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

plot (x = data_monitor$d_fr_refug_sqrt,
      y = data_monitor$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "d_fr_refug_sqrt",
      cex = 1.2 , pch = 16) #OK 

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

shapiro.test(Res) #not OK but not shocking


# outliers 

plot(data_monitor$d_fr_refug_sqrt) #OK 
plot(data_monitor$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# SPATIAL AUTOCORRELATION TEST ON THE RESIDUALS 

res_dharma_glmm_site_date = simulateResiduals(glmm_site_date)

testSpatialAutocorrelation(res_dharma_glmm_site_date, x =  data_monitor$long, y = data_monitor$lat)

# TEMPORAL AUTOCORRELATION TEST ON THE RESIDUALS

### on the scale of field days ###

data_monitor$date<- as.Date(data_monitor$date, format="%d/%m/%Y") # depending on the data set it may be "-" or "/"in sted of ":" data_monitor$date<- as.Date(data_monitor$date, format="%d:%m:%Y")

# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_glmm_site_date_day <- recalculateResiduals(res_dharma_glmm_site_date, group = data_monitor$date)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_glmm_site_date_day, time = unique(data_monitor$date))

### on an hour scale ###

# CHECK IF THE FORMAT PARAMETER IS THE SAME AS IN THE COLUMNS  

data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H") #if doesn't work try data_monitor$hour <- as.POSIXct(paste(data_monitor$date, data_monitor$time), format="%Y-%m-%d %H:%M:%S") and then remove the minutes and the seconds 


# recalculate residuals to account for repetitive dates (aggregate by time)

res_grouped_glmm_site_date_hour <- recalculateResiduals(res_dharma_glmm_site_date, group = data_monitor$hour)

# Test temporal autocorrelation 

testTemporalAutocorrelation(res_grouped_glmm_site_date_hour, time = unique(data_monitor$hour))

#####c) RESULTS #####

summary(glmm_site_date) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.23 
r.squaredGLMM(glmm_site_date) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.224 et R2c = 0.404 
# logic that site is so important 

##################################
#### 9) GLS WITH SITES AS FIXED FACTOR AND DATE AS RANDOM #######
###############################

# waiting for tao's advice on if we can actually had the random factors 

FIT_AR1 <- gls ( FIDi_sqrt ~ d_fr_refug_sqrt + site,
                   correlation = corAR1 ( form = ~ fid ) ,
                   data = data_monitor )
FIT_CAR1 <- gls ( FIDi_sqrt ~ d_fr_refug_sqrt + site,
                 correlation = corCAR1 ( form = ~ fid ) ,
                 data = data_monitor )
FIT_CorCS <- gls ( FIDi_sqrt ~ d_fr_refug_sqrt + site,
                  correlation = corCompSymm ( form = ~ fid ) ,
                  data = data_monitor )

####################################################################
############ 10) GLS for model_inter ###################
############################################"
FIT_AR1_inter <- gls ( FIDi_sqrt ~ d_fr_refug_sqrt * site,
                 correlation = corAR1 ( form = ~ fid ) ,
                 data = data_monitor )
FIT_CAR1_inter <- gls ( FIDi_sqrt ~ d_fr_refug_sqrt * site,
                  correlation = corCAR1 ( form = ~ fid ) ,
                  data = data_monitor )
FIT_CorCS_inter <- gls ( FIDi_sqrt ~ d_fr_refug_sqrt * site,
                   correlation = corCompSymm ( form = ~ fid ) ,
                   data = data_monitor )

################################################
###########MODEL COMPARISON#####################
################################################

AICc(model, model_anc, model_inter, glmm_date, glmm_site_date)
min(AICc(model, model_anc, model_inter, glmm_date, glmm_site_date)[,2])

#best model seems to be the taking into account d_fr_refuge + site and having date as a random factor. 

#using lme function to get the p-values conveniently 
glmm_site_date_lme = lme(fixed = FIDi_sqrt ~ d_fr_refug_sqrt + site , random= ~ 1 |date, data=data_monitor, method = "ML")


summary(glmm_site_date_lme)

# d_fr_refug : p = 0.01 => significative correlation : value = 0.27 (is it the slop of the regression ?)
# site : p = 0.0003 (we already  knew that)

# we can check with interactions to see if correlation different in salaya or phutthamonthon 
summary(lme(fixed = FIDi_sqrt ~ d_fr_refug_sqrt * site , random= ~ 1 |date, data=data_monitor, method = "ML")
)

# WEIRD !!!!! when we take interaction the correlation isn't significative anymore
AIC (glmm_site_date_lme) - AIC(glmm_site_date_inter)
# moreover delta_AIC < 2 so no significative difference between the two models ?!?!?!?! 

plot(data_monitor$FIDi_sqrt ~ data_monitor$d_fr_refug_sqrt )
abline(a = 1.75, b = -0.054)

####### SALAYA #############

AICc(model_salaya, glmm_date_s)

#best is glmm_date_s 

summary(lme(fixed = FIDi_sqrt ~ d_fr_refug_sqrt, random = ~ 1|date, data=data_salaya, method = "ML"))


####### PHUTTHAMONTHON ##############

AICc(model_phuttha, glmm_date_p)

summary(model_phuttha)

plot(data_phuttha$FIDi_sqrt ~ data_phuttha$d_fr_refug_sqrt )
abline(a = 1.66, b = 0.03)
# best is model_puttha 