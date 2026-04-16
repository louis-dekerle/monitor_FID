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


land_use = read.csv("C:/Users/LENOVO/Desktop/stage mahidol/stat/area_land_use_20m.csv", header= T, sep = ";")


###### EXCLUDE OBS OTHER THAN V_SALVA ######
data_monitor = data_monitor[(data_monitor$sp=="V_salva" ) ,]
View(data_monitor)

###### EXCLUDE OBS WITHOUT FID  ###### 


data_monitor = data_monitor[!is.na(data_monitor$FIDi),]
View(data_monitor)

####### MERGE DATASETS ###### 

data_ld = merge(data_monitor,land_use,all=T)

####### SQUARE ROOT TRANSFORMATION ######

data_ld$FIDi_sqrt = sqrt(data_ld$FIDi)
View (data_ld)

########## ADD BINARY FOR MICRO-HABITAT PRESENCE #################

grass01 = data_ld$p_grass >0 
data_ld$grass01 = grass01

HS01 = data_ld$p_human_structure >0 
data_ld$HS01 = HS01

tree01 = data_ld$p_tree >0
data_ld$tree01 = tree01

water01 = data_ld$p_water >0 
data_ld$water01 = water01

##############################################
########## I/ GRASS  #######################
########################################"

boxplot(FIDi_sqrt ~ grass01, data =data_ld)  

plot(FIDi_sqrt ~ p_grass, data =data_ld) # lots of zeros lol 

hist(data_ld$FIDi_sqrt) #not so bad

hist (data_ld$p_grass) # absolutly not normal lol

boxplot(p_grass ~ site, data = data_ld)

########## A/ wilcoxon test keeping the zeros #############

wilcox.test(data_ld$p_grass ~ data_ld$site,alternative = "two.sided") # no effect of site on p_grass 


wilcox.test(data_ld$FIDi_sqrt ~ data_ld$grass01,alternative = "two.sided") # no effect of binary presence of grass on FID

############ B/ linear model with positive data ###############

data_grass_pos = data_ld[(data_ld$p_grass>0 ) ,]

######## a) PRELIMINARY CORRELATION ANALYSES #############

plot(FIDi_sqrt ~ p_grass, data=data_grass_pos, col="blue") 

cor(data_grass_pos$FIDi_sqrt, data_grass_pos$p_grass) 

cor.test(data_grass_pos$FIDi_sqrt, data_grass_pos$p_grass)  # nope 

#########################################################
######### 1) CLASSIC LINEAR MODEL WITH BOTH SITES ###########
######################################################

####i) Model ####
model_g <- lm(FIDi_sqrt ~ p_grass, data=data_grass_pos)

####ii) Conditions of validity ###### 
model_g.res <- residuals(model_g); model_g.res

# independance of residuals 
dwtest(model_g) # OK 
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_g) #big cluster on the left 

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_grass_pos, aes(model_g.res), bins = 12)
p # OK 

shapiro.test(model_g.res) # OKKK 

# homoskedasticity
bptest(model_g) # OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_g = simulateResiduals(model_g)
testSpatialAutocorrelation(res_dharma_model_g, x =  data_grass_pos$long, y = data_grass_pos$lat)
#  OK 

# temporal 


data_grass_pos$day=-1

data_grass_pos$day[1]=1
data_grass_pos$day[2:9]=2
data_grass_pos$day[10:14]=3
data_grass_pos$day[15:16]=4
data_grass_pos$day[17:20]=5
data_grass_pos$day[21:24]=6
data_grass_pos$day[25:28]=7
data_grass_pos$day[29]=8
data_grass_pos$day[30:34]=9
data_grass_pos$day[35:38]=10
data_grass_pos$day[39]=11
data_grass_pos$day[40]=12
data_grass_pos$day[41:42]=13
data_grass_pos$day[43:45]=14
data_grass_pos$day[46:47]=15
data_grass_pos$day[48:49]=16
data_grass_pos$day[50:53]=17
data_grass_pos$day[54:58]=18
data_grass_pos$day[59:63]=19
data_grass_pos$day[64:65]=20

# aggregating residuals by time
res_dharma_model_g = recalculateResiduals(res_dharma_model_g, group = data_grass_pos$day)
testTemporalAutocorrelation(res_dharma_model_g, time = unique(data_grass_pos$day))
# no autocorralation on a field day scale 

####iii) Results ####
summary(model_g) # P = 0.48


####################################################################
###### 2) CLASSIC LINEAR MODEL WITH INTERACTION D_F_S * SITE #####
#################################################################

####i) Model ####
model_g_inter <- lm(FIDi_sqrt ~ p_grass * site, data=data_grass_pos)

####ii) Conditions of validity ###### 
model_g_inter.res <- residuals(model_g_inter); model_g_inter.res

# independance of residuals 
dwtest(model_g_inter) # OK 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_g_inter)

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_grass_pos, aes(model_g_inter.res), bins = 12)
p # OK 

shapiro.test(model_g_inter.res) # OK

# homoskedasticity
bptest(model_g_inter) # OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_g_inter = simulateResiduals(model_g_inter)
testSpatialAutocorrelation(res_dharma_model_g_inter, x =  data_grass_pos$long, y = data_grass_pos$lat)
#  no spatial autocorrelation of course 

# temporal 

# put a number for each day because the function don't like strings 
data_grass_pos$day=-1

data_grass_pos$day[1]=1
data_grass_pos$day[2:9]=2
data_grass_pos$day[10:14]=3
data_grass_pos$day[15:16]=4
data_grass_pos$day[17:20]=5
data_grass_pos$day[21:24]=6
data_grass_pos$day[25:28]=7
data_grass_pos$day[29]=8
data_grass_pos$day[30:34]=9
data_grass_pos$day[35:38]=10
data_grass_pos$day[39]=11
data_grass_pos$day[40]=12
data_grass_pos$day[41:42]=13
data_grass_pos$day[43:45]=14
data_grass_pos$day[46:47]=15
data_grass_pos$day[48:49]=16
data_grass_pos$day[50:53]=17
data_grass_pos$day[54:58]=18
data_grass_pos$day[59:63]=19
data_grass_pos$day[64:65]=20

 # aggregating residuals by time
res_dharma_model_g_inter = recalculateResiduals(res_dharma_model_g_inter, group = data_grass_pos$day)
testTemporalAutocorrelation(res_dharma_model_g_inter, time = unique(data_grass_pos$day))
# no autocorralation on a field day scale 

#### iii) Results ####
summary(model_g_inter)# site significant AND EFFECT OF SITE ON relation GRASS VS FID

#### iv) plot ####
data_grass_pos$site <- as.factor(data_grass_pos$site)
plot_model(model_g_inter, type = "pred", terms = c("p_grass", "site"), data = data_grass_pos)

##################################
########    3) ANCOVA ##########
###############################

####i) Condition of validity  ####

# verify linearity between response variable and explanatory variable 
ggscatter(
  data_grass_pos, x = "p_grass", y = "FIDi_sqrt",
  color = "site", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(after_stat(eq.label), ..rr.label.., sep = "~~~~"), color = site)
  )# ouch big interaction 

#Homogeneity of regression slopes (no effect of the explenatory variable on the grouping variable (site))

data_grass_pos %>% anova_test(FIDi_sqrt ~ site*p_grass) # performs type II anova between FIDi, distance from shelter, site, and the interaction d_f_s + site 
# interaction is significant 

# Normality of residuals  

# Calculer le modèle, la covariable passe en premier
model_g_anc <- lm(FIDi_sqrt ~ p_grass + site, data = data_grass_pos)

model_g.metrics <- augment(model_g_anc) %>%
  select(-.hat, -.sigma, -.fitted)

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_grass_pos, aes(model_g.metrics$.resid), bins = 12)
p # OK

shapiro_test(model_g.metrics$.resid) #nope 

#Homogeneity of variances 

model_g.metrics %>% levene_test(.resid ~ site) # OK 

# outliers 

model_g.metrics %>% 
  filter(abs(.std.resid) > 3) %>% 
  as.data.frame()

#### ii) Model and Results ####

res.aov <- data_grass_pos %>% anova_test(FIDi_sqrt ~ p_grass + site)

get_anova_table(res.aov) # only site  

#########################################################
####### 4) CLASSIC LINEAR MODEL WITH ONLY SALAYA ######
#########################################################

# take only obs in salaya
data_salaya_g_pos =  data_grass_pos[(data_grass_pos$site=="salaya" ) ,]
View(data_salaya_g_pos)


# square root transformation


data_salaya_g_pos$FIDi_sqrt = sqrt(data_salaya_g_pos $FIDi)


View(data_salaya_g_pos )


####i) Model ####
model_salaya_g_pos  <- lm(FIDi_sqrt ~ p_grass, data=data_salaya_g_pos )


####ii) Conditions of validity ###### 
model_salaya_g_pos.res <- residuals(model_salaya_g_pos)

# independance of residuals 
dwtest(model_salaya_g_pos) # OK

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_salaya_g_pos ) #meh

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_salaya_g_pos = simulateResiduals(model_salaya_g_pos)
testSpatialAutocorrelation(res_dharma_salaya_g_pos, x =  data_salaya_g_pos$long, y = data_salaya_g_pos$lat)
# pas de spatial autocorrelation 

# temporal 

# put a number for each day because the function don't like strings 

data_salaya_g_pos$day=-1

data_salaya_g_pos$day[1:8]=1
data_salaya_g_pos$day[9:10]=2
data_salaya_g_pos$day[11:14]=3
data_salaya_g_pos$day[15:18]=4
data_salaya_g_pos$day[19]=5
data_salaya_g_pos$day[20]=6
data_salaya_g_pos$day[21]=7
data_salaya_g_pos$day[22:23]=8
data_salaya_g_pos$day[24:25]=9
data_salaya_g_pos$day[26:29]=10
data_salaya_g_pos$day[30:32]=11
data_salaya_g_pos$day[33:34]=12


# aggregating residuals by time
res_dharma_salaya_g_pos = recalculateResiduals(res_dharma_salaya_g_pos, group = data_salaya_g_pos$day)
testTemporalAutocorrelation(res_dharma_salaya_g_pos, time = unique(data_salaya_g_pos$day))
# no autocorralation on a field day scale 


# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_g_pos, aes(model_salaya_g_pos.res), bins = 12)
p # meh

shapiro.test(model_salaya_g_pos.res) # OK

# homoskedasticity
bptest(model_salaya_g_pos) # OK 

####iii) Results ####
summary(model_salaya_g_pos) # P = 0.13 

dev.off()
plot(FIDi_sqrt ~ p_grass, data=data_salaya_g_pos, col="blue") 
abline(a= model_salaya_g_pos$coefficients[1], b= model_salaya_g_pos$coefficients[2]) #seems correlated BUT pulled by the 6 big data points 


##################################################################
####### 4.5) CLASSIC LINEAR MODEL WITH ONLY PHUTTHAMONTHON ######
##################################################################

# take only obs in phutthamonthon 
data_phuttha_g_pos =  data_grass_pos[(data_grass_pos$site=="phutthamonthon" ) ,]
View(data_phuttha_g_pos)


# square root transformation


data_phuttha_g_pos$FIDi_sqrt = sqrt(data_phuttha_g_pos$FIDi)

View(data_phuttha_g_pos)


####i) Model ####
model_phuttha_g_pos <- lm(FIDi_sqrt ~ p_grass, data=data_phuttha_g_pos)


####ii) Conditions of validity ###### 
model_phuttha_g_pos.res <- residuals(model_phuttha_g_pos)

# independance of residuals 
dwtest(model_phuttha_g_pos) # INDEPENDANCE !!!!!! => OK 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_phuttha_g_pos) # okeyish

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha_g_pos, aes(model_phuttha_g_pos.res), bins = 12)
p # OK 

shapiro.test(model_phuttha_g_pos.res) # OK

# homoskedasticity
bptest(model_phuttha_g_pos) # homoscedaticity  

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_phuttha_g_pos = simulateResiduals(model_phuttha_g_pos)
testSpatialAutocorrelation(res_dharma_phuttha_g_pos, x =  data_phuttha_g_pos$long, y = data_phuttha_g_pos$lat)
# no spatial autocorrelation 

# temporal 

# put a number for each day because the function don't like strings 

data_phuttha_g_pos$day=-1

data_phuttha_g_pos$day[1]=1
data_phuttha_g_pos$day[2:6]=2
data_phuttha_g_pos$day[7:10]=3
data_phuttha_g_pos$day[11:15]=4
data_phuttha_g_pos$day[16:19]=5
data_phuttha_g_pos$day[20:22]=6
data_phuttha_g_pos$day[23:24]=7
data_phuttha_g_pos$day[25:29]=8
data_phuttha_g_pos$day[30:31]=9


# aggregating residuals by time
res_dharma_phuttha_g_pos = recalculateResiduals(res_dharma_phuttha_g_pos, group = data_phuttha_g_pos$day)
testTemporalAutocorrelation(res_dharma_phuttha_g_pos, time = unique(data_phuttha_g_pos$day))
# no autocorralation on a field day scale 

####iii) Results ####
summary(model_phuttha_g_pos) # non significant 

plot(FIDi_sqrt ~p_grass, data=data_phuttha_g_pos, col="blue") 
abline(a= model_phuttha_g_pos$coefficients[1], b= model_phuttha_g_pos$coefficients[2]) 
#dev.off()

#################################################
####### 5) GLMM TAKING DATE INTO ACCOUNT  ######
################################################

####i) MODEL #####
glmm_date_g_pos = lmer(FIDi_sqrt ~ p_grass + (1|date), data=data_grass_pos, REML = F)

####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_g_pos)
Fit = fitted(glmm_date_g_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # ouch 4 really patterny patterns 

# Residuals against covariates

#change the date format to %Y%M%D
data_grass_pos$date<- as.Date(data_grass_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
View(data_grass_pos)


data_grass_pos$date = as.factor(data_grass_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_grass_pos)
abline(h=0,lty=2)

plot (x = data_grass_pos$p_grass,
      y = data_grass_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_grass",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_grass_pos, aes(Res), bins = 12)
p  #OK 

shapiro.test(Res) #OK 


# outliers 

plot(data_grass_pos$p_grass) #OK 
plot(data_grass_pos$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_g_pos = simulateResiduals(glmm_date_g_pos)
testSpatialAutocorrelation(res_dharma_date_g_pos, x =  data_grass_pos$long, y = data_grass_pos$lat)
#  spatial autocorrelation logic 

# temporal 

# put a number for each day because the function don't like strings 
data_grass_pos$day=-1

data_grass_pos$day[1]=1
data_grass_pos$day[2:9]=2
data_grass_pos$day[10:14]=3
data_grass_pos$day[15:16]=4
data_grass_pos$day[17:20]=5
data_grass_pos$day[21:24]=6
data_grass_pos$day[25:28]=7
data_grass_pos$day[29]=8
data_grass_pos$day[30:34]=9
data_grass_pos$day[35:38]=10
data_grass_pos$day[39]=11
data_grass_pos$day[40]=12
data_grass_pos$day[41:42]=13
data_grass_pos$day[43:45]=14
data_grass_pos$day[46:47]=15
data_grass_pos$day[48:49]=16
data_grass_pos$day[50:53]=17
data_grass_pos$day[54:58]=18
data_grass_pos$day[59:63]=19
data_grass_pos$day[64:65]=20

 
# aggregating residuals by time
res_dharma_date_g_pos = recalculateResiduals(res_dharma_date_g_pos, group = data_grass_pos$day)
testTemporalAutocorrelation(res_dharma_date_g_pos, time = unique(data_grass_pos$day))
# no autocorralation on a field day scale 

#####iii) RESULTS #####

summary(glmm_date_g_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
#ICC : 0.22
r.squaredGLMM(glmm_date_g_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.0007 et R2c = 0.22 so nope for correlation lol 
# NONETHELESS !!!! SITE NOT TAKEN INTO ACCOUNT  !!!! HENCE THE EFFECT OF THE DATE SO STRONG PROBABLY 


#############################################
####### 6) GLMM DATE WITH ONLY SALAYA  ######
#############################################


# take only obs in salaya
data_salaya_g_pos =  data_grass_pos[(data_grass_pos$site=="salaya" ) ,]
View(data_salaya_g_pos)


# square root transformation


data_salaya_g_pos$FIDi_sqrt = sqrt(data_salaya_g_pos$FIDi)


View(data_salaya_g_pos )


####i) MODEL #####
glmm_date_s_g_pos = lmer(FIDi_sqrt ~ p_grass + (1|date), data=data_salaya_g_pos, REML = F)
# singular grrrr

####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_s_g_pos)
Fit = fitted(glmm_date_s_g_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_salaya_g_pos$date<- as.Date(data_salaya_g_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_salaya_g_pos)


data_salaya_g_pos$date = as.factor(data_salaya_g_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_salaya_g_pos)
abline(h=0,lty=2)

plot (x = data_grass_pos$p_grass,
      y = data_grass_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_grass",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_g_pos, aes(Res), bins = 12)
p  #meh 

shapiro.test(Res) #OK 


# outliers 

plot(data_salaya_g_pos$p_grass) #OK il y une petite valeur forte mais pas choquant
plot(data_salaya_g_pos$FIDi_sqrt) #OK 


# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_s_g_pos = simulateResiduals(glmm_date_s_g_pos)
testSpatialAutocorrelation(res_dharma_date_s_g_pos, x =  data_salaya_g_pos$long, y = data_salaya_g_pos$lat)
# no spatial autocorrelation 

# temporal 

# put a number for each day because the function don't like strings 




data_salaya_g_pos$day=-1

data_salaya_g_pos$day[1:8]=1
data_salaya_g_pos$day[9:10]=2
data_salaya_g_pos$day[11:14]=3
data_salaya_g_pos$day[15:18]=4
data_salaya_g_pos$day[19]=5
data_salaya_g_pos$day[20]=6
data_salaya_g_pos$day[21]=7
data_salaya_g_pos$day[22:23]=8
data_salaya_g_pos$day[24:25]=9
data_salaya_g_pos$day[26:29]=10
data_salaya_g_pos$day[30:32]=11
data_salaya_g_pos$day[33:34]=12


# aggregating residuals by time
res_dharma_date_s_g_pos = recalculateResiduals(res_dharma_date_s_g_pos, group = data_salaya_g_pos$day)
testTemporalAutocorrelation(res_dharma_date_s_g_pos, time = unique(data_salaya_g_pos$day))
# no autocorralation on a field day scale 

#####iii) RESULTS #####



anova(glmm_date_s_HS_pos,model_salaya_HS_pos) #effect of the date p = 0.003

summary(glmm_date_s_g_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0 # no effect of date for some reason 

r.squaredGLMM(glmm_date_s_g_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.07 et R2c = 0.07 

##### iv) plot ####


# didn't manage to do it and I'm tired lol


########################################################
####### 6.5) GLMM DATE WITH ONLY PHUTTHAMONTHON  ######
#######################################################


# take only obs in phutthamonthon 
data_phuttha_g_pos =  data_grass_pos[(data_grass_pos$site=="phutthamonthon" ) ,]
View(data_phuttha_g_pos)


# square root transformation


data_phuttha_g_pos$FIDi_sqrt = sqrt(data_phuttha_g_pos$FIDi)

View(data_phuttha_g_pos)


####i) MODEL #####
glmm_date_p_g_pos = lmer(FIDi_sqrt ~ p_grass + (1|date), data=data_phuttha_g_pos, REML = F)
# singularity  is bad !!!!!


####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_p_g_pos)
Fit = fitted(glmm_date_p_g_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_phuttha_g_pos$date<- as.Date(data_phuttha_g_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_phuttha_g_pos)


data_phuttha_g_pos$date = as.factor(data_phuttha_g_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_phuttha_g_pos)
abline(h=0,lty=2)

plot (x = data_phuttha_g_pos$p_grass,
      y = data_phuttha_g_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_grass",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha_g_pos, aes(Res), bins = 12)
p  

shapiro.test(Res) #OK 


# outliers 

plot(data_phuttha_g_pos$p_grass) #OK 2 strong values but we'll make do 
plot(data_phuttha_g_pos$FIDi_sqrt) #OK  same

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_p_g_pos = simulateResiduals(glmm_date_p_g_pos)
testSpatialAutocorrelation(res_dharma_date_p_g_pos, x =  data_phuttha_g_pos$long, y = data_phuttha_g_pos$lat)
# no spatial autocorrelation 

# temporal 


# put a number for each day because the function don't like strings 
data_phuttha_g_pos$day=-1

data_phuttha_g_pos$day[1]=1
data_phuttha_g_pos$day[2:6]=2
data_phuttha_g_pos$day[7:10]=3
data_phuttha_g_pos$day[11:15]=4
data_phuttha_g_pos$day[16:19]=5
data_phuttha_g_pos$day[20:22]=6
data_phuttha_g_pos$day[23:24]=7
data_phuttha_g_pos$day[25:29]=8
data_phuttha_g_pos$day[30:31]=9


# aggregating residuals by time
res_dharma_date_p_g_pos = recalculateResiduals(res_dharma_date_p_g_pos, group = data_phuttha_g_pos$day)
testTemporalAutocorrelation(res_dharma_date_p_g_pos, time = unique(data_phuttha_g_pos$day))
# no autocorralation on a field day scale 

#####iii) RESULTS #####

summary(glmm_date_p_g_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.00 #singularity is bad 

r.squaredGLMM(glmm_date_p_g_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.07 et R2c = 0.07


##################################
#### 8) GLMM WITH SITES AS FIXED FACTOR AND DATE AS RANDOM #######
###############################


####i) MODEL #####

glmm_site_date_inter_g_pos = lmer(FIDi_sqrt ~ p_grass * site + (1|date), data=data_grass_pos, REML = F)


#glmm_site_date_g_pos = lmer(FIDi_sqrt ~ p_grass + site + (1|date), data=data_grass_pos, REML = F)

#AIC(glmm_site_date_g_pos,glmm_site_date_inter_g_pos) 

#AIC is smaller when accounting for the interaction so we only take the model with it from now on

####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_site_date_inter_g_pos)
Fit = fitted(glmm_site_date_inter_g_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_grass_pos$date<- as.Date(data_grass_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_grass_pos)


data_grass_pos$date = as.factor(data_grass_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_grass_pos)
abline(h=0,lty=2)

boxplot(Res ~ site, xlab = "site", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_grass_pos)
abline(h=0,lty=2)

plot (x = data_grass_pos$p_grass,
      y = data_grass_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_grass",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_grass_pos, aes(Res), bins = 12)
p  

shapiro.test(Res) # OK 


# outliers 

plot(data_grass_pos$p_grass) #OK 
plot(data_grass_pos$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_site_date_inter_g_pos = simulateResiduals(glmm_site_date_inter_g_pos)
testSpatialAutocorrelation(res_dharma_site_date_inter_g_pos, x =  data_grass_pos$long, y = data_grass_pos$lat)
# no spatial autocorrelation  

# temporal 

# put a number for each day because the function don't like strings 
data_grass_pos$day=-1

data_grass_pos$day[1]=1
data_grass_pos$day[2:9]=2
data_grass_pos$day[10:14]=3
data_grass_pos$day[15:16]=4
data_grass_pos$day[17:20]=5
data_grass_pos$day[21:24]=6
data_grass_pos$day[25:28]=7
data_grass_pos$day[29]=8
data_grass_pos$day[30:34]=9
data_grass_pos$day[35:38]=10
data_grass_pos$day[39]=11
data_grass_pos$day[40]=12
data_grass_pos$day[41:42]=13
data_grass_pos$day[43:45]=14
data_grass_pos$day[46:47]=15
data_grass_pos$day[48:49]=16
data_grass_pos$day[50:53]=17
data_grass_pos$day[54:58]=18
data_grass_pos$day[59:63]=19
data_grass_pos$day[64:65]=20

# aggregating residuals by time
res_dharma_site_date_inter_g_pos = recalculateResiduals(res_dharma_site_date_inter_g_pos, group = data_grass_pos$day)
testTemporalAutocorrelation(res_dharma_site_date_inter_g_pos, time = unique(data_grass_pos$day))
# no autocorrelation on a field day scale 

#####iii) RESULTS #####

summary(glmm_site_date_inter_g_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.01 
r.squaredGLMM(glmm_site_date_inter_g_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.18 et R2c = 0.19


################################################
###########MODEL COMPARISON#####################
################################################

AICc(model_g, model_g_anc, model_g_inter, glmm_date_g_pos, glmm_site_date_inter_g_pos)
min(AICc(model_g, model_g_anc, model_g_inter, glmm_date_g_pos, glmm_site_date_inter_g_pos)[,2])

# model_g_inter is the best 

summary(model_g_inter)# no effect of grass on FID but effect of site on the relation between grass and FID. 

# check salaya and phutthamonthon separatly 


#Salaya 
AICc(model_salaya_g_pos, glmm_date_s_g_pos)

summary(model_salaya_g_pos) #not significant  

#Phutthamothon 
AICc(model_phuttha_g_pos, glmm_date_p_g_pos)

summary(model_phuttha_g_pos) # p=0.14

########################################################"
####################II/ HUMAN STRUCTURE#####################
##########################################################

boxplot(FIDi_sqrt ~ HS01, data =data_ld)  

plot(FIDi_sqrt ~ p_human_structure, data =data_ld)   

hist(data_ld$FIDi_sqrt) #not so bad

hist (data_ld$p_human_structure) # absolutly not normal lol

boxplot(p_human_structure ~ site, data = data_ld) # lots of zero in phutthamothon => logic 

########## A) wilcoxon test keeping the zeros #############

wilcox.test(data_ld$p_human_structure ~ data_ld$site,alternative = "two.sided") # effect of site => lots less proportions of HS in Phutthamonthon 


wilcox.test(data_ld$FIDi_sqrt ~ data_ld$HS01,alternative = "two.sided") # no effect of binary presence of HS on FID

############ B/ linear model with positive data ###############

data_HS_pos = data_ld[(data_ld$p_human_structure>0 ) ,]
View(data_HS_pos)

######## a) PRELIMINARY CORRELATION ANALYSES #############

plot(FIDi_sqrt ~ p_human_structure, data=data_HS_pos, col="blue") 

cor(data_HS_pos$FIDi_sqrt, data_HS_pos$p_human_structure) 

cor.test(data_HS_pos$FIDi_sqrt, data_HS_pos$p_human_structure) # P = 0.005 ; c = -0.37


#########################################################
######### 1) CLASSIC LINEAR MODEL WITH BOTH SITES ###########
######################################################

####i) Model ####
model_hs <- lm(FIDi_sqrt ~ p_human_structure, data=data_HS_pos)

####ii) Conditions of validity ###### 
model_hs.res <- residuals(model_hs); model_hs.res

# independance of residuals 
dwtest(model_hs) # nope
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_hs) #big cluster on the right 

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_HS_pos, aes(model_hs.res), bins = 12)
p # OK 

shapiro.test(model_hs.res) # OKKK 

# homoskedasticity
bptest(model_hs) # not ok 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_hs = simulateResiduals(model_hs)
testSpatialAutocorrelation(res_dharma_model_hs, x =  data_HS_pos$long, y = data_HS_pos$lat)
#  ouch mega nope  

# temporal 
/!\/!\/!\/!\/!\/!\
/!\ DO NOT FORGET TO UPDATE DATASET TO NEWEST VERSION /!\
/!\/!\/!\/!\/!\/!\

data_HS_pos$day=-1

data_HS_pos$day[1:2]=1
data_HS_pos$day[3:13]=2
data_HS_pos$day[14]=3
data_HS_pos$day[15:16]=4
data_HS_pos$day[17:18]=5
data_HS_pos$day[19:20]=6
data_HS_pos$day[21:24]=7
data_HS_pos$day[25]=8
data_HS_pos$day[26:28]=9
data_HS_pos$day[29]=10
data_HS_pos$day[30]=11
data_HS_pos$day[31:32]=12
data_HS_pos$day[33]=13
data_HS_pos$day[34:38]=14
data_HS_pos$day[39:41]=15
data_HS_pos$day[42:45]=16
data_HS_pos$day[46:50]=17
data_HS_pos$day[51:53]=18

# aggregating residuals by time
res_dharma_model_hs = recalculateResiduals(res_dharma_model_hs, group = data_HS_pos$day)
testTemporalAutocorrelation(res_dharma_model_hs, time = unique(data_HS_pos$day))
# no autocorralation on a field day scale 

####iii) Results ####
summary(model_hs) # P = 0.005 ; spatial autocorrelation 

####################################################################
###### 2) CLASSIC LINEAR MODEL WITH INTERACTION D_F_S * SITE #####
#################################################################

####i) Model ####
model_HS_inter <- lm(FIDi_sqrt ~ p_human_structure * site, data=data_HS_pos)

####ii) Conditions of validity ###### 
model_HS_inter.res <- residuals(model_HS_inter); model_HS_inter.res

# independance of residuals 
dwtest(model_HS_inter) # OK 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_HS_inter) # 2 big clusters probably for the sites 

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_HS_pos, aes(model_HS_inter.res), bins = 12)
p # OK 

shapiro.test(model_HS_inter.res) # OK  

# homoskedasticity
bptest(model_HS_inter) # OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_HS_inter = simulateResiduals(model_HS_inter)
testSpatialAutocorrelation(res_dharma_model_HS_inter, x =  data_HS_pos$long, y = data_HS_pos$lat)
#  no spatial autocorrelation of course 

# temporal 

# put a number for each day because the function don't like strings 

data_HS_pos$day=-1

data_HS_pos$day[1:2]=1
data_HS_pos$day[3:13]=2
data_HS_pos$day[14]=3
data_HS_pos$day[15:16]=4
data_HS_pos$day[17:18]=5
data_HS_pos$day[19:20]=6
data_HS_pos$day[21:24]=7
data_HS_pos$day[25]=8
data_HS_pos$day[26:28]=9
data_HS_pos$day[29]=10
data_HS_pos$day[30]=11
data_HS_pos$day[31:32]=12
data_HS_pos$day[33]=13
data_HS_pos$day[34:38]=14
data_HS_pos$day[39:41]=15
data_HS_pos$day[42:45]=16
data_HS_pos$day[46:50]=17
data_HS_pos$day[51:53]=18

 # aggregating residuals by time
res_dharma_model_HS_inter = recalculateResiduals(res_dharma_model_HS_inter, group = data_HS_pos$day)
testTemporalAutocorrelation(res_dharma_model_HS_inter, time = unique(data_HS_pos$day))
# no autocorralation on a field day scale 

#### iii) Results ####
summary(model_HS_inter)# site alomst significant but that's all 

##################################
########    3) ANCOVA ##########
###############################

####i) Condition of validity  ####

# verify linearity between response variable and explanatory variable 
ggscatter(
  data_HS_pos, x = "p_human_structure", y = "FIDi_sqrt",
  color = "site", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(after_stat(eq.label), ..rr.label.., sep = "~~~~"), color = site)
  )# does't smell big correlation but same direction between P et S for once 

#Homogeneity of regression slopes (no effect of the explenatory variable on the grouping variable (site))

data_HS_pos %>% anova_test(FIDi_sqrt ~ site*p_human_structure) # performs type II anova between FIDi, distance from shelter, site, and the interaction d_f_s + site 
 #only site is sginficant 

# Normality of residuals  

# Calculer le modèle, la covariable passe en premier
model_HS_anc <- lm(FIDi_sqrt ~ p_human_structure + site, data = data_HS_pos)

model_HS.metrics <- augment(model_HS_anc) %>%
  select(-.hat, -.sigma, -.fitted)

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_HS_pos, aes(model_HS.metrics$.resid), bins = 12)
p # OK

shapiro_test(model_HS.metrics$.resid) #ok 

#Homogeneity of variances 

model_HS.metrics %>% levene_test(.resid ~ site) # OK 

# outliers 

model_HS.metrics %>% 
  filter(abs(.std.resid) > 3) %>% 
  as.data.frame()

#### ii) Model and Results ####

res.aov <- data_HS_pos %>% anova_test(FIDi_sqrt ~ p_human_structure + site)

get_anova_table(res.aov) # only site 

#########################################################
####### 4) CLASSIC LINEAR MODEL WITH ONLY SALAYA ######
#########################################################

# take only obs in salaya
data_salaya_HS_pos =  data_HS_pos[(data_HS_pos$site=="salaya" ) ,]
View(data_salaya_HS_pos)


# square root transformation


data_salaya_HS_pos$FIDi_sqrt = sqrt(data_salaya_HS_pos $FIDi)


View(data_salaya_HS_pos )


####i) Model ####
model_salaya_HS_pos  <- lm(FIDi_sqrt ~ p_human_structure, data=data_salaya_HS_pos )


####ii) Conditions of validity ###### 
model_salaya_HS_pos.res <- residuals(model_salaya_HS_pos)

# independance of residuals 
dwtest(model_salaya_HS_pos) #  ok 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_salaya_HS_pos )

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_salaya_HS_pos = simulateResiduals(model_salaya_HS_pos)
testSpatialAutocorrelation(res_dharma_salaya_HS_pos, x =  data_salaya_HS_pos$long, y = data_salaya_HS_pos$lat)
# spatial autocorrelation !!!!!!!!!

# temporal 

# put a number for each day because the function don't like strings 

data_salaya_HS_pos$day=-1

data_salaya_HS_pos$day[1:11]=1
data_salaya_HS_pos$day[12:13]=2
data_salaya_HS_pos$day[14:15]=3
data_salaya_HS_pos$day[16:19]=4
data_salaya_HS_pos$day[20]=5
data_salaya_HS_pos$day[21:22]=6
data_salaya_HS_pos$day[23:27]=7
data_salaya_HS_pos$day[28:31]=8
data_salaya_HS_pos$day[32:36]=9
data_salaya_HS_pos$day[37:39]=10



# aggregating residuals by time
res_dharma_salaya_HS_pos = recalculateResiduals(res_dharma_salaya_HS_pos, group = data_salaya_HS_pos$day)
testTemporalAutocorrelation(res_dharma_salaya_HS_pos, time = unique(data_salaya_HS_pos$day))
# no autocorralation on a field day scale 


# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_HS_pos, aes(model_salaya_HS_pos.res), bins = 12)
p # OK

shapiro.test(model_salaya_HS_pos.res) #OK

# homoskedasticity
bptest(model_salaya_HS_pos) # OK 

####iii) Results ####
summary(model_salaya_HS_pos) # not significant ; 

dev.off()
plot(FIDi_sqrt ~ p_human_structure, data=data_salaya_HS_pos, col="blue") 
abline(a= model_salaya_HS_pos$coefficients[1], b= model_salaya_HS_pos$coefficients[2]) 


##################################################################
####### 4.5) CLASSIC LINEAR MODEL WITH ONLY PHUTTHAMONTHON ######
##################################################################

# take only obs in phutthamonthon 
data_phuttha_HS_pos =  data_HS_pos[(data_HS_pos$site=="phutthamonthon" ) ,]
View(data_phuttha_HS_pos)


# square root transformation


data_phuttha_HS_pos$FIDi_sqrt = sqrt(data_phuttha_HS_pos$FIDi)

View(data_phuttha_HS_pos)


####i) Model ####
model_phuttha_HS_pos <- lm(FIDi_sqrt ~ p_human_structure, data=data_phuttha_HS_pos)


####ii) Conditions of validity ###### 
model_phuttha_HS_pos.res <- residuals(model_phuttha_HS_pos)

# independance of residuals 
dwtest(model_phuttha_HS_pos) # INDEPENDANCE !!!!!! => OK 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_phuttha_HS_pos) # okeyish

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha_HS_pos, aes(model_phuttha_HS_pos.res), bins = 12)
p # WTF 

shapiro.test(model_phuttha_HS_pos.res) # OK jpp

# homoskedasticity
bptest(model_phuttha_HS_pos) # homoscedaticity  

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_phuttha_HS_pos = simulateResiduals(model_phuttha_HS_pos)
testSpatialAutocorrelation(res_dharma_phuttha_HS_pos, x =  data_phuttha_HS_pos$long, y = data_phuttha_HS_pos$lat)
# spatial autocorrelation !!!!!

# temporal 

# put a number for each day because the function don't like strings 

data_phuttha_HS_pos$day=-1

data_phuttha_HS_pos$day[1:2]=1
data_phuttha_HS_pos$day[3]=2
data_phuttha_HS_pos$day[4:5]=3
data_phuttha_HS_pos$day[6]=4
data_phuttha_HS_pos$day[7:9]=5
data_phuttha_HS_pos$day[10]=6
data_phuttha_HS_pos$day[11]=7
data_phuttha_HS_pos$day[12:14]=8


# aggregating residuals by time
res_dharma_phuttha_HS_pos = recalculateResiduals(res_dharma_phuttha_HS_pos, group = data_phuttha_HS_pos$day)
testTemporalAutocorrelation(res_dharma_phuttha_HS_pos, time = unique(data_phuttha_HS_pos$day))
# no autocorralation on a field day scale 

####iii) Results ####
summary(model_phuttha_HS_pos) # non significant 

dev.off()
plot(FIDi_sqrt ~p_human_structure, data=data_phuttha_HS_pos, col="blue") 
abline(a= model_phuttha_HS_pos$coefficients[1], b= model_phuttha_HS_pos$coefficients[2]) 


#################################################
####### 5) GLMM TAKING DATE INTO ACCOUNT  ######
################################################

####i) MODEL #####
glmm_date_HS_pos = lmer(FIDi_sqrt ~ p_human_structure + (1|date), data=data_HS_pos, REML = F)

####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_HS_pos)
Fit = fitted(glmm_date_HS_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # ouch 4 really patterny patterns 

# Residuals against covariates

#change the date format to %Y%M%D
data_HS_pos$date<- as.Date(data_HS_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
View(data_HS_pos)


data_HS_pos$date = as.factor(data_HS_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_HS_pos)
abline(h=0,lty=2)

plot (x = data_HS_pos$p_human_structure,
      y = data_HS_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_human_structure",
      cex = 1.2 , pch = 16) #meh lots of near zero values and triangle shape 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_HS_pos, aes(Res), bins = 12)
p  #OK 

shapiro.test(Res) #OK 


# outliers 

plot(data_HS_pos$p_human_structure) #OK 
plot(data_HS_pos$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_HS_pos = simulateResiduals(glmm_date_HS_pos)
testSpatialAutocorrelation(res_dharma_date_HS_pos, x =  data_HS_pos$long, y = data_HS_pos$lat)
#  big nope 

# temporal 

# put a number for each day because the function don't like strings 

data_HS_pos$day=-1

data_HS_pos$day[1:2]=1
data_HS_pos$day[3:13]=2
data_HS_pos$day[14]=3
data_HS_pos$day[15:16]=4
data_HS_pos$day[17:18]=5
data_HS_pos$day[19:20]=6
data_HS_pos$day[21:24]=7
data_HS_pos$day[25]=8
data_HS_pos$day[26:28]=9
data_HS_pos$day[29]=10
data_HS_pos$day[30]=11
data_HS_pos$day[31:32]=12
data_HS_pos$day[33]=13
data_HS_pos$day[34:38]=14
data_HS_pos$day[39:41]=15
data_HS_pos$day[42:45]=16
data_HS_pos$day[46:50]=17
data_HS_pos$day[51:53]=18


# aggregating residuals by time
res_dharma_date_HS_pos = recalculateResiduals(res_dharma_date_HS_pos, group = data_HS_pos$day)
testTemporalAutocorrelation(res_dharma_date_HS_pos, time = unique(data_HS_pos$day))
# no autocorralation on a field day scale 

#####iii) RESULTS #####

summary(glmm_date_HS_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
#ICC : 0.40
r.squaredGLMM(glmm_date_HS_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.08 et R2c = 0.46 so nope for correlation lol 
# NONETHELESS !!!! SITE NOT TAKEN INTO ACCOUNT  !!!! HENCE THE EFFECT OF THE DATE SO STRONG PROBABLY 


#############################################
####### 6) GLMM DATE WITH ONLY SALAYA  ######
#############################################


# take only obs in salaya
data_salaya_HS_pos =  data_HS_pos[(data_HS_pos$site=="salaya" ) ,]
View(data_salaya_HS_pos)


# square root transformation


data_salaya_HS_pos$FIDi_sqrt = sqrt(data_salaya_HS_pos$FIDi)


View(data_salaya_HS_pos )


####i) MODEL #####
glmm_date_s_HS_pos = lmer(FIDi_sqrt ~ p_human_structure + (1|date), data=data_salaya_HS_pos, REML = F)

####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_s_HS_pos)
Fit = fitted(glmm_date_s_HS_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_salaya_HS_pos$date<- as.Date(data_salaya_HS_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_salaya_HS_pos)


data_salaya_HS_pos$date = as.factor(data_salaya_HS_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_salaya_HS_pos)
abline(h=0,lty=2)

plot (x = data_HS_pos$p_human_structure,
      y = data_HS_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_human_structure",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_HS_pos, aes(Res), bins = 12)
p  #meh 

shapiro.test(Res) #OK 


# outliers 

plot(data_salaya_HS_pos$p_human_structure) #OK il y une petite valeur forte mais pas choquant
plot(data_salaya_HS_pos$FIDi_sqrt) #OK 


# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_s_HS_pos = simulateResiduals(glmm_date_s_HS_pos)
testSpatialAutocorrelation(res_dharma_date_s_HS_pos, x =  data_salaya_HS_pos$long, y = data_salaya_HS_pos$lat)
# spatial autocorrelation 

# temporal 

# put a number for each day because the function don't like strings 



data_salaya_HS_pos$day=-1

data_salaya_HS_pos$day[1:11]=1
data_salaya_HS_pos$day[12:13]=2
data_salaya_HS_pos$day[14:15]=3
data_salaya_HS_pos$day[16:19]=4
data_salaya_HS_pos$day[20]=5
data_salaya_HS_pos$day[21:22]=6
data_salaya_HS_pos$day[23:27]=7
data_salaya_HS_pos$day[28:31]=8
data_salaya_HS_pos$day[32:36]=9
data_salaya_HS_pos$day[37:39]=10


# aggregating residuals by time
res_dharma_date_s_HS_pos = recalculateResiduals(res_dharma_date_s_HS_pos, group = data_salaya_HS_pos$day)
testTemporalAutocorrelation(res_dharma_date_s_HS_pos, time = unique(data_salaya_HS_pos$day))
# no autocorralation on a field day scale 

#####iii) RESULTS #####

summary(glmm_date_s_HS_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.52 # no effect of date for some reason 

r.squaredGLMM(glmm_date_s_HS_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.045 et R2c = 0.55 

##### iv) plot #####

mydf <- ggpredict(glmm_date_s_HS_pos, terms = c("p_human_structure"))

ggplot(mydf, aes(x, predicted)) +
  geom_point(data=mydf, aes(data_salaya_HS_pos$FIDi_sqrt,  data_salaya_HS_pos$p_human_structure), alpha = 0.5) + 
  geom_line(aes(color = group)) +
  labs(x = "p_human_structure", y = "sqrt_FID")


########################################################
####### 6.5) GLMM DATE WITH ONLY PHUTTHAMONTHON  ######
#######################################################


# take only obs in phutthamonthon 
data_phuttha_HS_pos =  data_HS_pos[(data_HS_pos$site=="phutthamonthon" ) ,]
View(data_phuttha_HS_pos)


# square root transformation


data_phuttha_HS_pos$FIDi_sqrt = sqrt(data_phuttha_HS_pos$FIDi)

View(data_phuttha_HS_pos)


####i) MODEL #####
glmm_date_p_HS_pos = lmer(FIDi_sqrt ~ p_human_structure + (1|date), data=data_phuttha_HS_pos, REML = F)
# singularity  is bad !!!!!


####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_p_HS_pos)
Fit = fitted(glmm_date_p_HS_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_phuttha_HS_pos$date<- as.Date(data_phuttha_HS_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
View(data_phuttha_HS_pos)


data_phuttha_HS_pos$date = as.factor(data_phuttha_HS_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_phuttha_HS_pos)
abline(h=0,lty=2)

plot (x = data_phuttha_HS_pos$p_human_structure,
      y = data_phuttha_HS_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_human_structure",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha_HS_pos, aes(Res), bins = 12)
p  #WTF 

shapiro.test(Res) #OK 


# outliers 

plot(data_phuttha_HS_pos$p_human_structure) #OK 2 strong values but we'll make do 
plot(data_phuttha_HS_pos$FIDi_sqrt) #OK  same

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_p_HS_pos = simulateResiduals(glmm_date_p_HS_pos)
testSpatialAutocorrelation(res_dharma_date_p_HS_pos, x =  data_phuttha_HS_pos$long, y = data_phuttha_HS_pos$lat)
# spatial autocorrelation !!!!!!

# temporal 


# put a number for each day because the function don't like strings 

data_phuttha_HS_pos$day=-1

data_phuttha_HS_pos$day[1:2]=1
data_phuttha_HS_pos$day[3]=2
data_phuttha_HS_pos$day[4:5]=3
data_phuttha_HS_pos$day[6]=4
data_phuttha_HS_pos$day[7:9]=5
data_phuttha_HS_pos$day[10]=6
data_phuttha_HS_pos$day[11]=7
data_phuttha_HS_pos$day[12:14]=8


# aggregating residuals by time
res_dharma_date_p_HS_pos = recalculateResiduals(res_dharma_date_p_HS_pos, group = data_phuttha_HS_pos$day)
testTemporalAutocorrelation(res_dharma_date_p_HS_pos, time = unique(data_phuttha_HS_pos$day))
# no autocorralation on a field day scale 

#####iii) RESULTS #####

summary(glmm_date_p_HS_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.00 #singularity is bad 

r.squaredGLMM(glmm_date_p_HS_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.004 et R2c = 0.004


##################################
#### 8) GLMM WITH SITES AS FIXED FACTOR AND DATE AS RANDOM #######
###############################


####i) MODEL #####

#glmm_site_date_inter_HS_pos = lmer(FIDi_sqrt ~ p_human_structure * site + (1|date), data=data_HS_pos, REML = F)


glmm_site_date_HS_pos = lmer(FIDi_sqrt ~ p_human_structure + site + (1|date), data=data_HS_pos, REML = F)

#AIC(glmm_site_date_HS_pos,glmm_site_date_inter_HS_pos) 

#AIC is smaller when not accounting for the interaction so we only take the model without from now on

####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_site_date_HS_pos)
Fit = fitted(glmm_site_date_HS_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_HS_pos$date<- as.Date(data_HS_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_HS_pos)


data_HS_pos$date = as.factor(data_HS_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_HS_pos)
abline(h=0,lty=2)

boxplot(Res ~ site, xlab = "site", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_HS_pos)
abline(h=0,lty=2)

plot (x = data_HS_pos$p_human_structure,
      y = data_HS_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_human_structure",
      cex = 1.2 , pch = 16) # kinda trianglish 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_HS_pos, aes(Res), bins = 12)
p  

shapiro.test(Res) # OK 


# outliers 

plot(data_HS_pos$p_human_structure) #OK 
plot(data_HS_pos$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_site_date_HS_pos = simulateResiduals(glmm_site_date_HS_pos)
testSpatialAutocorrelation(res_dharma_site_date_HS_pos, x =  data_HS_pos$long, y = data_HS_pos$lat)
# no spatial autocorrelation  

# temporal 

# put a number for each day because the function don't like strings 

data_HS_pos$day=-1

data_HS_pos$day[1:2]=1
data_HS_pos$day[3:13]=2
data_HS_pos$day[14]=3
data_HS_pos$day[15:16]=4
data_HS_pos$day[17:18]=5
data_HS_pos$day[19:20]=6
data_HS_pos$day[21:24]=7
data_HS_pos$day[25]=8
data_HS_pos$day[26:28]=9
data_HS_pos$day[29]=10
data_HS_pos$day[30]=11
data_HS_pos$day[31:32]=12
data_HS_pos$day[33]=13
data_HS_pos$day[34:38]=14
data_HS_pos$day[39:41]=15
data_HS_pos$day[42:45]=16
data_HS_pos$day[46:50]=17
data_HS_pos$day[51:53]=18

# aggregating residuals by time
res_dharma_site_date_HS_pos = recalculateResiduals(res_dharma_site_date_HS_pos, group = data_HS_pos$day)
testTemporalAutocorrelation(res_dharma_site_date_HS_pos, time = unique(data_HS_pos$day))
# no autocorrelation on a field day scale 

#####iii) RESULTS #####

summary(glmm_site_date_HS_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.26 # singularity 
r.squaredGLMM(glmm_site_date_HS_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.27 et R2c = 0.45



################################################
###########MODEL COMPARISON#####################
################################################

AICc(model_hs, model_HS_anc, model_HS_inter, glmm_date_HS_pos, glmm_site_date_HS_pos)
min(AICc(model_hs, model_HS_anc, model_HS_inter, glmm_date_HS_pos, glmm_site_date_HS_pos)[,2])

# model_HS_anc is the best 

summary(model_HS_anc)# no effect of HS on FID and no effect of site on the relation between HS and FID. 
#ONLY EFFECT OF SITE ON FID 
#validity OK 

# check salaya and phutthamonthon separatly 


#Salaya 
AICc(model_salaya_HS_pos, glmm_date_s_HS_pos)


summary(glmm_date_s_HS_pos) 

summary(lme(fixed = FIDi_sqrt ~ p_human_structure, random = ~ 1 | date, method = "ML", data = data_salaya_HS_pos))

#not significant + spatial autocorr

plot(FIDi_sqrt ~ p_human_structure, data=data_salaya_HS_pos, col="blue") 
abline(a= 2.2779705, b= -0.8762325)

# test LM classic salaya avec GLS au cas où 

model_gls_exp <- gls( FIDi_sqrt ~ p_human_structure, 
                      data = data_salaya_HS_pos, 
                      correlation = corExp(form = ~ long + lat, nugget = TRUE))
summary(model_gls_exp)

model_gls_gaus <- gls( FIDi_sqrt ~ p_human_structure, 
                       data = data_salaya_HS_pos, 
                       correlation = corGaus(form = ~ long + lat, nugget = TRUE))
summary(model_gls_gaus)

model_gls_lin <- gls( FIDi_sqrt ~ p_human_structure, 
                      data = data_salaya_HS_pos, 
                      correlation = corLin(form = ~ long + lat, nugget = TRUE))
summary(model_gls_lin)

model_gls_ratio <- gls( FIDi_sqrt ~ p_human_structure, 
                        data = data_salaya_HS_pos, 
                        correlation = corRatio(form = ~ long + lat, nugget = TRUE))
summary(model_gls_ratio)

model_gls_spher <- gls( FIDi_sqrt ~ p_human_structure, 
                        data = data_salaya_HS_pos, 
                        correlation = corSpher(form = ~ long + lat, nugget = TRUE))
summary(model_gls_spher)

#Phutthamothon 
AICc(model_phuttha_HS_pos, glmm_date_p_HS_pos)

summary(model_phuttha_HS_pos) # p=0.8 and spatial autocorrelation somehow

# Phutthamonthon GLS somewhat jsp chu fatigué 

model_gls_exp <- gls( FIDi_sqrt ~ p_human_structure, 
                 data = data_phuttha_HS_pos, 
                 correlation = corExp(form = ~ long + lat, nugget = TRUE))
summary(model_gls_exp)

model_gls_gaus <- gls( FIDi_sqrt ~ p_human_structure, 
                      data = data_phuttha_HS_pos, 
                      correlation = corGaus(form = ~ long + lat, nugget = TRUE))
summary(model_gls_gaus)

model_gls_lin <- gls( FIDi_sqrt ~ p_human_structure, 
                       data = data_phuttha_HS_pos, 
                       correlation = corLin(form = ~ long + lat, nugget = TRUE))
summary(model_gls_lin)

model_gls_ratio <- gls( FIDi_sqrt ~ p_human_structure, 
                       data = data_phuttha_HS_pos, 
                       correlation = corRatio(form = ~ long + lat, nugget = TRUE))
summary(model_gls_ratio)

model_gls_spher <- gls( FIDi_sqrt ~ p_human_structure, 
                       data = data_phuttha_HS_pos, 
                       correlation = corSpher(form = ~ long + lat, nugget = TRUE))
summary(model_gls_spher)

# globally nope p doesn't change 


#####################################################""
################## III/ TREE#############################
#####################################################"""

boxplot(FIDi_sqrt ~ tree01, data =data_ld)  

plot(FIDi_sqrt ~ p_tree, data =data_ld)  #relatively OK we can hope for something 

hist(data_ld$FIDi_sqrt) #not so bad

hist (data_ld$p_tree) # not normal but at least relatively homogenous 

boxplot(p_tree ~ site, data = data_ld) #a bit more trees in Phuttha 

########## A) wilcoxon test keeping the zeros #############

wilcox.test(data_ld$p_tree ~ data_ld$site,alternative = "two.sided") #  effect of site on p_tree 


wilcox.test(data_ld$FIDi_sqrt ~ data_ld$tree01,alternative = "two.sided") # no effect of binary presence of tree on FID

############ B/ linear model with positive data ###############

data_T_pos = data_ld[(data_ld$p_tree>0 ) ,]
View(data_T_pos)

######## a) PRELIMINARY CORRELATION ANALYSES #############

plot(FIDi_sqrt ~ p_tree, data=data_T_pos, col="blue") 

cor(data_T_pos$FIDi_sqrt, data_T_pos$p_tree) 

cor.test(data_T_pos$FIDi_sqrt, data_T_pos$p_tree) #p = 0.0007 : c = 0.34

#########################################################
######### 1) CLASSIC LINEAR MODEL WITH BOTH SITES ###########
######################################################

####i) Model ####
model_T <- lm(FIDi_sqrt ~ p_tree, data=data_T_pos)

####ii) Conditions of validity ###### 
model_T.res <- residuals(model_T); model_T.res

# independance of residuals 
dwtest(model_T) # ok 
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_T) #OK

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_T_pos, aes(model_T.res), bins = 12)
p # OK 

shapiro.test(model_T.res) # OKKK 

# homoskedasticity
bptest(model_T) #  not ok 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_T = simulateResiduals(model_T)
testSpatialAutocorrelation(res_dharma_model_T, x =  data_T_pos$long, y = data_T_pos$lat)
#  ouch mega nope  

# temporal 


data_T_pos$day=-1

data_T_pos$day[1:5]=1
data_T_pos$day[6:15]=2
data_T_pos$day[16:20]=3
data_T_pos$day[21:22]=4
data_T_pos$day[23:27]=5
data_T_pos$day[28:29]=6
data_T_pos$day[30:33]=7
data_T_pos$day[34:38]=8
data_T_pos$day[39]=9
data_T_pos$day[40:44]=10
data_T_pos$day[45:50]=11
data_T_pos$day[51]=12
data_T_pos$day[52:54]=13
data_T_pos$day[55]=14
data_T_pos$day[56:59]=15
data_T_pos$day[60:62]=16
data_T_pos$day[63:70]=17
data_T_pos$day[71:75]=18
data_T_pos$day[76:82]=19
data_T_pos$day[83:87]=20
data_T_pos$day[88:94]=21
data_T_pos$day[95:97]=22

# aggregating residuals by time
res_dharma_model_T = recalculateResiduals(res_dharma_model_T, group = data_T_pos$day)
testTemporalAutocorrelation(res_dharma_model_T, time = unique(data_T_pos$day))
# no autocorralation on a field day scale 

####iii) Results ####
summary(model_T) # P = 0.01 ; spatial autocorrelation 

dev.off()
plot(FIDi_sqrt ~ p_tree, data=data_T_pos, col="blue") 
abline(a= model_T$coefficients[1], b= model_T$coefficients[2]) #noice 

####################################################################
###### 2) CLASSIC LINEAR MODEL WITH INTERACTION D_F_S * SITE #####
#################################################################

####i) Model ####
model_T_inter <- lm(FIDi_sqrt ~ p_tree * site, data=data_T_pos)

####ii) Conditions of validity ###### 
model_T_inter.res <- residuals(model_T_inter); model_T_inter.res

# independance of residuals 
dwtest(model_T_inter) # OK 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_T_inter) # big clusters on the left ouch 

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_T_pos, aes(model_T_inter.res), bins = 12)
p # OK 

shapiro.test(model_T_inter.res) # barely not OK  

# homoskedasticity
bptest(model_T_inter) # OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_T_inter = simulateResiduals(model_T_inter)
testSpatialAutocorrelation(res_dharma_model_T_inter, x =  data_T_pos$long, y = data_T_pos$lat)
#  no spatial autocorrelation of course 

# temporal 

# put a number for each day because the function don't like strings 


data_T_pos$day=-1

data_T_pos$day[1:5]=1
data_T_pos$day[6:15]=2
data_T_pos$day[16:20]=3
data_T_pos$day[21:22]=4
data_T_pos$day[23:27]=5
data_T_pos$day[28:29]=6
data_T_pos$day[30:33]=7
data_T_pos$day[34:38]=8
data_T_pos$day[39]=9
data_T_pos$day[40:44]=10
data_T_pos$day[45:50]=11
data_T_pos$day[51]=12
data_T_pos$day[52:54]=13
data_T_pos$day[55]=14
data_T_pos$day[56:59]=15
data_T_pos$day[60:62]=16
data_T_pos$day[63:70]=17
data_T_pos$day[71:75]=18
data_T_pos$day[76:82]=19
data_T_pos$day[83:87]=20
data_T_pos$day[88:94]=21
data_T_pos$day[95:97]=22

# aggregating residuals by time
res_dharma_model_T_inter = recalculateResiduals(res_dharma_model_T_inter, group = data_T_pos$day)
testTemporalAutocorrelation(res_dharma_model_T_inter, time = unique(data_T_pos$day))
# no autocorralation on a field day scale 

#### iii) Results ####
summary(model_T_inter)# site not significant ???? BUT FID AND INTERACTION YES 

#### iv) plot ####
data_T_pos$site <- as.factor(data_T_pos$site)
plot_model(model_T_inter, type = "pred", terms = c("p_tree", "site"), data = data_T_pos)

##################################
########    3) ANCOVA ##########
###############################

####i) Condition of validity  ####

# verify linearity between response variable and explanatory variable 
ggscatter(
  data_T_pos, x = "p_tree", y = "FIDi_sqrt",
  color = "site", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(after_stat(eq.label), ..rr.label.., sep = "~~~~"), color = site)
  )# no correlation in salaya but clearly in phutthamonthon  

#Homogeneity of regression slopes (no effect of the explenatory variable on the grouping variable (site))

data_T_pos %>% anova_test(FIDi_sqrt ~ site*p_tree) # performs type II anova between FIDi, distance from shelter, site, and the interaction d_f_s + site 
#site et inter  significant 

# Normality of residuals  

# Calculer le modèle, la covariable passe en premier
model_T_anc <- lm(FIDi_sqrt ~ p_tree + site, data = data_T_pos)

model_T.metrics <- augment(model_T_anc) %>%
  select(-.hat, -.sigma, -.fitted)

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_T_pos, aes(model_T.metrics$.resid), bins = 12)
p # OK

shapiro_test(model_T.metrics$.resid) #ok 

#Homogeneity of variances 

model_T.metrics %>% levene_test(.resid ~ site) # OK 

# outliers 

model_T.metrics %>% 
  filter(abs(.std.resid) > 3) %>% 
  as.data.frame()

#### ii) Model and Results ####

res.aov <- data_T_pos %>% anova_test(FIDi_sqrt ~ p_tree + site)

get_anova_table(res.aov) # only site significant 

#########################################################
####### 4) CLASSIC LINEAR MODEL WITH ONLY SALAYA ######
#########################################################

# take only obs in salaya
data_salaya_T_pos =  data_T_pos[(data_T_pos$site=="salaya" ) ,]
View(data_salaya_T_pos)


# square root transformation


data_salaya_T_pos$FIDi_sqrt = sqrt(data_salaya_T_pos $FIDi)


View(data_salaya_T_pos )


####i) Model ####
model_salaya_T_pos  <- lm(FIDi_sqrt ~ p_tree, data=data_salaya_T_pos )


####ii) Conditions of validity ###### 
model_salaya_T_pos.res <- residuals(model_salaya_T_pos)

# independance of residuals 
dwtest(model_salaya_T_pos) #  ok 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_salaya_T_pos ) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_salaya_T_pos = simulateResiduals(model_salaya_T_pos)
testSpatialAutocorrelation(res_dharma_salaya_T_pos, x =  data_salaya_T_pos$long, y = data_salaya_T_pos$lat)
# pas de spatial autocorrelation 

# temporal 

# put a number for each day because the function don't like strings 

data_salaya_T_pos$day=-1

data_salaya_T_pos$day[1:10]=1
data_salaya_T_pos$day[11:12]=2
data_salaya_T_pos$day[13:17]=3
data_salaya_T_pos$day[18:22]=4
data_salaya_T_pos$day[23]=5
data_salaya_T_pos$day[24]=6
data_salaya_T_pos$day[25]=7
data_salaya_T_pos$day[26:29]=8
data_salaya_T_pos$day[30:37]=9
data_salaya_T_pos$day[38:44]=10
data_salaya_T_pos$day[45:49]=11
data_salaya_T_pos$day[50:52]=12

# aggregating residuals by time
res_dharma_salaya_T_pos = recalculateResiduals(res_dharma_salaya_T_pos, group = data_salaya_T_pos$day)
testTemporalAutocorrelation(res_dharma_salaya_T_pos, time = unique(data_salaya_T_pos$day))
# no autocorralation on a field day scale 


# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_T_pos, aes(model_salaya_T_pos.res), bins = 12)
p # OK

shapiro.test(model_salaya_T_pos.res) # not OK

# homoskedasticity
bptest(model_salaya_T_pos) # OK 

####iii) Results ####
summary(model_salaya_T_pos) # not significant ; 

dev.off()
plot(FIDi_sqrt ~ p_tree, data=data_salaya_T_pos, col="blue") 
abline(a= model_salaya_T_pos$coefficients[1], b= model_salaya_T_pos$coefficients[2]) #seems correlated BUT pulled by the 6 big data points 

##################################################################
####### 4.5) CLASSIC LINEAR MODEL WITH ONLY PHUTTHAMONTHON ######
##################################################################

# take only obs in phutthamonthon 
data_phuttha_T_pos =  data_T_pos[(data_T_pos$site=="phutthamonthon" ) ,]
View(data_phuttha_T_pos)


# square root transformation


data_phuttha_T_pos$FIDi_sqrt = sqrt(data_phuttha_T_pos$FIDi)

View(data_phuttha_T_pos)


####i) Model ####
model_phuttha_T_pos <- lm(FIDi_sqrt ~ p_tree, data=data_phuttha_T_pos)


####ii) Conditions of validity ###### 
model_phuttha_T_pos.res <- residuals(model_phuttha_T_pos)

# independance of residuals 
dwtest(model_phuttha_T_pos) # INDEPENDANCE !!!!!! => OK 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_phuttha_T_pos) # okeyish

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha_T_pos, aes(model_phuttha_T_pos.res), bins = 12)
p # Okayish

shapiro.test(model_phuttha_T_pos.res) # OK 

# homoskedasticity
bptest(model_phuttha_T_pos) # homoscedaticity  

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_phuttha_T_pos = simulateResiduals(model_phuttha_T_pos)
testSpatialAutocorrelation(res_dharma_phuttha_T_pos, x =  data_phuttha_T_pos$long, y = data_phuttha_T_pos$lat)
# no spatial autocorrelation 

# temporal 

# put a number for each day because the function don't like strings 

data_phuttha_T_pos$day=-1

data_phuttha_T_pos$day[1:5]=1
data_phuttha_T_pos$day[6:10]=2
data_phuttha_T_pos$day[11:12]=3
data_phuttha_T_pos$day[13:16]=4
data_phuttha_T_pos$day[17:21]=5
data_phuttha_T_pos$day[22:27]=6
data_phuttha_T_pos$day[28:30]=7
data_phuttha_T_pos$day[31:33]=8
data_phuttha_T_pos$day[34:38]=9
data_phuttha_T_pos$day[39:43]=10
data_phuttha_T_pos$day[44:45]=11


# aggregating residuals by time
res_dharma_phuttha_T_pos = recalculateResiduals(res_dharma_phuttha_T_pos, group = data_phuttha_T_pos$day)
testTemporalAutocorrelation(res_dharma_phuttha_T_pos, time = unique(data_phuttha_T_pos$day))
# no autocorralation on a field day scale 

####iii) Results ####
summary(model_phuttha_T_pos) # p = 0.003

dev.off()
plot(FIDi_sqrt ~p_tree, data=data_phuttha_T_pos, col="blue") 
abline(a= model_phuttha_T_pos$coefficients[1], b= model_phuttha_T_pos$coefficients[2]) 


#################################################
####### 5) GLMM TAKING DATE INTO ACCOUNT  ######
################################################

####i) MODEL #####
glmm_date_T_pos = lmer(FIDi_sqrt ~ p_tree + (1|date), data=data_T_pos, REML = F)

####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_T_pos)
Fit = fitted(glmm_date_T_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # ouch 4 really patterny patterns 

# Residuals against covariates

#change the date format to %Y%M%D
data_T_pos$date<- as.Date(data_T_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
View(data_T_pos)


data_T_pos$date = as.factor(data_T_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_T_pos)
abline(h=0,lty=2)

plot (x = data_T_pos$p_tree,
      y = data_T_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_tree",
      cex = 1.2 , pch = 16) #meh lots of near zero values and triangle shape 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_T_pos, aes(Res), bins = 12)
p  #OK 

shapiro.test(Res) #OK 


# outliers 

plot(data_T_pos$p_tree) #OK 
plot(data_T_pos$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_T_pos = simulateResiduals(glmm_date_T_pos)
testSpatialAutocorrelation(res_dharma_date_T_pos, x =  data_T_pos$long, y = data_T_pos$lat)
#  big nope 

# temporal 

# put a number for each day because the function don't like strings 


data_T_pos$day=-1

data_T_pos$day[1:5]=1
data_T_pos$day[6:15]=2
data_T_pos$day[16:20]=3
data_T_pos$day[21:22]=4
data_T_pos$day[23:27]=5
data_T_pos$day[28:29]=6
data_T_pos$day[30:33]=7
data_T_pos$day[34:38]=8
data_T_pos$day[39]=9
data_T_pos$day[40:44]=10
data_T_pos$day[45:50]=11
data_T_pos$day[51]=12
data_T_pos$day[52:54]=13
data_T_pos$day[55]=14
data_T_pos$day[56:59]=15
data_T_pos$day[60:62]=16
data_T_pos$day[63:70]=17
data_T_pos$day[71:75]=18
data_T_pos$day[76:82]=19
data_T_pos$day[83:87]=20
data_T_pos$day[88:94]=21
data_T_pos$day[95:97]=22



# aggregating residuals by time
res_dharma_date_T_pos = recalculateResiduals(res_dharma_date_T_pos, group = data_T_pos$day)
testTemporalAutocorrelation(res_dharma_date_T_pos, time = unique(data_T_pos$day))
# no autocorralation on a field day scale 

#####iii) RESULTS #####

summary(glmm_date_T_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
#ICC : 0.32
r.squaredGLMM(glmm_date_T_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.02 et R2c = 0.33 so nope for correlation lol 
# NONETHELESS !!!! SITE NOT TAKEN INTO ACCOUNT  !!!! HENCE THE EFFECT OF THE DATE SO STRONG PROBABLY 


#############################################
####### 6) GLMM DATE WITH ONLY SALAYA  ######
#############################################


# take only obs in salaya
data_salaya_T_pos =  data_T_pos[(data_T_pos$site=="salaya" ) ,]
View(data_salaya_T_pos)


# square root transformation


data_salaya_T_pos$FIDi_sqrt = sqrt(data_salaya_T_pos$FIDi)


View(data_salaya_T_pos )


####i) MODEL #####
glmm_date_s_T_pos = lmer(FIDi_sqrt ~ p_tree + (1|date), data=data_salaya_T_pos, REML = F)

####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_s_T_pos)
Fit = fitted(glmm_date_s_T_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) #clusterish 

# Residuals against covariates

#change the date format to %Y%M%D
data_salaya_T_pos$date<- as.Date(data_salaya_T_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_salaya_T_pos)


data_salaya_T_pos$date = as.factor(data_salaya_T_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_salaya_T_pos)
abline(h=0,lty=2)

plot (x = data_T_pos$p_tree,
      y = data_T_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_tree",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_T_pos, aes(Res), bins = 12)
p  #OK

shapiro.test(Res) #not OK 


# outliers 

plot(data_salaya_T_pos$p_tree) #OK il y une petite valeur forte mais pas choquant
plot(data_salaya_T_pos$FIDi_sqrt) # idem 


# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_s_T_pos = simulateResiduals(glmm_date_s_T_pos)
testSpatialAutocorrelation(res_dharma_date_s_T_pos, x =  data_salaya_T_pos$long, y = data_salaya_T_pos$lat)
# no spatial autocorrelation 

# temporal 

# put a number for each day because the function don't like strings 


data_salaya_T_pos$day=-1


data_salaya_T_pos$day[1:10]=1
data_salaya_T_pos$day[11:12]=2
data_salaya_T_pos$day[13:17]=3
data_salaya_T_pos$day[18:22]=4
data_salaya_T_pos$day[23]=5
data_salaya_T_pos$day[24]=6
data_salaya_T_pos$day[25]=7
data_salaya_T_pos$day[26:29]=8
data_salaya_T_pos$day[30:37]=9
data_salaya_T_pos$day[38:44]=10
data_salaya_T_pos$day[45:49]=11
data_salaya_T_pos$day[50:52]=12



# aggregating residuals by time
res_dharma_date_s_T_pos = recalculateResiduals(res_dharma_date_s_T_pos, group = data_salaya_T_pos$day)
testTemporalAutocorrelation(res_dharma_date_s_T_pos, time = unique(data_salaya_T_pos$day))
# no autocorralation on a field day scale 

#####iii) RESULTS #####

summary(glmm_date_s_T_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.26 

r.squaredGLMM(glmm_date_s_T_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.02 et R2c = 0.28 # ouch  but predictible since no effect in salaya 

########################################################
####### 6.5) GLMM DATE WITH ONLY PHUTTHAMONTHON  ######
#######################################################


# take only obs in phutthamonthon 
data_phuttha_T_pos =  data_T_pos[(data_T_pos$site=="phutthamonthon" ) ,]
View(data_phuttha_T_pos)


# square root transformation


data_phuttha_T_pos$FIDi_sqrt = sqrt(data_phuttha_T_pos$FIDi)

View(data_phuttha_T_pos)


####i) MODEL #####
glmm_date_p_T_pos = lmer(FIDi_sqrt ~ p_tree + (1|date), data=data_phuttha_T_pos, REML = F)
# singularity  is bad !!!!!


####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_p_T_pos)
Fit = fitted(glmm_date_p_T_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_phuttha_T_pos$date<- as.Date(data_phuttha_T_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
View(data_phuttha_T_pos)


data_phuttha_T_pos$date = as.factor(data_phuttha_T_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_phuttha_T_pos)
abline(h=0,lty=2)

plot (x = data_phuttha_T_pos$p_tree,
      y = data_phuttha_T_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_tree",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha_T_pos, aes(Res), bins = 12)
p  #okayish

shapiro.test(Res) #OK 


# outliers 

plot(data_phuttha_T_pos$p_tree) #OK 
plot(data_phuttha_T_pos$FIDi_sqrt) #one big low and one big high 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_p_T_pos = simulateResiduals(glmm_date_p_T_pos)
testSpatialAutocorrelation(res_dharma_date_p_T_pos, x =  data_phuttha_T_pos$long, y = data_phuttha_T_pos$lat)
# no spatial autocorrelation 

# temporal 


# put a number for each day because the function don't like strings 


data_phuttha_T_pos$day=-1

data_phuttha_T_pos$day[1:5]=1
data_phuttha_T_pos$day[6:10]=2
data_phuttha_T_pos$day[11:12]=3
data_phuttha_T_pos$day[13:16]=4
data_phuttha_T_pos$day[17:21]=5
data_phuttha_T_pos$day[22:27]=6
data_phuttha_T_pos$day[28:30]=7
data_phuttha_T_pos$day[31:33]=8
data_phuttha_T_pos$day[34:38]=9
data_phuttha_T_pos$day[39:43]=10
data_phuttha_T_pos$day[44:45]=11



# aggregating residuals by time
res_dharma_date_p_T_pos = recalculateResiduals(res_dharma_date_p_T_pos, group = data_phuttha_T_pos$day)
testTemporalAutocorrelation(res_dharma_date_p_T_pos, time = unique(data_phuttha_T_pos$day))
# no autocorralation on a field day scale 

#####iii) RESULTS #####

summary(glmm_date_p_T_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.00 #singularity is bad 

r.squaredGLMM(glmm_date_p_T_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.2 et R2c = 0.2

##################################
#### 8) GLMM WITH SITES AS FIXED FACTOR AND DATE AS RANDOM #######
###############################


####i) MODEL #####

glmm_site_date_inter_T_pos = lmer(FIDi_sqrt ~ p_tree * site + (1|date), data=data_T_pos, REML = F)


#glmm_site_date_T_pos = lmer(FIDi_sqrt ~ p_tree + site + (1|date), data=data_T_pos, REML = F)

#AICc(glmm_site_date_T_pos,glmm_site_date_inter_T_pos) 

#AIC is smaller when accounting for the interaction so we only take the model with it from now on

####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_site_date_inter_T_pos)
Fit = fitted(glmm_site_date_inter_T_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_T_pos$date<- as.Date(data_T_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
View(data_T_pos)


data_T_pos$date = as.factor(data_T_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_T_pos)
abline(h=0,lty=2)

boxplot(Res ~ site, xlab = "site", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_T_pos)
abline(h=0,lty=2)

plot (x = data_T_pos$p_tree,
      y = data_T_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_tree",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_T_pos, aes(Res), bins = 12)
p  #okayish

shapiro.test(Res) # not OK 


# outliers 

plot(data_T_pos$p_tree) #OK 
plot(data_T_pos$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_site_date_inter_T_pos = simulateResiduals(glmm_site_date_inter_T_pos)
testSpatialAutocorrelation(res_dharma_site_date_inter_T_pos, x =  data_T_pos$long, y = data_T_pos$lat)
# no spatial autocorrelation  

# temporal 

# put a number for each day because the function don't like strings 

data_T_pos$day=-1

data_T_pos$day[1:5]=1
data_T_pos$day[6:15]=2
data_T_pos$day[16:20]=3
data_T_pos$day[21:22]=4
data_T_pos$day[23:27]=5
data_T_pos$day[28:29]=6
data_T_pos$day[30:33]=7
data_T_pos$day[34:38]=8
data_T_pos$day[39]=9
data_T_pos$day[40:44]=10
data_T_pos$day[45:50]=11
data_T_pos$day[51]=12
data_T_pos$day[52:54]=13
data_T_pos$day[55]=14
data_T_pos$day[56:59]=15
data_T_pos$day[60:62]=16
data_T_pos$day[63:70]=17
data_T_pos$day[71:75]=18
data_T_pos$day[76:82]=19
data_T_pos$day[83:87]=20
data_T_pos$day[88:94]=21
data_T_pos$day[95:97]=22


# aggregating residuals by time
res_dharma_site_date_inter_T_pos = recalculateResiduals(res_dharma_site_date_inter_T_pos, group = data_T_pos$day)
testTemporalAutocorrelation(res_dharma_site_date_inter_T_pos, time = unique(data_T_pos$day))
# no autocorrelation on a field day scale 

#####iii) RESULTS #####

summary(glmm_site_date_inter_T_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.15 
r.squaredGLMM(glmm_site_date_inter_T_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.27 et R2c = 0.39


################################################
###########MODEL COMPARISON#####################
################################################

AICc(model_T, model_T_anc, model_T_inter, glmm_date_T_pos, glmm_site_date_inter_T_pos)
min(AICc(model_T, model_T_anc, model_T_inter, glmm_date_T_pos, glmm_site_date_inter_T_pos)[,2])

# model_T_inter is the best 

summary(model_T_inter)# EFFECT of T and interaction  

#validity plot is ugly af + normality okayish 

# check salaya and phutthamonthon separatly 


#Salaya 
AICc(model_salaya_T_pos, glmm_date_s_T_pos)

summary(model_salaya_T_pos) #not significant + normality not OK 

dev.off()
plot(FIDi_sqrt ~ p_tree, data=data_salaya_T_pos, col="blue") 
abline(a= model_salaya_T_pos$coefficients[1], b= model_salaya_T_pos$coefficients[2]) #seems correlated BUT pulled by the 6 big data points 


#Phutthamothon 
AICc(model_phuttha_T_pos, glmm_date_p_T_pos)

summary(model_phuttha_T_pos) # p=0.002 ; R = 0.18 ; a= 1.67 

dev.off()
plot(FIDi_sqrt ~p_tree, data=data_phuttha_T_pos, col="blue") 
abline(a= model_phuttha_T_pos$coefficients[1], b= model_phuttha_T_pos$coefficients[2]) 

##############################################################
##########################IV/ WATER #######################
#############################################################

boxplot(FIDi_sqrt ~ water01, data =data_ld)  

plot(FIDi_sqrt ~ p_water, data =data_ld)  # not so bad, still have to see if it is actually exploitable 

hist(data_ld$FIDi_sqrt) #not so bad

hist (data_ld$p_water) # almost normal ?!?!?!

boxplot(p_water ~ site, data = data_ld)  

########## A) wilcoxon test keeping the zeros #############

wilcox.test(data_ld$p_water ~ data_ld$site, alternative = "less") # no  effect of site on p_water
 


wilcox.test(data_ld$FIDi_sqrt ~ data_ld$water01,alternative = "greater") # no effect of binary presence of water on FID

############ B/ linear model with positive data ###############

data_W_pos = data_ld[(data_ld$p_water>0 ) ,]
View(data_W_pos)

######## a) PRELIMINARY CORRELATION ANALYSES #############

plot(FIDi_sqrt ~ p_water, data=data_W_pos, col="blue") 

cor(data_W_pos$FIDi_sqrt, data_W_pos$p_water) 

cor.test(data_W_pos$FIDi_sqrt, data_W_pos$p_water) #p = 0.03 : c = -0.22 # negative correlation ?!?!?! wololoooooooo

#########################################################
######### 1) CLASSIC LINEAR MODEL WITH BOTH SITES ###########
######################################################

####i) Model ####
model_W <- lm(FIDi_sqrt ~ p_water, data=data_W_pos)

####ii) Conditions of validity ###### 
model_W.res <- residuals(model_W); model_W.res

# independance of residuals 
dwtest(model_W) # OK
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_W) #OK 

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_W_pos, aes(model_W.res), bins = 12)
p # OK 

shapiro.test(model_W.res) # OKKK 

# homoskedasticity
bptest(model_W) # ok 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_W = simulateResiduals(model_W)
testSpatialAutocorrelation(res_dharma_model_W, x =  data_W_pos$long, y = data_W_pos$lat)
#  ouch mega nope  

# temporal 


data_W_pos$day=-1

data_W_pos$day[1:5]=1
data_W_pos$day[6:16]=2
data_W_pos$day[17:21]=3
data_W_pos$day[22:23]=4
data_W_pos$day[24:28]=5
data_W_pos$day[29:30]=6
data_W_pos$day[31:34]=7
data_W_pos$day[35:36]=8
data_W_pos$day[37]=9
data_W_pos$day[38:42]=10
data_W_pos$day[43:48]=11
data_W_pos$day[49]=12
data_W_pos$day[50:52]=13
data_W_pos$day[53]=14
data_W_pos$day[54:56]=15
data_W_pos$day[57:59]=16
data_W_pos$day[60:66]=17
data_W_pos$day[67:71]=18
data_W_pos$day[72:78]=19
data_W_pos$day[79:83]=20
data_W_pos$day[84:90]=21
data_W_pos$day[91:93]=22




# aggregating residuals by time
res_dharma_model_W = recalculateResiduals(res_dharma_model_W, group = data_W_pos$day)
testTemporalAutocorrelation(res_dharma_model_W, time = unique(data_W_pos$day))
# no autocorralation on a field day scale 

####iii) Results ####
summary(model_W) # P = 0.03 ; spatial autocorrelation 

dev.off()
plot(FIDi_sqrt ~ p_water, data=data_W_pos, col="blue") 
abline(a= model_W$coefficients[1], b= model_W$coefficients[2]) #noice 

####################################################################
###### 2) CLASSIC LINEAR MODEL WITH INTERACTION D_F_S * SITE #####
#################################################################

####i) Model ####
model_W_inter <- lm(FIDi_sqrt ~ p_water * site, data=data_W_pos)

####ii) Conditions of validity ###### 
model_W_inter.res <- residuals(model_W_inter); model_W_inter.res

# independance of residuals 
dwtest(model_W_inter) # OK 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_W_inter) # 2 big clusters probably for the sites 

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_W_pos, aes(model_W_inter.res), bins = 12)
p # OK 

shapiro.test(model_W_inter.res) # not ok but graph is good 

# homoskedasticity
bptest(model_W_inter) # OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_W_inter = simulateResiduals(model_W_inter)
testSpatialAutocorrelation(res_dharma_model_W_inter, x =  data_W_pos$long, y = data_W_pos$lat)
#  no spatial autocorrelation of course 

# temporal 

# put a number for each day because the function don't like strings 


data_W_pos$day=-1

data_W_pos$day[1:5]=1
data_W_pos$day[6:16]=2
data_W_pos$day[17:21]=3
data_W_pos$day[22:23]=4
data_W_pos$day[24:28]=5
data_W_pos$day[29:30]=6
data_W_pos$day[31:34]=7
data_W_pos$day[35:36]=8
data_W_pos$day[37]=9
data_W_pos$day[38:42]=10
data_W_pos$day[43:48]=11
data_W_pos$day[49]=12
data_W_pos$day[50:52]=13
data_W_pos$day[53]=14
data_W_pos$day[54:56]=15
data_W_pos$day[57:59]=16
data_W_pos$day[60:66]=17
data_W_pos$day[67:71]=18
data_W_pos$day[72:78]=19
data_W_pos$day[79:83]=20
data_W_pos$day[84:90]=21
data_W_pos$day[91:93]=22

# aggregating residuals by time
res_dharma_model_W_inter = recalculateResiduals(res_dharma_model_W_inter, group = data_W_pos$day)
testTemporalAutocorrelation(res_dharma_model_W_inter, time = unique(data_W_pos$day))
# no autocorralation on a field day scale 

#### iii) Results ####
summary(model_W_inter)# P_water and site are significant ; no interaction for once smells good 



##################################
########    3) ANCOVA ##########
###############################

####i) Condition of validity  ####

# verify linearity between response variable and explanatory variable 
ggscatter(
  data_W_pos, x = "p_water", y = "FIDi_sqrt",
  color = "site", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(after_stat(eq.label), ..rr.label.., sep = "~~~~"), color = site)
  )# weiiiiird : we clrearly see an interaction with negative correlation in phutthamonthon but nothing in salaya 

#Homogeneity of regression slopes (no effect of the explenatory variable on the grouping variable (site))

data_W_pos %>% anova_test(FIDi_sqrt ~ site*p_water) # performs type II anova between FIDi, distance from shelter, site, and the interaction d_f_s + site 
#only site is sginficant ???

# Normality of residuals  

# Calculer le modèle, la covariable passe en premier
model_W_anc <- lm(FIDi_sqrt ~ p_water + site, data = data_W_pos)

model_W.metrics <- augment(model_W_anc) %>%
  select(-.hat, -.sigma, -.fitted)

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_W_pos, aes(model_W.metrics$.resid), bins = 12)
p # OK

shapiro_test(model_W.metrics$.resid) #not ok but graph okayish

#Homogeneity of variances 

model_W.metrics %>% levene_test(.resid ~ site) # OK 

# outliers 

model_W.metrics %>% 
  filter(abs(.std.resid) > 3) %>% 
  as.data.frame()

#### ii) Model and Results ####

res.aov <- data_W_pos %>% anova_test(FIDi_sqrt ~ p_water + site)

get_anova_table(res.aov) # only site blblblbl 


#########################################################
####### 4) CLASSIC LINEAR MODEL WITH ONLY SALAYA ######
#########################################################

# take only obs in salaya
data_salaya_W_pos =  data_W_pos[(data_W_pos$site=="salaya" ) ,]
View(data_salaya_W_pos)


# square root transformation


data_salaya_W_pos$FIDi_sqrt = sqrt(data_salaya_W_pos $FIDi)


View(data_salaya_W_pos )


####i) Model ####
model_salaya_W_pos  <- lm(FIDi_sqrt ~ p_water, data=data_salaya_W_pos )


####ii) Conditions of validity ###### 
model_salaya_W_pos.res <- residuals(model_salaya_W_pos)

# independance of residuals 
dwtest(model_salaya_W_pos) # ok 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_salaya_W_pos )

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_salaya_W_pos = simulateResiduals(model_salaya_W_pos)
testSpatialAutocorrelation(res_dharma_salaya_W_pos, x =  data_salaya_W_pos$long, y = data_salaya_W_pos$lat)
# pas de spatial autocorrelation 

# temporal 

# put a number for each day because the function don't like strings 

data_salaya_W_pos$day=-1

data_salaya_W_pos$day[1:11]=1
data_salaya_W_pos$day[12:13]=2
data_salaya_W_pos$day[14:18]=3
data_salaya_W_pos$day[19:20]=4
data_salaya_W_pos$day[21]=5
data_salaya_W_pos$day[22]=6
data_salaya_W_pos$day[23]=7
data_salaya_W_pos$day[24:26]=8
data_salaya_W_pos$day[27:33]=9
data_salaya_W_pos$day[34:40]=10
data_salaya_W_pos$day[41:45]=11
data_salaya_W_pos$day[46:48]=12



# aggregating residuals by time
res_dharma_salaya_W_pos = recalculateResiduals(res_dharma_salaya_W_pos, group = data_salaya_W_pos$day)
testTemporalAutocorrelation(res_dharma_salaya_W_pos, time = unique(data_salaya_W_pos$day))
# no autocorralation on a field day scale 


# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_W_pos, aes(model_salaya_W_pos.res), bins = 12)
p # meh

shapiro.test(model_salaya_W_pos.res) #not ok 

# homoskedasticity
bptest(model_salaya_W_pos) # OK 

####iii) Results ####
summary(model_salaya_W_pos) # not significant ; 

dev.off()
plot(FIDi_sqrt ~ p_water, data=data_salaya_W_pos, col="blue") 
abline(a= model_salaya_W_pos$coefficients[1], b= model_salaya_W_pos$coefficients[2]) #seems correlated BUT pulled by the 6 big data points 



##################################################################
####### 4.5) CLASSIC LINEAR MODEL WITH ONLY PHUTTHAMONTHON ######
##################################################################

# take only obs in phutthamonthon 
data_phuttha_W_pos =  data_W_pos[(data_W_pos$site=="phutthamonthon" ) ,]
View(data_phuttha_W_pos)


# square root transformation


data_phuttha_W_pos$FIDi_sqrt = sqrt(data_phuttha_W_pos$FIDi)

View(data_phuttha_W_pos)


####i) Model ####
model_phuttha_W_pos <- lm(FIDi_sqrt ~ p_water, data=data_phuttha_W_pos)


####ii) Conditions of validity ###### 
model_phuttha_W_pos.res <- residuals(model_phuttha_W_pos)

# independance of residuals 
dwtest(model_phuttha_W_pos) # INDEPENDANCE !!!!!! => OK 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_phuttha_W_pos) # okeyish

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha_W_pos, aes(model_phuttha_W_pos.res), bins = 12)
p # meh

shapiro.test(model_phuttha_W_pos.res) # OK 

# homoskedasticity
bptest(model_phuttha_W_pos) # homoscedaticity  

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_phuttha_W_pos = simulateResiduals(model_phuttha_W_pos)
testSpatialAutocorrelation(res_dharma_phuttha_W_pos, x =  data_phuttha_W_pos$long, y = data_phuttha_W_pos$lat)
# no spatial autocorrelation 

# temporal 

# put a number for each day because the function don't like strings 

data_phuttha_W_pos$day=-1

data_phuttha_W_pos$day[1:5]=1
data_phuttha_W_pos$day[6:10]=2
data_phuttha_W_pos$day[11:12]=3
data_phuttha_W_pos$day[13:16]=4
data_phuttha_W_pos$day[17:21]=5
data_phuttha_W_pos$day[22:27]=6
data_phuttha_W_pos$day[28:30]=7
data_phuttha_W_pos$day[31:33]=8
data_phuttha_W_pos$day[34:38]=9
data_phuttha_W_pos$day[39:43]=10
data_phuttha_W_pos$day[44:45]=11


# aggregating residuals by time
res_dharma_phuttha_W_pos = recalculateResiduals(res_dharma_phuttha_W_pos, group = data_phuttha_W_pos$day)
testTemporalAutocorrelation(res_dharma_phuttha_W_pos, time = unique(data_phuttha_W_pos$day))
# no autocorralation on a field day scale 

####iii) Results ####
summary(model_phuttha_W_pos) # p = 0.02 ; R= 0.1 ; c= -1.96 

dev.off()
plot(FIDi_sqrt ~p_water, data=data_phuttha_W_pos, col="blue") 
abline(a= model_phuttha_W_pos$coefficients[1], b= model_phuttha_W_pos$coefficients[2]) 


#################################################
####### 5) GLMM TAKING DATE INTO ACCOUNT  ######
################################################

####i) MODEL #####
glmm_date_W_pos = lmer(FIDi_sqrt ~ p_water + (1|date), data=data_W_pos, REML = F)

####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_W_pos)
Fit = fitted(glmm_date_W_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # ouch 4 really patterny patterns 

# Residuals against covariates

#change the date format to %Y%M%D
data_W_pos$date<- as.Date(data_W_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
View(data_W_pos)


data_W_pos$date = as.factor(data_W_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_W_pos)
abline(h=0,lty=2)

plot (x = data_W_pos$p_water,
      y = data_W_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_water",
      cex = 1.2 , pch = 16) #meh lots of near zero values and triangle shape 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_W_pos, aes(Res), bins = 12)
p  #OK 

shapiro.test(Res) #OK 


# outliers 

plot(data_W_pos$p_water) #OK 
plot(data_W_pos$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_W_pos = simulateResiduals(glmm_date_W_pos)
testSpatialAutocorrelation(res_dharma_date_W_pos, x =  data_W_pos$long, y = data_W_pos$lat)
#  big nope 

# temporal 

# put a number for each day because the function don't like strings 


data_W_pos$day=-1

data_W_pos$day[1:5]=1
data_W_pos$day[6:16]=2
data_W_pos$day[17:21]=3
data_W_pos$day[22:23]=4
data_W_pos$day[24:28]=5
data_W_pos$day[29:30]=6
data_W_pos$day[31:34]=7
data_W_pos$day[35:36]=8
data_W_pos$day[37]=9
data_W_pos$day[38:42]=10
data_W_pos$day[43:48]=11
data_W_pos$day[49]=12
data_W_pos$day[50:52]=13
data_W_pos$day[53]=14
data_W_pos$day[54:56]=15
data_W_pos$day[57:59]=16
data_W_pos$day[60:66]=17
data_W_pos$day[67:71]=18
data_W_pos$day[72:78]=19
data_W_pos$day[79:83]=20
data_W_pos$day[84:90]=21
data_W_pos$day[91:93]=22


# aggregating residuals by time
res_dharma_date_W_pos = recalculateResiduals(res_dharma_date_W_pos, group = data_W_pos$day)
testTemporalAutocorrelation(res_dharma_date_W_pos, time = unique(data_W_pos$day))
# no autocorralation on a field day scale 

#####iii) RESULTS #####

summary(glmm_date_W_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
#ICC : 0.34
r.squaredGLMM(glmm_date_W_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.02 et R2c = 0.36 so nope for correlation lol 
# NONETHELESS !!!! SITE NOT TAKEN INTO ACCOUNT  !!!! HENCE THE EFFECT OF THE DATE SO STRONG PROBABLY 


#############################################
####### 6) GLMM DATE WITH ONLY SALAYA  ######
#############################################


# take only obs in salaya
data_salaya_W_pos =  data_W_pos[(data_W_pos$site=="salaya" ) ,]
View(data_salaya_W_pos)


# square root transformation


data_salaya_W_pos$FIDi_sqrt = sqrt(data_salaya_W_pos$FIDi)


View(data_salaya_W_pos )


####i) MODEL #####
glmm_date_s_W_pos = lmer(FIDi_sqrt ~ p_water + (1|date), data=data_salaya_W_pos, REML = F)

####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_s_W_pos)
Fit = fitted(glmm_date_s_W_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # clustery 

# Residuals against covariates

#change the date format to %Y%M%D
data_salaya_W_pos$date<- as.Date(data_salaya_W_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_salaya_W_pos)


data_salaya_W_pos$date = as.factor(data_salaya_W_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_salaya_W_pos)
abline(h=0,lty=2)

plot (x = data_W_pos$p_water,
      y = data_W_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_water",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_W_pos, aes(Res), bins = 12)
p  #meh 

shapiro.test(Res) #OK 


# outliers 

plot(data_salaya_W_pos$p_water) #OK
plot(data_salaya_W_pos$FIDi_sqrt) #OK even if 2 maximums a bit high 


# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_s_W_pos = simulateResiduals(glmm_date_s_W_pos)
testSpatialAutocorrelation(res_dharma_date_s_W_pos, x =  data_salaya_W_pos$long, y = data_salaya_W_pos$lat)
# spatial autocorrelation 

# temporal 

# put a number for each day because the function don't like strings 



data_salaya_W_pos$day=-1

data_salaya_W_pos$day[1:11]=1
data_salaya_W_pos$day[12:13]=2
data_salaya_W_pos$day[14:18]=3
data_salaya_W_pos$day[19:20]=4
data_salaya_W_pos$day[21]=5
data_salaya_W_pos$day[22]=6
data_salaya_W_pos$day[23]=7
data_salaya_W_pos$day[24:26]=8
data_salaya_W_pos$day[27:33]=9
data_salaya_W_pos$day[34:40]=10
data_salaya_W_pos$day[41:45]=11
data_salaya_W_pos$day[46:48]=12

# aggregating residuals by time
res_dharma_date_s_W_pos = recalculateResiduals(res_dharma_date_s_W_pos, group = data_salaya_W_pos$day)
testTemporalAutocorrelation(res_dharma_date_s_W_pos, time = unique(data_salaya_W_pos$day))
# no autocorralation on a field day scale 

#####iii) RESULTS #####

summary(glmm_date_s_W_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.19 # no effect of date for some reason 

r.squaredGLMM(glmm_date_s_W_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.003 et R2c = 0.20 #rip correlation in salaya


#############################################
####### 6) GLMM DATE WITH ONLY SALAYA  ######
#############################################


# take only obs in salaya
data_salaya_W_pos =  data_W_pos[(data_W_pos$site=="salaya" ) ,]
View(data_salaya_W_pos)


# square root transformation


data_salaya_W_pos$FIDi_sqrt = sqrt(data_salaya_W_pos$FIDi)


View(data_salaya_W_pos )


####i) MODEL #####
glmm_date_s_W_pos = lmer(FIDi_sqrt ~ p_water + (1|date), data=data_salaya_W_pos, REML = F)

####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_s_W_pos)
Fit = fitted(glmm_date_s_W_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_salaya_W_pos$date<- as.Date(data_salaya_W_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
View(data_salaya_W_pos)


data_salaya_W_pos$date = as.factor(data_salaya_W_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_salaya_W_pos)
abline(h=0,lty=2)

plot (x = data_W_pos$p_water,
      y = data_W_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_water",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_W_pos, aes(Res), bins = 12)
p  #meh 

shapiro.test(Res) #OK 


# outliers 

plot(data_salaya_W_pos$p_water) #OK il y une petite valeur forte mais pas choquant
plot(data_salaya_W_pos$FIDi_sqrt) #OK 


# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_s_W_pos = simulateResiduals(glmm_date_s_W_pos)
testSpatialAutocorrelation(res_dharma_date_s_W_pos, x =  data_salaya_W_pos$long, y = data_salaya_W_pos$lat)
# spatial autocorrelation 

# temporal 

# put a number for each day because the function don't like strings 



data_salaya_W_pos$day=-1

data_salaya_W_pos$day[1:11]=1
data_salaya_W_pos$day[12:13]=2
data_salaya_W_pos$day[14:18]=3
data_salaya_W_pos$day[19:20]=4
data_salaya_W_pos$day[21]=5
data_salaya_W_pos$day[22]=6
data_salaya_W_pos$day[23]=7
data_salaya_W_pos$day[24:26]=8
data_salaya_W_pos$day[27:33]=9
data_salaya_W_pos$day[34:40]=10
data_salaya_W_pos$day[41:45]=11
data_salaya_W_pos$day[46:48]=12


# aggregating residuals by time
res_dharma_date_s_W_pos = recalculateResiduals(res_dharma_date_s_W_pos, group = data_salaya_W_pos$day)
testTemporalAutocorrelation(res_dharma_date_s_W_pos, time = unique(data_salaya_W_pos$day))
# no autocorralation on a field day scale 

#####iii) RESULTS #####

summary(glmm_date_s_W_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.18 

r.squaredGLMM(glmm_date_s_W_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.003 et R2c = 0.19


########################################################
####### 6.5) GLMM DATE WITH ONLY PHUTTHAMONTHON  ######
#######################################################


# take only obs in phutthamonthon 
data_phuttha_W_pos =  data_W_pos[(data_W_pos$site=="phutthamonthon" ) ,]
View(data_phuttha_W_pos)


# square root transformation


data_phuttha_W_pos$FIDi_sqrt = sqrt(data_phuttha_W_pos$FIDi)

View(data_phuttha_W_pos)


####i) MODEL #####
glmm_date_p_W_pos = lmer(FIDi_sqrt ~ p_water + (1|date), data=data_phuttha_W_pos, REML = F)



####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_p_W_pos)
Fit = fitted(glmm_date_p_W_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_phuttha_W_pos$date<- as.Date(data_phuttha_W_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
View(data_phuttha_W_pos)


data_phuttha_W_pos$date = as.factor(data_phuttha_W_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_phuttha_W_pos)
abline(h=0,lty=2)

plot (x = data_phuttha_W_pos$p_water,
      y = data_phuttha_W_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_water",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha_W_pos, aes(Res), bins = 12)
p  #meh

shapiro.test(Res) #OK 


# outliers 

plot(data_phuttha_W_pos$p_water) #OK 2 strong values but we'll make do 
plot(data_phuttha_W_pos$FIDi_sqrt) #OK  same

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_date_p_W_pos = simulateResiduals(glmm_date_p_W_pos)
testSpatialAutocorrelation(res_dharma_date_p_W_pos, x =  data_phuttha_W_pos$long, y = data_phuttha_W_pos$lat)
# spatial autocorrelation !!!!!!

# temporal 


# put a number for each day because the function don't like strings 

data_phuttha_W_pos$day=-1

data_phuttha_W_pos$day[1:5]=1
data_phuttha_W_pos$day[6:10]=2
data_phuttha_W_pos$day[11:12]=3
data_phuttha_W_pos$day[13:16]=4
data_phuttha_W_pos$day[17:21]=5
data_phuttha_W_pos$day[22:27]=6
data_phuttha_W_pos$day[28:30]=7
data_phuttha_W_pos$day[31:33]=8
data_phuttha_W_pos$day[34:38]=9
data_phuttha_W_pos$day[39:43]=10
data_phuttha_W_pos$day[44:45]=11


# aggregating residuals by time
res_dharma_date_p_W_pos = recalculateResiduals(res_dharma_date_p_W_pos, group = data_phuttha_W_pos$day)
testTemporalAutocorrelation(res_dharma_date_p_W_pos, time = unique(data_phuttha_W_pos$day))
# no autocorralation on a field day scale 

#####iii) RESULTS #####

summary(glmm_date_p_W_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.042 

r.squaredGLMM(glmm_date_p_W_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.12 et R2c = 0.16


##################################
#### 8) GLMM WITH SITES AS FIXED FACTOR AND DATE AS RANDOM #######
###############################


####i) MODEL #####

#glmm_site_date_inter_W_pos = lmer(FIDi_sqrt ~ p_water * site + (1|date), data=data_W_pos, REML = F)


glmm_site_date_W_pos = lmer(FIDi_sqrt ~ p_water + site + (1|date), data=data_W_pos, REML = F)

#AICc(glmm_site_date_W_pos,glmm_site_date_inter_W_pos) 

# no significant difference (delta_AICc < 2) so we take the simplest  

####ii) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_site_date_W_pos)
Fit = fitted(glmm_site_date_W_pos)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_W_pos$date<- as.Date(data_W_pos$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
View(data_W_pos)


data_W_pos$date = as.factor(data_W_pos$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_W_pos)
abline(h=0,lty=2)

boxplot(Res ~ site, xlab = "site", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_W_pos)
abline(h=0,lty=2)

plot (x = data_W_pos$p_water,
      y = data_W_pos$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "p_water",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_W_pos, aes(Res), bins = 12)
p  #ok 

shapiro.test(Res) # not OK but graph ok 


# outliers 

plot(data_W_pos$p_water) #OK 
plot(data_W_pos$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_site_date_W_pos = simulateResiduals(glmm_site_date_W_pos)
testSpatialAutocorrelation(res_dharma_site_date_W_pos, x =  data_W_pos$long, y = data_W_pos$lat)
# no spatial autocorrelation  

# temporal 

# put a number for each day because the function don't like strings 


data_W_pos$day=-1

data_W_pos$day[1:5]=1
data_W_pos$day[6:16]=2
data_W_pos$day[17:21]=3
data_W_pos$day[22:23]=4
data_W_pos$day[24:28]=5
data_W_pos$day[29:30]=6
data_W_pos$day[31:34]=7
data_W_pos$day[35:36]=8
data_W_pos$day[37]=9
data_W_pos$day[38:42]=10
data_W_pos$day[43:48]=11
data_W_pos$day[49]=12
data_W_pos$day[50:52]=13
data_W_pos$day[53]=14
data_W_pos$day[54:56]=15
data_W_pos$day[57:59]=16
data_W_pos$day[60:66]=17
data_W_pos$day[67:71]=18
data_W_pos$day[72:78]=19
data_W_pos$day[79:83]=20
data_W_pos$day[84:90]=21
data_W_pos$day[91:93]=22

# aggregating residuals by time
res_dharma_site_date_W_pos = recalculateResiduals(res_dharma_site_date_W_pos, group = data_W_pos$day)
testTemporalAutocorrelation(res_dharma_site_date_W_pos, time = unique(data_W_pos$day))
# no autocorrelation on a field day scale 

#####iii) RESULTS #####

summary(glmm_site_date_W_pos) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.13 
r.squaredGLMM(glmm_site_date_W_pos) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.26 et R2c = 0.35


################################################
###########MODEL COMPARISON#####################
################################################

AICc(model_W, model_W_anc, model_W_inter, glmm_date_W_pos, glmm_site_date_W_pos)
min(AICc(model_W, model_W_anc, model_W_inter, glmm_date_W_pos, glmm_site_date_W_pos)[,2])

# model_W_anc is the best as usual 

plot(model_W_anc)

summary(model_W_anc)# effect of p_water on FID (p = 0.052) and effect of site ; no interaction significant here 
# shapiro barely not OK but graph is good 

# check salaya and phutthamonthon separatly 


#Salaya 
AICc(model_salaya_W_pos, glmm_date_s_W_pos)

summary(model_salaya_W_pos) #not significant + normality not OK 

dev.off()
plot(FIDi_sqrt ~ p_water, data=data_salaya_W_pos, col="blue") 
abline(a= model_salaya_W_pos$coefficients[1], b= model_salaya_W_pos$coefficients[2]) #seems correlated BUT pulled by the 6 big data points 



#Phutthamothon 
AICc(model_phuttha_W_pos, glmm_date_p_W_pos)

summary(model_phuttha_W_pos) # p=0.02 ; R = 0.1 ; a = -1.96
# validity ok 

dev.off()
plot(FIDi_sqrt ~p_water, data=data_phuttha_W_pos, col="blue") 
abline(a= model_phuttha_W_pos$coefficients[1], b= model_phuttha_W_pos$coefficients[2]) 



summary(lme(fixed = FIDi_sqrt ~ p_water, random = ~ 1 | date, method = "ML", data = data_phuttha_W_pos))








