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

land_use = read.csv("C:/Users/LENOVO/Desktop/stage mahidol/stat/area_land_use.csv", header= T, sep = ";")
  
  

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

###############################################
#######1) ACP##############################
################################

##############
# ACP normee
##############

##### a) graph preliminaire ####

# Draftsman plot pour étudier les variables et leur relation
pairs.panels(data_ld[,28:31], 
             method = "pearson", # correlation method # fabou fait plutôt pearson que spearman parce que l'ACP fait du person
             hist.col = "#F10808",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)
# Ci-dessus on utilise la correlation de Spearman (basee sur le rang des valeurs) car on identifie sur les nuages de points des tendances non lineaires et/ou des points avec des valeurs relativements fortes/faibles
# on a une correlation négatif entre G et HS ; G et T ; T et W 

# on pourrait vouloir plus de corrélation mais le bouquin dit at least midly donc ça devrait pouvoir le faire 

##### b) ACP ####

ld_pca = prcomp(data_ld[,28:31],
                center = T, scale. = T)

summary(ld_pca) # on garde les CP qui permettent d'atteindre 80% cumulé 


###### c) choix et contribution des CP ####

screeplot(ld_pca, type = "lines") #critere du coude, on garde les CP avant une grosse chute de la courbe (ici entre 1 et 2 ou entre 3 et 4)

(ld_pca$sdev)^2 #critere de kaiser, on garde les CP avec varaince >1 

# avec les 3 critères on garde les 3 premières 

ld_pca$rotation[,1] #coefff de charge = cpntribution de chaque variable à la CP1 
# => importance de T > W > G > HS

ld_pca$rotation[,2] #pour CP2 
# => importance de HS > G > W > T

ld_pca$rotation[,3] #pour CP3 
# => importance de W > G > HS > T


##### d) représentation graphique ######

acp1=dudi.pca(data_ld[,28:31], scale=T, center=T, scannf = FALSE, nf = 3) # fonction pour faire une ACP normee avec ADE4. 


#cercle de correlation 

s.corcircle(acp1$co) #le long des Axe 1 et 2 ()

# Valeurs des coefficients de correlation de Pearson : 
cor(data_ld[,28:31])

s.corcircle(acp1$co, xax=1, yax=3)  # le long des axes 1 et 3 => en soit la CP3 participe presque autant que la CP 2 donc ça peut valoir le coup d'essayer de les interchanger 
#s.corcircle(acp1$co, xax=2, yax=3)  # le long des axes 2 et 3 


# representation des indvd 

acp1$li # coordonnees des individus sur chaque composante factorielle

s.label(acp1$li[,1:2], clabel=0.5) # representation sur les axes 1 et 2
# on enterprete par rapport aux variables de depart : les indvd au milieux sont moyen et l'indvd 20 plus d'herbe tandis que 11 a bcp d'eau 


s.label(acp1$li[,c(1,3)], clabel=0.5) # representation sur les axes 1 et 3 

# Coordonnees des individus sur l'axe 1 (axe des x) en fonction de leur valeur pour chaque variable (axe des y) : 
score(acp1)

# Pour l'axe 2:
par(mfrow=c(2,2))
plot(acp1$li[,2],data_ld[,1], xlab="Axe 2", ylab=colnames(data_ld[1]))
plot(acp1$li[,2],data_ld[,2], xlab="Axe 2", ylab=colnames(data_ld[2]))

######## e) extract the data for furtherr analyses ##################

data_ld_pca = data_ld

data_ld_pca$PC1 = ld_pca$x[,1]
data_ld_pca$PC2 = ld_pca$x[,2]
data_ld_pca$PC3 = ld_pca$x[,3]

###### f) graph with the new variables ######

par(mfrow = c(1,1), mar = c(5,5,1,1), cex.lab = 1)


plot(FIDi_sqrt ~ PC1, data_ld_pca,
        ylab = "FID_sqrt",
        xlab = "PC1")

plot(FIDi_sqrt ~ PC2, data_ld_pca,
     ylab = "FID_sqrt",
     xlab = "PC2")

plot(FIDi_sqrt ~ PC3, data_ld_pca,
     ylab = "FID_sqrt",
     xlab = "PC3")

#############################################
##### B/ LINEAR MODELS WITH CP1 #############
#############################################

######## PC1 PRELIMINARY CORRELATION ANALYSES #############

plot(FIDi_sqrt ~ PC1, data=data_ld_pca, col="blue") # hmmmmmmmmmmmmmmmmmmmmmmm

cor.test(data_ld_pca$FIDi_sqrt, data_ld_pca$PC1) # seems to be correlated I don't know why 
# p = 0.013 ; c = -0.25 

#########################################################
######### 1) CLASSIC LINEAR MODEL WITH BOTH SITES PC1 ###########
######################################################

####a) Model ####
model <- lm(FIDi_sqrt ~ PC1, data=data_ld_pca)

####b) Conditions of validity ###### 
model.res <- residuals(model); model.res

# independance of residuals 
dwtest(model) # OK

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
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(model.res), bins = 12)
p # OK 

shapiro.test(model.res) # OKKK 

# homoskedasticity
bptest(model) # OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model = simulateResiduals(model)
testSpatialAutocorrelation(res_dharma_model, x =  data_ld_pca$long, y = data_ld_pca$lat)
#  spatial autocorrelation which is ok because we don't takke site into account but weird because Durbin Watson was OK 

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_model, time =  data_ld_pca$fid)
# temporal autocorrelation on the scale of individual observation !!!

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_ld_pca$day=-1
data_ld_pca$day[1:5]=1
data_ld_pca$day[6:16]=2
data_ld_pca$day[17:21]=3
data_ld_pca$day[22:23]=4
data_ld_pca$day[24:28]=5
data_ld_pca$day[29:30]=6
data_ld_pca$day[31:34]=7
data_ld_pca$day[35:39]=8
data_ld_pca$day[40]=9
data_ld_pca$day[41:45]=10
data_ld_pca$day[46:51]=11
data_ld_pca$day[52]=12
data_ld_pca$day[53:55]=13
data_ld_pca$day[56]=14
data_ld_pca$day[57:60]=15
data_ld_pca$day[61:63]=16
data_ld_pca$day[64:71]=17
data_ld_pca$day[72:76]=18
data_ld_pca$day[77:83]=19
data_ld_pca$day[84:88]=20
data_ld_pca$day[89:95]=21
data_ld_pca$day[96:98]=22



# aggregating residuals by time
res_dharma_model = recalculateResiduals(res_dharma_model, group = data_ld_pca$day)
testTemporalAutocorrelation(res_dharma_model, time = unique(data_ld_pca$day))
# no autocorralation on a field day scale 

####c) Results ####
summary(model)  

# well it is significant the coeff isn't that big and adjusted R-squared = 0.05

plot(FIDi_sqrt ~ PC1, data=data_ld_pca, col="blue") # hmmmmmmmmmmmmmmmmmmmmmmm
abline(model, col="red")

####################################################################
###### 2) CLASSIC LINEAR MODEL WITH INTERACTION PC1 * SITE #####
#################################################################

####a) Model ####
model_inter <- lm(FIDi_sqrt ~ PC1 * site, data=data_ld_pca)

####b) Conditions of validity ###### 
model_inter.res <- residuals(model_inter); model_inter.res

# independance of residuals 
dwtest(model_inter) # INDEPENDANCE !!!!!! => we took account of the effect of sites 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_inter) #big cluster on the left not nice at all 

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(model_inter.res), bins = 12)
p # OK 

shapiro.test(model_inter.res) # OK 

# homoskedasticity
bptest(model_inter) # OK 

#outliers 

plot(data_ld_pca$FIDi_sqrt)

plot(data_ld_pca$PC1)

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_inter = simulateResiduals(model_inter)
testSpatialAutocorrelation(res_dharma_model_inter, x =  data_ld_pca$long, y = data_ld_pca$lat)
#   ok because we now take site into account  

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_model_inter, time =  data_ld_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!!

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_ld_pca$day=-1
data_ld_pca$day[1:5]=1
data_ld_pca$day[6:16]=2
data_ld_pca$day[17:21]=3
data_ld_pca$day[22:23]=4
data_ld_pca$day[24:28]=5
data_ld_pca$day[29:30]=6
data_ld_pca$day[31:34]=7
data_ld_pca$day[35:39]=8
data_ld_pca$day[40]=9
data_ld_pca$day[41:45]=10
data_ld_pca$day[46:51]=11
data_ld_pca$day[52]=12
data_ld_pca$day[53:55]=13
data_ld_pca$day[56]=14
data_ld_pca$day[57:60]=15
data_ld_pca$day[61:63]=16
data_ld_pca$day[64:71]=17
data_ld_pca$day[72:76]=18
data_ld_pca$day[77:83]=19
data_ld_pca$day[84:88]=20
data_ld_pca$day[89:95]=21
data_ld_pca$day[96:98]=22



# aggregating residuals by time
res_dharma_model_inter = recalculateResiduals(res_dharma_model_inter, group = data_ld_pca$day)
testTemporalAutocorrelation(res_dharma_model_inter, time = unique(data_ld_pca$day))
# no autocorralation on a field day scale 

#### c) Results ####
summary(model_inter)# everything is explanatory which is gooooooooood 

AIC(model,model_inter) #model_inter better than model tout court 


##################################
########    3) ANCOVA PC1##########
###############################

####a) Condition of validity  ####

# verify linearity between response variable and explanatory variable 
ggscatter(
  data_ld_pca, x = "PC1", y = "FIDi_sqrt",
  color = "site", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(after_stat(eq.label), ..rr.label.., sep = "~~~~"), color = site)
  )

#Homogeneity of regression slopes (no effect of the explenatory variable on the grouping variable (site))

data_ld_pca %>% anova_test(FIDi_sqrt ~ site*PC1) # performs type II anova between FIDi, PC1, site, and the PC1 + site 
# => P-value for interaction < 0.05 => not OK  
# => p-value PC1 > 0.05 => non significant whereas it was in the  classic lm (effect of the type II method wihich is more accurate in our case anyway)   

# Normality of residuals  

# Calculer le modèle, la covariable passe en premier
model_anc <- lm(FIDi_sqrt ~ PC1 + site, data = data_ld_pca)

model.metrics <- augment(model_anc) %>%
  select(-.hat, -.sigma, -.fitted)

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(model.metrics$.resid), bins = 12)
p # OK

shapiro_test(model.metrics$.resid) # not OK but I've seen worse 

#Homogeneity of variances 

model.metrics %>% levene_test(.resid ~ site) # OK 

# outliers #the 3 is general rule of thumb  

model.metrics %>% 
  filter(abs(.std.resid) > 3) %>% 
  as.data.frame() #there is one but it's not that serious IG 

#### b) Model and Results ####

res.aov <- data_ld_pca %>% anova_test(FIDi_sqrt ~ PC1 + site)

get_anova_table(res.aov) #PC1 no significant with ANCOVA 


#########################################################
####### 4) CLASSIC LINEAR MODEL WITH ONLY SALAYA PC1 ######
#########################################################

# take only obs in salaya
data_salaya_pca =  data_ld_pca[(data_ld_pca$site=="salaya" ) ,]
View(data_salaya_pca)


# square root transformation


data_salaya_pca$FIDi_sqrt = sqrt(data_salaya_pca$FIDi)

View(data_salaya_pca)


####a) Model ####
model_salaya_pc1 <- lm(FIDi_sqrt ~ PC1, data=data_salaya_pca)

####b) Conditions of validity ###### 
model_salaya_pc1.res <- residuals(model_salaya_pc1)

# independance of residuals 
dwtest(model_salaya_pc1) # OK

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_salaya_pc1)


# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_pca, aes(model_salaya_pc1.res), bins = 12)
p # not OK oulala

shapiro.test(model_salaya_pc1.res) # not OK 

# homoskedasticity
bptest(model_salaya_pc1) # OK  

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_salaya_pc1 = simulateResiduals(model_salaya_pc1)
testSpatialAutocorrelation(res_dharma_salaya_pc1, x =  data_salaya_pca$long, y = data_salaya_pca$lat)
# no spatial autocorrelation 

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_salaya_pc1, time =  data_salaya_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_salaya_pca$day=-1

data_salaya_pca$day[1:11]=1
data_salaya_pca$day[12:13]=2
data_salaya_pca$day[14:18]=3
data_salaya_pca$day[19:23]=4
data_salaya_pca$day[24]=5
data_salaya_pca$day[25]=6
data_salaya_pca$day[26]=7
data_salaya_pca$day[27:30]=8
data_salaya_pca$day[31:38]=9
data_salaya_pca$day[39:45]=10
data_salaya_pca$day[46:50]=11
data_salaya_pca$day[51:53]=12

# aggregating residuals by time
res_dharma_salaya_pc1 = recalculateResiduals(res_dharma_salaya_pc1, group = data_salaya_pca$day)
testTemporalAutocorrelation(res_dharma_salaya_pc1, time = unique(data_salaya_pca$day))
# no autocorralation on a field day scale 


####c) Results ####
summary(model_salaya_pc1) # nope  (P = 0.57) 

plot(FIDi_sqrt ~ PC1, data=data_salaya, col="blue") 
abline(a= model_salaya$coefficients[1], b= model_salaya$coefficients[2]) #seems correlated BUT pulled by the 6 big data points 

##################################################################
####### 4.5) CLASSIC LINEAR MODEL WITH ONLY PHUTTHAMONTHON PC1######
##################################################################

# take only obs in phutthamonthon 
data_phuttha_pca =  data_ld_pca[(data_ld_pca$site=="phutthamonthon" ) ,]
View(data_phuttha_pca)


# square root transformation


data_phuttha_pca$FIDi_sqrt = sqrt(data_phuttha_pca$FIDi)

View(data_phuttha_pca)


####a) Model ####
model_phuttha_pc1 <- lm(FIDi_sqrt ~ PC1, data=data_phuttha_pca)

####b) Conditions of validity ###### 
model_phuttha_pc1.res <- residuals(model_phuttha_pc1)

# independance of residuals 
dwtest(model_phuttha_pc1) # OK

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_phuttha_pc1) #looks OK 


# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha_pca, aes(model_phuttha_pc1.res), bins = 12)
p # not OK oulala

shapiro.test(model_phuttha_pc1.res) # OK ?  

# homoskedasticity
bptest(model_phuttha_pc1) # OK  

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_phuttha_pc1 = simulateResiduals(model_phuttha_pc1)
testSpatialAutocorrelation(res_dharma_phuttha_pc1, x =  data_phuttha_pca$long, y = data_phuttha_pca$lat)
# no spatial autocorrelation 

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_phuttha_pc1, time =  data_phuttha_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_phuttha_pca$day=-1

data_phuttha_pca$day[1:5]=1
data_phuttha_pca$day[6:10]=2
data_phuttha_pca$day[11:12]=3
data_phuttha_pca$day[13:16]=4
data_phuttha_pca$day[17:21]=5
data_phuttha_pca$day[22:27]=6
data_phuttha_pca$day[28:30]=7
data_phuttha_pca$day[31:33]=8
data_phuttha_pca$day[34:38]=9
data_phuttha_pca$day[39:43]=10
data_phuttha_pca$day[44:45]=11


# aggregating residuals by time
res_dharma_phuttha_pc1 = recalculateResiduals(res_dharma_phuttha_pc1, group = data_phuttha_pca$day)
testTemporalAutocorrelation(res_dharma_phuttha_pc1, time = unique(data_phuttha_pca$day))
# no autocorralation on a field day scale 

####c) Results ####
summary(model_phuttha_pc1) # significant  (P = 0.0006135) + adjusted R-squared = 0.22

plot(FIDi_sqrt ~ PC1, data=data_phuttha_pca, col="blue") 
abline(a= model_phuttha_pc1$coefficients[1], b= model_phuttha_pc1$coefficients[2]) #nicely correlated :)  



#################################################
####### 5) GLMM TAKING DATE INTO ACCOUNT  PC1######
################################################

####a) MODEL #####
glmm_date_pc1 = lmer(FIDi_sqrt ~ PC1 + (1|date), data=data_ld_pca, REML = F)


####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_pc1)
Fit = fitted(glmm_date_pc1)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_ld_pca$date<- as.Date(data_ld_pca$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
View(data_ld_pca)


data_ld_pca$date = as.factor(data_ld_pca$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_ld_pca)
abline(h=0,lty=2)

plot (x = data_ld_pca$PC1,
      y = data_ld_pca$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "PC1",
      cex = 1.2 , pch = 16) # so so but not  horrible  

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(Res), bins = 12)
p  

shapiro.test(Res) #OK 


# outliers 

plot(data_ld_pca$PC1) #OK 
plot(data_ld_pca$FIDi_sqrt) #OK 

#DHARMA test for autocorrelation ! 

# spatial 
res_dharma_glmm_date_pc1 = simulateResiduals(glmm_date_pc1)
testSpatialAutocorrelation(res_dharma_glmm_date_pc1, x =  data_ld_pca$long, y = data_ld_pca$lat)
# spatial autocorrelation  : we don't take site into account yet

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_glmm_date_pc1, time =  data_ld_pca$fid)
# temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 


data_ld_pca$day=-1
data_ld_pca$day[1:5]=1
data_ld_pca$day[6:16]=2
data_ld_pca$day[17:21]=3
data_ld_pca$day[22:23]=4
data_ld_pca$day[24:28]=5
data_ld_pca$day[29:30]=6
data_ld_pca$day[31:34]=7
data_ld_pca$day[35:39]=8
data_ld_pca$day[40]=9
data_ld_pca$day[41:45]=10
data_ld_pca$day[46:51]=11
data_ld_pca$day[52]=12
data_ld_pca$day[53:55]=13
data_ld_pca$day[56]=14
data_ld_pca$day[57:60]=15
data_ld_pca$day[61:63]=16
data_ld_pca$day[64:71]=17
data_ld_pca$day[72:76]=18
data_ld_pca$day[77:83]=19
data_ld_pca$day[84:88]=20
data_ld_pca$day[89:95]=21
data_ld_pca$day[96:98]=22


# aggregating residuals by time
res_dharma_glmm_date_pc1 = recalculateResiduals(res_dharma_glmm_date_pc1, group = data_ld_pca$day)
testTemporalAutocorrelation(res_dharma_glmm_date_pc1, time = unique(data_ld_pca$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_date_pc1) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
#ICC = 0.33
r.squaredGLMM(glmm_date_pc1) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.026 et R2c = 0.34 so nope for correlation lol 
# NONETHELESS !!!! SITE NOT TAKEN INTO ACCOUNT  !!!! HENCE THE EFFECT OF THE DATE SO STRONG PROBABLY 


#############################################
####### 6) GLMM DATE WITH ONLY SALAYA  PC1######
#############################################



# take only obs in salaya
data_salaya_pca =  data_ld_pca[(data_ld_pca$site=="salaya" ) ,]
View(data_salaya_pca)


# square root transformation


data_salaya_pca$FIDi_sqrt = sqrt(data_salaya_pca$FIDi)


####a) MODEL #####
glmm_date_s_pc1 = lmer(FIDi_sqrt ~ PC1 + (1|date), data=data_salaya_pca, REML = F)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_s_pc1)
Fit = fitted(glmm_date_s_pc1)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_salaya_pca$date<- as.Date(data_salaya_pca$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_salaya_pca)


data_salaya_pca$date = as.factor(data_salaya_pca$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_salaya_pca)
abline(h=0,lty=2)

plot (x = data_salaya_pca$PC1,
      y = data_salaya_pca$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "PC1",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_pca, aes(Res), bins = 12)
p  

shapiro.test(Res) #OK 


# outliers 

plot(data_salaya_pca$PC1) #OK il y une petite valeur forte mais pas choquant
plot(data_salaya_pca$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_glmm_date_s_pc1= simulateResiduals(glmm_date_s_pc1)
testSpatialAutocorrelation(res_dharma_glmm_date_s_pc1, x =  data_salaya_pca$long, y = data_salaya_pca$lat)
# no spatial autocorrelation 

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_glmm_date_s_pc1, time =  data_salaya_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_salaya_pca$day=-1

data_salaya_pca$day[1:11]=1
data_salaya_pca$day[12:13]=2
data_salaya_pca$day[14:18]=3
data_salaya_pca$day[19:23]=4
data_salaya_pca$day[24]=5
data_salaya_pca$day[25]=6
data_salaya_pca$day[26]=7
data_salaya_pca$day[27:30]=8
data_salaya_pca$day[31:38]=9
data_salaya_pca$day[39:45]=10
data_salaya_pca$day[46:50]=11
data_salaya_pca$day[51:53]=12

# aggregating residuals by time
res_dharma_glmm_date_s_pc1 = recalculateResiduals(res_dharma_glmm_date_s_pc1, group = data_salaya_pca$day)
testTemporalAutocorrelation(res_dharma_glmm_date_s_pc1, time = unique(data_salaya_pca$day))
# no autocorralation on a field day scale 


#####c) RESULTS #####

summary(glmm_date_s_pc1) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.23  

r.squaredGLMM(glmm_date_s_pc1) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.014 et R2c = 0.24 so nope for correlation lol 
# Now weird why so much importance of date though 



#############################################
####### 7) GLMM DATE WITH ONLY PHUTTHAMONTHON  PC1######
#############################################


# take only obs in phutthamonthon
data_phuttha_pca =  data_ld_pca[(data_ld_pca$site=="phutthamonthon" ) ,]
View(data_phuttha_pca)


# square root transformation


data_phuttha_pca$FIDi_sqrt = sqrt(data_phuttha_pca$FIDi)


####a) MODEL #####
glmm_date_p_pc1 = lmer(FIDi_sqrt ~ PC1 + (1|date), data=data_phuttha_pca, REML = F)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_p_pc1)
Fit = fitted(glmm_date_p_pc1)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_phuttha_pca$date<- as.Date(data_phuttha_pca$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_salaya_pca)


data_phuttha_pca$date = as.factor(data_phuttha_pca$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_phuttha_pca)
abline(h=0,lty=2)

plot (x = data_phuttha_pca$PC1,
      y = data_phuttha_pca$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "PC1",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha_pca, aes(Res), bins = 12)
p  #not that good 

shapiro.test(Res) #OK ? 


# outliers 

plot(data_phuttha_pca$PC1) #OK il y une petite valeur forte mais pas choquant
plot(data_phuttha_pca$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_glmm_p_pc1 = simulateResiduals(glmm_date_p_pc1)
testSpatialAutocorrelation(res_dharma_glmm_p_pc1, x =  data_phuttha_pca$long, y = data_phuttha_pca$lat)
# no spatial autocorrelation 

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_glmm_p_pc1, time =  data_phuttha_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_phuttha_pca$day=-1

data_phuttha_pca$day[1:5]=1
data_phuttha_pca$day[6:10]=2
data_phuttha_pca$day[11:12]=3
data_phuttha_pca$day[13:16]=4
data_phuttha_pca$day[17:21]=5
data_phuttha_pca$day[22:27]=6
data_phuttha_pca$day[28:30]=7
data_phuttha_pca$day[31:33]=8
data_phuttha_pca$day[34:38]=9
data_phuttha_pca$day[39:43]=10
data_phuttha_pca$day[44:45]=11


# aggregating residuals by time
res_dharma_glmm_p_pc1 = recalculateResiduals(res_dharma_glmm_p_pc1, group = data_phuttha_pca$day)
testTemporalAutocorrelation(res_dharma_glmm_p_pc1, time = unique(data_phuttha_pca$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_date_p_pc1) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0 #curious 

r.squaredGLMM(glmm_date_p_pc1) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.24 et R2c = 0.25 

#can't manage to du lme to have the p-value but non mixed version as best AIC anyway 



##################################
#### 8) GLMM WITH SITES AS FIXED FACTOR AND DATE AS RANDOM PC1#######
###############################



####a) MODEL #####

glmm_site_date_inter_pc1 = lmer(FIDi_sqrt ~ PC1 * site + (1|date), data=data_ld_pca, REML = F)

#glmm_site_date_pc1 = lmer(FIDi_sqrt ~ PC1 + site + (1|date), data=data_ld_pca, REML = F)

#AIC(glmm_site_date_pc1,glmm_site_date_inter_pc1) 

# AIC is smaller when accounting for the interaction so we only take this model from now on

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_site_date_inter_pc1)
Fit = fitted(glmm_site_date_inter_pc1)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_ld_pca$date<- as.Date(data_ld_pca$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_monitor)


data_ld_pca$date = as.factor(data_ld_pca$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_ld_pca)
abline(h=0,lty=2)

boxplot(Res ~ site, xlab = "site", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_ld_pca)
abline(h=0,lty=2)

plot (x = data_ld_pca$PC1,
      y = data_ld_pca$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "PC1",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(Res), bins = 12)
p  

shapiro.test(Res) #OK


# outliers 

plot(data_ld_pca$PC1) #OK 
plot(data_ld_pca$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_site_date_inter_pc1 = simulateResiduals(glmm_site_date_inter_pc1)
testSpatialAutocorrelation(res_dharma_site_date_inter_pc1, x =  data_ld_pca$long, y = data_ld_pca$lat)
# no spatial autocorrelation  

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_site_date_inter_pc1, time =  data_ld_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 


data_ld_pca$day=-1

data_ld_pca$day[1:5]=1
data_ld_pca$day[6:16]=2
data_ld_pca$day[17:21]=3
data_ld_pca$day[22:23]=4
data_ld_pca$day[24:28]=5
data_ld_pca$day[29:30]=6
data_ld_pca$day[31:34]=7
data_ld_pca$day[35:39]=8
data_ld_pca$day[40]=9
data_ld_pca$day[41:45]=10
data_ld_pca$day[46:51]=11
data_ld_pca$day[52]=12
data_ld_pca$day[53:55]=13
data_ld_pca$day[56]=14
data_ld_pca$day[57:60]=15
data_ld_pca$day[61:63]=16
data_ld_pca$day[64:71]=17
data_ld_pca$day[72:76]=18
data_ld_pca$day[77:83]=19
data_ld_pca$day[84:88]=20
data_ld_pca$day[89:95]=21
data_ld_pca$day[96:98]=22

# aggregating residuals by time
res_dharma_site_date_inter_pc1 = recalculateResiduals(res_dharma_site_date_inter_pc1, group = data_ld_pca$day)
testTemporalAutocorrelation(res_dharma_site_date_inter_pc1, time = unique(data_ld_pca$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_site_date_inter_pc1) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.16
r.squaredGLMM(glmm_site_date_inter_pc1) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.29 et R2c = 0.4 



################################################
###########MODEL COMPARISON PC1 #####################
################################################

AIC(glmm_date_pc1, glmm_site_date_inter_pc1, model, model_anc, model_inter)
min(AIC(glmm_date_pc1, glmm_site_date_inter_pc1, model, model_anc, model_inter)[,2])

# lowest AIC is glmm_site_date_inter_pc1 

#using lme function to get the p-values conveniently 
glmm_site_date_inter_pc1_lme = lme(fixed = FIDi_sqrt ~ PC1 * site , random= ~ 1 |date, data=data_ld_pca, method = "ML")
summary(glmm_site_date_inter_pc1_lme)

# everything is significant : site, PC1 and PC1:site



#############################################
##### C/ LINEAR MODELS WITH CP2 #############
#############################################

######## PRELIMINARY CORRELATION ANALYSES PC2#############

plot(FIDi_sqrt ~ PC2, data=data_ld_pca, col="blue") # hmmmmmmmmmmmmmmmmmmmmmmm

cor.test(data_ld_pca$FIDi_sqrt, data_ld_pca$PC2) # doesn't seem to be correlated 
# p = 0.07 ; c = 0.18

#########################################################
######### 1) CLASSIC LINEAR MODEL WITH BOTH SITES PC2###########
######################################################

####a) Model ####
model_pc2 <- lm(FIDi_sqrt ~ PC2, data=data_ld_pca)

####b) Conditions of validity ###### 
model.res_pc2 <- residuals(model_pc2); model.res_pc2

# independance of residuals 
dwtest(model_pc2) # not OK

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_pc2) #not really good 

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(model.res_pc2), bins = 12)
p # OK 

shapiro.test(model.res_pc2) # OKKK 

# homoskedasticity
bptest(model_pc2) # OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_pc2 = simulateResiduals(model_pc2)
testSpatialAutocorrelation(res_dharma_model_pc2, x =  data_ld_pca$long, y = data_ld_pca$lat)
#  spatial autocorrelation which is ok because we don't takke site into account  

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_model_pc2, time =  data_ld_pca$fid)
# temporal autocorrelation on the scale of individual observation !!!

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_ld_pca$day=-1
data_ld_pca$day[1:5]=1
data_ld_pca$day[6:16]=2
data_ld_pca$day[17:21]=3
data_ld_pca$day[22:23]=4
data_ld_pca$day[24:28]=5
data_ld_pca$day[29:30]=6
data_ld_pca$day[31:34]=7
data_ld_pca$day[35:39]=8
data_ld_pca$day[40]=9
data_ld_pca$day[41:45]=10
data_ld_pca$day[46:51]=11
data_ld_pca$day[52]=12
data_ld_pca$day[53:55]=13
data_ld_pca$day[56]=14
data_ld_pca$day[57:60]=15
data_ld_pca$day[61:63]=16
data_ld_pca$day[64:71]=17
data_ld_pca$day[72:76]=18
data_ld_pca$day[77:83]=19
data_ld_pca$day[84:88]=20
data_ld_pca$day[89:95]=21
data_ld_pca$day[96:98]=22



# aggregating residuals by time
res_dharma_model_pc2 = recalculateResiduals(res_dharma_model_pc2, group = data_ld_pca$day)
testTemporalAutocorrelation(res_dharma_model_pc2, time = unique(data_ld_pca$day))
# no autocorralation on a field day scale 

####c) Results ####
summary(model_pc2) # almost significant (P = 0.08) but adjusted R-squared = 0.02 + unreliable because doesn't account for site and autocorrelation and all 


plot(FIDi_sqrt ~ PC2, data=data_ld_pca, col="blue") # hmmmmmmmmmmmmmmmmmmmmmmm
abline(model_pc2, col="red")


####################################################################
###### 2) CLASSIC LINEAR MODEL WITH INTERACTION PC2 * SITE #####
#################################################################

####a) Model ####
model_inter_pc2 <- lm(FIDi_sqrt ~ PC2 * site, data=data_ld_pca)

####b) Conditions of validity ###### 
model_inter.res_pc2 <- residuals(model_inter_pc2); model_inter.res_pc2

# independance of residuals 
dwtest(model_inter_pc2) # INDEPENDANCE !!!!!! => we took account of the effect of sites 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_inter_pc2) #2 big clusters because sites u know

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(model_inter.res_pc2), bins = 12)
p # OK 

shapiro.test(model_inter.res_pc2) # OK 

# homoskedasticity
bptest(model_inter_pc2) # OK 

#outliers 

plot(data_ld_pca$FIDi_sqrt)

plot(data_ld_pca$PC2)

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_inter_pc2 = simulateResiduals(model_inter_pc2)
testSpatialAutocorrelation(res_dharma_model_inter_pc2, x =  data_ld_pca$long, y = data_ld_pca$lat)
#   ok because we now take site into account  

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_model_inter_pc2, time =  data_ld_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!!

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_ld_pca$day=-1
data_ld_pca$day[1:5]=1
data_ld_pca$day[6:16]=2
data_ld_pca$day[17:21]=3
data_ld_pca$day[22:23]=4
data_ld_pca$day[24:28]=5
data_ld_pca$day[29:30]=6
data_ld_pca$day[31:34]=7
data_ld_pca$day[35:39]=8
data_ld_pca$day[40]=9
data_ld_pca$day[41:45]=10
data_ld_pca$day[46:51]=11
data_ld_pca$day[52]=12
data_ld_pca$day[53:55]=13
data_ld_pca$day[56]=14
data_ld_pca$day[57:60]=15
data_ld_pca$day[61:63]=16
data_ld_pca$day[64:71]=17
data_ld_pca$day[72:76]=18
data_ld_pca$day[77:83]=19
data_ld_pca$day[84:88]=20
data_ld_pca$day[89:95]=21
data_ld_pca$day[96:98]=22



# aggregating residuals by time
res_dharma_model_inter_pc2 = recalculateResiduals(res_dharma_model_inter_pc2, group = data_ld_pca$day)
testTemporalAutocorrelation(res_dharma_model_inter_pc2, time = unique(data_ld_pca$day))
# no autocorralation on a field day scale 

#### c) Results ####
summary(model_inter_pc2)# everything is explanatory which is gooooooooood 

AIC(model_pc2,model_inter_pc2) #model_inter better than model tout court 



##################################
########    3) ANCOVA PC2##########
###############################

####a) Condition of validity  ####

# verify linearity between response variable and explanatory variable 
ggscatter(
  data_ld_pca, x = "PC2", y = "FIDi_sqrt",
  color = "site", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(after_stat(eq.label), ..rr.label.., sep = "~~~~"), color = site)
  ) # houlala that'a mess lol 
# stong negative effect in phutthamonthon and negligible positive effect in salaya 


#Homogeneity of regression slopes (no effect of the explenatory variable on the grouping variable (site))

data_ld_pca %>% anova_test(FIDi_sqrt ~ site*PC2) # performs type II anova between FIDi, PC2, site, and the PC2 + site 
# PC2 doesn't seem to be significant but there is an interraction 

# Normality of residuals  

# Calculer le modèle, la covariable passe en premier
model_anc_pc2 <- lm(FIDi_sqrt ~ PC2 + site, data = data_ld_pca)

model.metrics <- augment(model_anc_pc2) %>%
  select(-.hat, -.sigma, -.fitted)

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(model.metrics$.resid), bins = 12)
p # OK

shapiro_test(model.metrics$.resid) # not OK 

#Homogeneity of variances 

model.metrics %>% levene_test(.resid ~ site) # OK 

# outliers #the 3 is general rule of thumb  

model.metrics %>% 
  filter(abs(.std.resid) > 3) %>% 
  as.data.frame() #there is 2 but it's not that serious IG 

#### b) Model and Results ####

res.aov <- data_ld_pca %>% anova_test(FIDi_sqrt ~ PC2 + site)

get_anova_table(res.aov) #PC2 no significant with ANCOVA 



#########################################################
####### 4) CLASSIC LINEAR MODEL WITH ONLY SALAYA PC2######
#########################################################

# take only obs in salaya
data_salaya_pca =  data_ld_pca[(data_ld_pca$site=="salaya" ) ,]
View(data_salaya_pca)


# square root transformation


data_salaya_pca$FIDi_sqrt = sqrt(data_salaya_pca$FIDi)

View(data_salaya_pca)


####a) Model ####
model_salaya_pc2 <- lm(FIDi_sqrt ~ PC2, data=data_salaya_pca)

####b) Conditions of validity ###### 
model_salaya_pc2.res <- residuals(model_salaya_pc2)

# independance of residuals 
dwtest(model_salaya_pc2) # OK

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_salaya_pc2) #i've seen worse


# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_pca, aes(model_salaya_pc2.res), bins = 12)
p # OK

shapiro.test(model_salaya_pc2.res) # OK 

# homoskedasticity
bptest(model_salaya_pc2) # OK  

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_salaya_pc2 = simulateResiduals(model_salaya_pc2)
testSpatialAutocorrelation(res_dharma_salaya_pc2, x =  data_salaya_pca$long, y = data_salaya_pca$lat)
# no spatial autocorrelation 

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_salaya_pc2, time =  data_salaya_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_salaya_pca$day=-1

data_salaya_pca$day[1:11]=1
data_salaya_pca$day[12:13]=2
data_salaya_pca$day[14:18]=3
data_salaya_pca$day[19:23]=4
data_salaya_pca$day[24]=5
data_salaya_pca$day[25]=6
data_salaya_pca$day[26]=7
data_salaya_pca$day[27:30]=8
data_salaya_pca$day[31:38]=9
data_salaya_pca$day[39:45]=10
data_salaya_pca$day[46:50]=11
data_salaya_pca$day[51:53]=12

# aggregating residuals by time
res_dharma_salaya_pc2 = recalculateResiduals(res_dharma_salaya_pc2, group = data_salaya_pca$day)
testTemporalAutocorrelation(res_dharma_salaya_pc2, time = unique(data_salaya_pca$day))
# no autocorralation on a field day scale 


####c) Results ####
summary(model_salaya_pc2) # nope  (P = 0.12) 

plot(FIDi_sqrt ~ PC2, data=data_salaya_pca, col="blue") 
abline(a= model_salaya_pc2$coefficients[1], b= model_salaya_pc2$coefficients[2]) 


##################################################################
####### 4.5) CLASSIC LINEAR MODEL WITH ONLY PHUTTHAMONTHON PC2######
##################################################################

# take only obs in phutthamonthon 
data_phuttha_pca =  data_ld_pca[(data_ld_pca$site=="phutthamonthon" ) ,]
View(data_phuttha_pca)


# square root transformation


data_phuttha_pca$FIDi_sqrt = sqrt(data_phuttha_pca$FIDi)

View(data_phuttha_pca)


####a) Model ####
model_phuttha_pc2 <- lm(FIDi_sqrt ~ PC2, data=data_phuttha_pca)

####b) Conditions of validity ###### 
model_phuttha_pc2.res <- residuals(model_phuttha_pc2)

# independance of residuals 
dwtest(model_phuttha_pc2) # OK

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_phuttha_pc2) #looks so so with big cluster 


# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha_pca, aes(model_phuttha_pc2.res), bins = 12)
p # not OK oulala

shapiro.test(model_phuttha_pc2.res) # OK ?  

# homoskedasticity
bptest(model_phuttha_pc2) # OK  

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_phuttha_pc2 = simulateResiduals(model_phuttha_pc2)
testSpatialAutocorrelation(res_dharma_phuttha_pc2, x =  data_phuttha_pca$long, y = data_phuttha_pca$lat)
# no spatial autocorrelation 

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_phuttha_pc2, time =  data_phuttha_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_phuttha_pca$day=-1

data_phuttha_pca$day[1:5]=1
data_phuttha_pca$day[6:10]=2
data_phuttha_pca$day[11:12]=3
data_phuttha_pca$day[13:16]=4
data_phuttha_pca$day[17:21]=5
data_phuttha_pca$day[22:27]=6
data_phuttha_pca$day[28:30]=7
data_phuttha_pca$day[31:33]=8
data_phuttha_pca$day[34:38]=9
data_phuttha_pca$day[39:43]=10
data_phuttha_pca$day[44:45]=11


# aggregating residuals by time
res_dharma_phuttha_pc2 = recalculateResiduals(res_dharma_phuttha_pc2, group = data_phuttha_pca$day)
testTemporalAutocorrelation(res_dharma_phuttha_pc2, time = unique(data_phuttha_pca$day))
# no autocorralation on a field day scale 

####c) Results ####
summary(model_phuttha_pc2) # significant  (P =  0.0012) + adjusted R-squared = 0.2
# c= -0.5
plot(FIDi_sqrt ~ PC2, data=data_phuttha_pca, col="blue") 
abline(a= model_phuttha_pc2$coefficients[1], b= model_phuttha_pc2$coefficients[2]) #nicely anti-correlated :)  


#################################################
####### 5) GLMM TAKING DATE INTO ACCOUNT  PC2######
################################################

####a) MODEL #####
glmm_date_pc2 = lmer(FIDi_sqrt ~ PC2 + (1|date), data=data_ld_pca, REML = F)


####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_pc2)
Fit = fitted(glmm_date_pc2)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_ld_pca$date<- as.Date(data_ld_pca$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
View(data_ld_pca)


data_ld_pca$date = as.factor(data_ld_pca$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_ld_pca)
abline(h=0,lty=2)

plot (x = data_ld_pca$PC2,
      y = data_ld_pca$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "PC2",
      cex = 1.2 , pch = 16) #the vertical line doesn't look good  

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(Res), bins = 12)
p  

shapiro.test(Res) #OK 


# outliers 

plot(data_ld_pca$PC2) #horizontal line yuck yuck 
plot(data_ld_pca$FIDi_sqrt) #OK 

#DHARMA test for autocorrelation ! 

# spatial 
res_dharma_glmm_date_pc2 = simulateResiduals(glmm_date_pc2)
testSpatialAutocorrelation(res_dharma_glmm_date_pc2, x =  data_ld_pca$long, y = data_ld_pca$lat)
# spatial autocorrelation  : we don't take site into account yet

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_glmm_date_pc2, time =  data_ld_pca$fid)
# temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 


data_ld_pca$day=-1
data_ld_pca$day[1:5]=1
data_ld_pca$day[6:16]=2
data_ld_pca$day[17:21]=3
data_ld_pca$day[22:23]=4
data_ld_pca$day[24:28]=5
data_ld_pca$day[29:30]=6
data_ld_pca$day[31:34]=7
data_ld_pca$day[35:39]=8
data_ld_pca$day[40]=9
data_ld_pca$day[41:45]=10
data_ld_pca$day[46:51]=11
data_ld_pca$day[52]=12
data_ld_pca$day[53:55]=13
data_ld_pca$day[56]=14
data_ld_pca$day[57:60]=15
data_ld_pca$day[61:63]=16
data_ld_pca$day[64:71]=17
data_ld_pca$day[72:76]=18
data_ld_pca$day[77:83]=19
data_ld_pca$day[84:88]=20
data_ld_pca$day[89:95]=21
data_ld_pca$day[96:98]=22


# aggregating residuals by time
res_dharma_glmm_date_pc2 = recalculateResiduals(res_dharma_glmm_date_pc2, group = data_ld_pca$day)
testTemporalAutocorrelation(res_dharma_glmm_date_pc2, time = unique(data_ld_pca$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_date_pc2) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
#ICC = 0.35
r.squaredGLMM(glmm_date_pc2) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.0004 et R2c = 0.35 so nope for correlation lol 
# NONETHELESS !!!! SITE NOT TAKEN INTO ACCOUNT  !!!!

#############################################
####### 6) GLMM DATE WITH ONLY SALAYA  PC2######
#############################################



# take only obs in salaya
data_salaya_pca =  data_ld_pca[(data_ld_pca$site=="salaya" ) ,]
View(data_salaya_pca)


# square root transformation


data_salaya_pca$FIDi_sqrt = sqrt(data_salaya_pca$FIDi)


####a) MODEL #####
glmm_date_s_pc2 = lmer(FIDi_sqrt ~ PC2 + (1|date), data=data_salaya_pca, REML = F)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_s_pc2)
Fit = fitted(glmm_date_s_pc2)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_salaya_pca$date<- as.Date(data_salaya_pca$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_salaya_pca)


data_salaya_pca$date = as.factor(data_salaya_pca$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_salaya_pca)
abline(h=0,lty=2)

plot (x = data_salaya_pca$PC2,
      y = data_salaya_pca$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "PC2",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_pca, aes(Res), bins = 12)
p  

shapiro.test(Res) #OK 


# outliers 

plot(data_salaya_pca$PC2) #OK il y une petite valeur forte mais pas choquant
plot(data_salaya_pca$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_glmm_date_s_pc2= simulateResiduals(glmm_date_s_pc2)
testSpatialAutocorrelation(res_dharma_glmm_date_s_pc2, x =  data_salaya_pca$long, y = data_salaya_pca$lat)
# no spatial autocorrelation 

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_glmm_date_s_pc2, time =  data_salaya_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_salaya_pca$day=-1

data_salaya_pca$day[1:11]=1
data_salaya_pca$day[12:13]=2
data_salaya_pca$day[14:18]=3
data_salaya_pca$day[19:23]=4
data_salaya_pca$day[24]=5
data_salaya_pca$day[25]=6
data_salaya_pca$day[26]=7
data_salaya_pca$day[27:30]=8
data_salaya_pca$day[31:38]=9
data_salaya_pca$day[39:45]=10
data_salaya_pca$day[46:50]=11
data_salaya_pca$day[51:53]=12

# aggregating residuals by time
res_dharma_glmm_date_s_pc2 = recalculateResiduals(res_dharma_glmm_date_s_pc2, group = data_salaya_pca$day)
testTemporalAutocorrelation(res_dharma_glmm_date_s_pc2, time = unique(data_salaya_pca$day))
# no autocorralation on a field day scale 


#####c) RESULTS #####

summary(glmm_date_s_pc2) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.16 

r.squaredGLMM(glmm_date_s_pc2) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.033 et R2c = 0.2 so nope for correlation lol 


#############################################
####### 7) GLMM DATE WITH ONLY PHUTTHAMONTHON  PC2######
#############################################


# take only obs in phutthamonthon
data_phuttha_pca =  data_ld_pca[(data_ld_pca$site=="phutthamonthon" ) ,]
View(data_phuttha_pca)


# square root transformation


data_phuttha_pca$FIDi_sqrt = sqrt(data_phuttha_pca$FIDi)


####a) MODEL #####
glmm_date_p_pc2 = lmer(FIDi_sqrt ~ PC2 + (1|date), data=data_phuttha_pca, REML = F)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_p_pc2)
Fit = fitted(glmm_date_p_pc2)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_phuttha_pca$date<- as.Date(data_phuttha_pca$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_salaya_pca)


data_phuttha_pca$date = as.factor(data_phuttha_pca$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_phuttha_pca)
abline(h=0,lty=2)

plot (x = data_phuttha_pca$PC2,
      y = data_phuttha_pca$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "PC2",
      cex = 1.2 , pch = 16) #meh  

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha_pca, aes(Res), bins = 12)
p  #not that good 

shapiro.test(Res) #OK ? 


# outliers 

plot(data_phuttha_pca$PC2) #meh 
plot(data_phuttha_pca$FIDi_sqrt) #extremes but isoké

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_glmm_p_pc2 = simulateResiduals(glmm_date_p_pc2)
testSpatialAutocorrelation(res_dharma_glmm_p_pc2, x =  data_phuttha_pca$long, y = data_phuttha_pca$lat)
# no spatial autocorrelation 

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_glmm_p_pc2, time =  data_phuttha_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_phuttha_pca$day=-1

data_phuttha_pca$day[1:5]=1
data_phuttha_pca$day[6:10]=2
data_phuttha_pca$day[11:12]=3
data_phuttha_pca$day[13:16]=4
data_phuttha_pca$day[17:21]=5
data_phuttha_pca$day[22:27]=6
data_phuttha_pca$day[28:30]=7
data_phuttha_pca$day[31:33]=8
data_phuttha_pca$day[34:38]=9
data_phuttha_pca$day[39:43]=10
data_phuttha_pca$day[44:45]=11


# aggregating residuals by time
res_dharma_glmm_p_pc2 = recalculateResiduals(res_dharma_glmm_p_pc2, group = data_phuttha_pca$day)
testTemporalAutocorrelation(res_dharma_glmm_p_pc2, time = unique(data_phuttha_pca$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_date_p_pc2) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0 #curious 

r.squaredGLMM(glmm_date_p_pc2) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.22 et R2c = 0.22 => cause no effect f date for some reason, maybe the date is just not there because error of some sort

#manged to do lme but didn't find the p-value lol but non mixed version as best AIC anyway 
AIC ( glmm_date_p_pc2, model_phuttha_pc2)


##################################
#### 8) GLMM WITH SITES AS FIXED FACTOR AND DATE AS RANDOM PC2 #######
###############################



####a) MODEL #####

glmm_site_date_inter_pc2 = lmer(FIDi_sqrt ~ PC2 * site + (1|date), data=data_ld_pca, REML = F)
#glmm_site_date_pc2 = lmer(FIDi_sqrt ~ PC2 + site + (1|date), data=data_ld_pca, REML = F)

#AIC(glmm_site_date_pc2,glmm_site_date_inter_pc2) 

# AIC is smaller when accounting for the interaction so we only take this model from now on

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_site_date_inter_pc2)
Fit = fitted(glmm_site_date_inter_pc2)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_ld_pca$date<- as.Date(data_ld_pca$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_monitor)


data_ld_pca$date = as.factor(data_ld_pca$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_ld_pca)
abline(h=0,lty=2)

boxplot(Res ~ site, xlab = "site", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_ld_pca)
abline(h=0,lty=2)

plot (x = data_ld_pca$PC2,
      y = data_ld_pca$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "PC2",
      cex = 1.2 , pch = 16) #vertical line doesn't look good 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(Res), bins = 12)
p  

shapiro.test(Res) #OK


# outliers 

plot(data_ld_pca$PC2) # meh  
plot(data_ld_pca$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_site_date_inter_pc2 = simulateResiduals(glmm_site_date_inter_pc2)
testSpatialAutocorrelation(res_dharma_site_date_inter_pc2, x =  data_ld_pca$long, y = data_ld_pca$lat)
# no spatial autocorrelation  

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_site_date_inter_pc2, time =  data_ld_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 


data_ld_pca$day=-1

data_ld_pca$day[1:5]=1
data_ld_pca$day[6:16]=2
data_ld_pca$day[17:21]=3
data_ld_pca$day[22:23]=4
data_ld_pca$day[24:28]=5
data_ld_pca$day[29:30]=6
data_ld_pca$day[31:34]=7
data_ld_pca$day[35:39]=8
data_ld_pca$day[40]=9
data_ld_pca$day[41:45]=10
data_ld_pca$day[46:51]=11
data_ld_pca$day[52]=12
data_ld_pca$day[53:55]=13
data_ld_pca$day[56]=14
data_ld_pca$day[57:60]=15
data_ld_pca$day[61:63]=16
data_ld_pca$day[64:71]=17
data_ld_pca$day[72:76]=18
data_ld_pca$day[77:83]=19
data_ld_pca$day[84:88]=20
data_ld_pca$day[89:95]=21
data_ld_pca$day[96:98]=22

# aggregating residuals by time
res_dharma_site_date_inter_pc2 = recalculateResiduals(res_dharma_site_date_inter_pc2, group = data_ld_pca$day)
testTemporalAutocorrelation(res_dharma_site_date_inter_pc2, time = unique(data_ld_pca$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_site_date_inter_pc2) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.11
r.squaredGLMM(glmm_site_date_inter_pc2) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.31 et R2c = 0.39 


################################################
###########MODEL COMPARISON PC2#####################
################################################

AIC(glmm_date_pc2, glmm_site_date_inter_pc2, model_pc2, model_anc_pc2, model_inter_pc2)
min(AIC(glmm_date_pc2, glmm_site_date_inter_pc2, model_pc2, model_anc_pc2, model_inter_pc2)[,2])

# lowest AIC is model_inter_pc2

summary(model_inter_pc2)

# everything is significant : site, PC2 and PC2:site



#############################################
##### D/ LINEAR MODELS WITH CP3 #############
#############################################

######## PRELIMINARY CORRELATION ANALYSES PC3#############

plot(FIDi_sqrt ~ PC3, data=data_ld_pca, col="blue") # hmmmmmmmmmmmmmmmmmmmmmmm

cor.test(data_ld_pca$FIDi_sqrt, data_ld_pca$PC3) # doesn't seem to be correlated 
# p = 0.6 ; c = -0.05

#########################################################
######### 1) CLASSIC LINEAR MODEL WITH BOTH SITES PC3###########
######################################################

####a) Model ####
model_pc3 <- lm(FIDi_sqrt ~ PC3, data=data_ld_pca)

####b) Conditions of validity ###### 
model.res_pc3 <- residuals(model_pc3); model.res_pc3

# independance of residuals 
dwtest(model_pc3) # not OK

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_pc3) #kinda ok  

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(model.res_pc3), bins = 12)
p # OK 

shapiro.test(model.res_pc3) # OKKK 

# homoskedasticity
bptest(model_pc3) # OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_pc3 = simulateResiduals(model_pc3)
testSpatialAutocorrelation(res_dharma_model_pc3, x =  data_ld_pca$long, y = data_ld_pca$lat)
#  spatial autocorrelation which is ok because we don't take site into account  

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_model_pc3, time =  data_ld_pca$fid)
# temporal autocorrelation on the scale of individual observation !!!

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_ld_pca$day=-1
data_ld_pca$day[1:5]=1
data_ld_pca$day[6:16]=2
data_ld_pca$day[17:21]=3
data_ld_pca$day[22:23]=4
data_ld_pca$day[24:28]=5
data_ld_pca$day[29:30]=6
data_ld_pca$day[31:34]=7
data_ld_pca$day[35:39]=8
data_ld_pca$day[40]=9
data_ld_pca$day[41:45]=10
data_ld_pca$day[46:51]=11
data_ld_pca$day[52]=12
data_ld_pca$day[53:55]=13
data_ld_pca$day[56]=14
data_ld_pca$day[57:60]=15
data_ld_pca$day[61:63]=16
data_ld_pca$day[64:71]=17
data_ld_pca$day[72:76]=18
data_ld_pca$day[77:83]=19
data_ld_pca$day[84:88]=20
data_ld_pca$day[89:95]=21
data_ld_pca$day[96:98]=22



# aggregating residuals by time
res_dharma_model_pc3 = recalculateResiduals(res_dharma_model_pc3, group = data_ld_pca$day)
testTemporalAutocorrelation(res_dharma_model_pc3, time = unique(data_ld_pca$day))
# no autocorralation on a field day scale 

####c) Results ####
summary(model_pc3) # not significant (P = 0.6) 


plot(FIDi_sqrt ~ PC3, data=data_ld_pca, col="blue") # hmmmmmmmmmmmmmmmmmmmmmmm
abline(model_pc3, col="red")


####################################################################
###### 2) CLASSIC LINEAR MODEL WITH INTERACTION PC3 * SITE #####
#################################################################

####a) Model ####
model_inter_pc3 <- lm(FIDi_sqrt ~ PC3 * site, data=data_ld_pca)

####b) Conditions of validity ###### 
model_inter.res_pc3 <- residuals(model_inter_pc3); model_inter.res_pc3

# independance of residuals 
dwtest(model_inter_pc3) # INDEPENDANCE !!!!!! => we took account of the effect of sites 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_inter_pc3) #2 big clusters because sites u know

# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(model_inter.res_pc3), bins = 12)
p # kinda OK 

shapiro.test(model_inter.res_pc3) # not OK 

# homoskedasticity
bptest(model_inter_pc3) # OK 

#outliers 

plot(data_ld_pca$FIDi_sqrt) # ok

plot(data_ld_pca$PC3) # OK

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_model_inter_pc3 = simulateResiduals(model_inter_pc3)
testSpatialAutocorrelation(res_dharma_model_inter_pc3, x =  data_ld_pca$long, y = data_ld_pca$lat)
#   ok because we now take site into account  

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_model_inter_pc3, time =  data_ld_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!!

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_ld_pca$day=-1
data_ld_pca$day[1:5]=1
data_ld_pca$day[6:16]=2
data_ld_pca$day[17:21]=3
data_ld_pca$day[22:23]=4
data_ld_pca$day[24:28]=5
data_ld_pca$day[29:30]=6
data_ld_pca$day[31:34]=7
data_ld_pca$day[35:39]=8
data_ld_pca$day[40]=9
data_ld_pca$day[41:45]=10
data_ld_pca$day[46:51]=11
data_ld_pca$day[52]=12
data_ld_pca$day[53:55]=13
data_ld_pca$day[56]=14
data_ld_pca$day[57:60]=15
data_ld_pca$day[61:63]=16
data_ld_pca$day[64:71]=17
data_ld_pca$day[72:76]=18
data_ld_pca$day[77:83]=19
data_ld_pca$day[84:88]=20
data_ld_pca$day[89:95]=21
data_ld_pca$day[96:98]=22



# aggregating residuals by time
res_dharma_model_inter_pc3 = recalculateResiduals(res_dharma_model_inter_pc3, group = data_ld_pca$day)
testTemporalAutocorrelation(res_dharma_model_inter_pc3, time = unique(data_ld_pca$day))
# no autocorralation on a field day scale 

#### c) Results ####
summary(model_inter_pc3)# only site is explanatory 

AIC(model_pc3,model_inter_pc3) #model_inter better than model tout court 



##################################
########    3) ANCOVA PC3##########
###############################

####a) Condition of validity  ####

# verify linearity between response variable and explanatory variable 
ggscatter(
  data_ld_pca, x = "PC3", y = "FIDi_sqrt",
  color = "site", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(after_stat(eq.label), ..rr.label.., sep = "~~~~"), color = site)
  ) # meh
# bof bof effect in both sites 


#Homogeneity of regression slopes (no effect of the explenatory variable on the grouping variable (site))

data_ld_pca %>% anova_test(FIDi_sqrt ~ site*PC3) # performs type II anova between FIDi, PC3, site, and the PC3 + site 
# ony site seems to be significant 

# Normality of residuals  

# Calculer le modèle, la covariable passe en premier
model_anc_pc3 <- lm(FIDi_sqrt ~ PC3 + site, data = data_ld_pca)

model.metrics <- augment(model_anc_pc3) %>%
  select(-.hat, -.sigma, -.fitted)

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(model.metrics$.resid), bins = 12)
p # OK

shapiro_test(model.metrics$.resid) # not OK 

#Homogeneity of variances 

model.metrics %>% levene_test(.resid ~ site) # OK 

# outliers #the 3 is general rule of thumb  

model.metrics %>% 
  filter(abs(.std.resid) > 3) %>% 
  as.data.frame() #there is 2 but it's not that serious IG 

#### b) Model and Results ####

res.aov <- data_ld_pca %>% anova_test(FIDi_sqrt ~ PC3 + site)

get_anova_table(res.aov) #PC3 no significant with ANCOVA 



#########################################################
####### 4) CLASSIC LINEAR MODEL WITH ONLY SALAYA PC3 ######
#########################################################

# take only obs in salaya
data_salaya_pca =  data_ld_pca[(data_ld_pca$site=="salaya" ) ,]
View(data_salaya_pca)


# square root transformation


data_salaya_pca$FIDi_sqrt = sqrt(data_salaya_pca$FIDi)

View(data_salaya_pca)


####a) Model ####
model_salaya_pc3 <- lm(FIDi_sqrt ~ PC3, data=data_salaya_pca)

####b) Conditions of validity ###### 
model_salaya_pc3.res <- residuals(model_salaya_pc3)

# independance of residuals 
dwtest(model_salaya_pc3) # OK

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_salaya_pc3) #i've seen worse


# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_pca, aes(model_salaya_pc3.res), bins = 12)
p # meh

shapiro.test(model_salaya_pc3.res) # OK 

# homoskedasticity
bptest(model_salaya_pc3) # OK  

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_salaya_pc3 = simulateResiduals(model_salaya_pc3)
testSpatialAutocorrelation(res_dharma_salaya_pc3, x =  data_salaya_pca$long, y = data_salaya_pca$lat)
# no spatial autocorrelation 

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_salaya_pc3, time =  data_salaya_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_salaya_pca$day=-1

data_salaya_pca$day[1:11]=1
data_salaya_pca$day[12:13]=2
data_salaya_pca$day[14:18]=3
data_salaya_pca$day[19:23]=4
data_salaya_pca$day[24]=5
data_salaya_pca$day[25]=6
data_salaya_pca$day[26]=7
data_salaya_pca$day[27:30]=8
data_salaya_pca$day[31:38]=9
data_salaya_pca$day[39:45]=10
data_salaya_pca$day[46:50]=11
data_salaya_pca$day[51:53]=12

# aggregating residuals by time
res_dharma_salaya_pc3 = recalculateResiduals(res_dharma_salaya_pc3, group = data_salaya_pca$day)
testTemporalAutocorrelation(res_dharma_salaya_pc3, time = unique(data_salaya_pca$day))
# no autocorralation on a field day scale 


####c) Results ####
summary(model_salaya_pc3) # nope  (P = 0.16) 

plot(FIDi_sqrt ~ PC3, data=data_salaya_pca, col="blue") 
abline(a= model_salaya_pc3$coefficients[1], b= model_salaya_pc3$coefficients[2]) 


##################################################################
####### 4.5) CLASSIC LINEAR MODEL WITH ONLY PHUTTHAMONTHON PC3######
##################################################################

# take only obs in phutthamonthon 
data_phuttha_pca =  data_ld_pca[(data_ld_pca$site=="phutthamonthon" ) ,]
View(data_phuttha_pca)


# square root transformation


data_phuttha_pca$FIDi_sqrt = sqrt(data_phuttha_pca$FIDi)

View(data_phuttha_pca)


####a) Model ####
model_phuttha_pc3 <- lm(FIDi_sqrt ~ PC3, data=data_phuttha_pca)

####b) Conditions of validity ###### 
model_phuttha_pc3.res <- residuals(model_phuttha_pc3)

# independance of residuals 
dwtest(model_phuttha_pc3) # OK

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_phuttha_pc3) #looks okay


# normality of the residuals 
p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha_pca, aes(model_phuttha_pc3.res), bins = 12)
p # meh

shapiro.test(model_phuttha_pc3.res) # OK ?  

# homoskedasticity
bptest(model_phuttha_pc3) # OK  

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_phuttha_pc3 = simulateResiduals(model_phuttha_pc3)
testSpatialAutocorrelation(res_dharma_phuttha_pc3, x =  data_phuttha_pca$long, y = data_phuttha_pca$lat)
# no spatial autocorrelation 

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_phuttha_pc3, time =  data_phuttha_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_phuttha_pca$day=-1

data_phuttha_pca$day[1:5]=1
data_phuttha_pca$day[6:10]=2
data_phuttha_pca$day[11:12]=3
data_phuttha_pca$day[13:16]=4
data_phuttha_pca$day[17:21]=5
data_phuttha_pca$day[22:27]=6
data_phuttha_pca$day[28:30]=7
data_phuttha_pca$day[31:33]=8
data_phuttha_pca$day[34:38]=9
data_phuttha_pca$day[39:43]=10
data_phuttha_pca$day[44:45]=11


# aggregating residuals by time
res_dharma_phuttha_pc3 = recalculateResiduals(res_dharma_phuttha_pc3, group = data_phuttha_pca$day)
testTemporalAutocorrelation(res_dharma_phuttha_pc3, time = unique(data_phuttha_pca$day))
# no autocorralation on a field day scale 

####c) Results ####
summary(model_phuttha_pc3) # nope  (P =  0.46) 

plot(FIDi_sqrt ~ PC3, data=data_phuttha_pca, col="blue") 
abline(a= model_phuttha_pc3$coefficients[1], b= model_phuttha_pc3$coefficients[2]) #nicely anti-correlated :)  


#################################################
####### 5) GLMM TAKING DATE INTO ACCOUNT  PC3 ######
################################################

####a) MODEL #####
glmm_date_pc3 = lmer(FIDi_sqrt ~ PC3 + (1|date), data=data_ld_pca, REML = F)


####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_pc3)
Fit = fitted(glmm_date_pc3)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess even if 2 visible parts but we don't take sites into account

# Residuals against covariates

#change the date format to %Y%M%D
data_ld_pca$date<- as.Date(data_ld_pca$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
View(data_ld_pca)


data_ld_pca$date = as.factor(data_ld_pca$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_ld_pca)
abline(h=0,lty=2)

plot (x = data_ld_pca$PC3,
      y = data_ld_pca$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "PC3",
      cex = 1.2 , pch = 16) #the vertical line doesn't look good  

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(Res), bins = 12)
p  

shapiro.test(Res) #OK 


# outliers 

plot(data_ld_pca$PC3) #horizontal line yuck yuck 
plot(data_ld_pca$FIDi_sqrt) #OK 

#DHARMA test for autocorrelation ! 

# spatial 
res_dharma_glmm_date_pc3 = simulateResiduals(glmm_date_pc3)
testSpatialAutocorrelation(res_dharma_glmm_date_pc3, x =  data_ld_pca$long, y = data_ld_pca$lat)
# spatial autocorrelation  : we don't take site into account yet

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_glmm_date_pc3, time =  data_ld_pca$fid)
# temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 


data_ld_pca$day=-1
data_ld_pca$day[1:5]=1
data_ld_pca$day[6:16]=2
data_ld_pca$day[17:21]=3
data_ld_pca$day[22:23]=4
data_ld_pca$day[24:28]=5
data_ld_pca$day[29:30]=6
data_ld_pca$day[31:34]=7
data_ld_pca$day[35:39]=8
data_ld_pca$day[40]=9
data_ld_pca$day[41:45]=10
data_ld_pca$day[46:51]=11
data_ld_pca$day[52]=12
data_ld_pca$day[53:55]=13
data_ld_pca$day[56]=14
data_ld_pca$day[57:60]=15
data_ld_pca$day[61:63]=16
data_ld_pca$day[64:71]=17
data_ld_pca$day[72:76]=18
data_ld_pca$day[77:83]=19
data_ld_pca$day[84:88]=20
data_ld_pca$day[89:95]=21
data_ld_pca$day[96:98]=22


# aggregating residuals by time
res_dharma_glmm_date_pc3 = recalculateResiduals(res_dharma_glmm_date_pc3, group = data_ld_pca$day)
testTemporalAutocorrelation(res_dharma_glmm_date_pc3, time = unique(data_ld_pca$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_date_pc3) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
#ICC = 0.35
r.squaredGLMM(glmm_date_pc3) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.002 et R2c = 0.35 so nope for correlation lol 
# NONETHELESS !!!! SITE NOT TAKEN INTO ACCOUNT  !!!!

#############################################
####### 6) GLMM DATE WITH ONLY SALAYA  PC3######
#############################################



# take only obs in salaya
data_salaya_pca =  data_ld_pca[(data_ld_pca$site=="salaya" ) ,]
View(data_salaya_pca)


# square root transformation


data_salaya_pca$FIDi_sqrt = sqrt(data_salaya_pca$FIDi)


####a) MODEL #####
glmm_date_s_pc3 = lmer(FIDi_sqrt ~ PC3 + (1|date), data=data_salaya_pca, REML = F)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_s_pc3)
Fit = fitted(glmm_date_s_pc3)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_salaya_pca$date<- as.Date(data_salaya_pca$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_salaya_pca)


data_salaya_pca$date = as.factor(data_salaya_pca$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_salaya_pca)
abline(h=0,lty=2)

plot (x = data_salaya_pca$PC3,
      y = data_salaya_pca$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "PC3",
      cex = 1.2 , pch = 16) #OK 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_salaya_pca, aes(Res), bins = 12)
p  

shapiro.test(Res) #OK 


# outliers 

plot(data_salaya_pca$PC3) #OK il y une petite valeur forte mais pas choquant
plot(data_salaya_pca$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_glmm_date_s_pc3= simulateResiduals(glmm_date_s_pc3)
testSpatialAutocorrelation(res_dharma_glmm_date_s_pc3, x =  data_salaya_pca$long, y = data_salaya_pca$lat)
# no spatial autocorrelation 

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_glmm_date_s_pc3, time =  data_salaya_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_salaya_pca$day=-1

data_salaya_pca$day[1:11]=1
data_salaya_pca$day[12:13]=2
data_salaya_pca$day[14:18]=3
data_salaya_pca$day[19:23]=4
data_salaya_pca$day[24]=5
data_salaya_pca$day[25]=6
data_salaya_pca$day[26]=7
data_salaya_pca$day[27:30]=8
data_salaya_pca$day[31:38]=9
data_salaya_pca$day[39:45]=10
data_salaya_pca$day[46:50]=11
data_salaya_pca$day[51:53]=12

# aggregating residuals by time
res_dharma_glmm_date_s_pc3 = recalculateResiduals(res_dharma_glmm_date_s_pc3, group = data_salaya_pca$day)
testTemporalAutocorrelation(res_dharma_glmm_date_s_pc3, time = unique(data_salaya_pca$day))
# no autocorralation on a field day scale 


#####c) RESULTS #####

summary(glmm_date_s_pc3) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.18 

r.squaredGLMM(glmm_date_s_pc3) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.02 et R2c = 0.2 so nope for correlation lol 


#############################################
####### 7) GLMM DATE WITH ONLY PHUTTHAMONTHON PC3 ######
#############################################


# take only obs in phutthamonthon
data_phuttha_pca =  data_ld_pca[(data_ld_pca$site=="phutthamonthon" ) ,]
View(data_phuttha_pca)


# square root transformation


data_phuttha_pca$FIDi_sqrt = sqrt(data_phuttha_pca$FIDi)


####a) MODEL #####
glmm_date_p_pc3 = lmer(FIDi_sqrt ~ PC3 + (1|date), data=data_phuttha_pca, REML = F)

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_date_p_pc3)
Fit = fitted(glmm_date_p_pc3)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_phuttha_pca$date<- as.Date(data_phuttha_pca$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_salaya_pca)


data_phuttha_pca$date = as.factor(data_phuttha_pca$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_phuttha_pca)
abline(h=0,lty=2)

plot (x = data_phuttha_pca$PC3,
      y = data_phuttha_pca$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "PC3",
      cex = 1.2 , pch = 16) #meh  

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_phuttha_pca, aes(Res), bins = 12)
p  #not that good 

shapiro.test(Res) #OK ? 


# outliers 

plot(data_phuttha_pca$PC3) # good
plot(data_phuttha_pca$FIDi_sqrt) #extremes but isoké

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_glmm_p_pc3 = simulateResiduals(glmm_date_p_pc3)
testSpatialAutocorrelation(res_dharma_glmm_p_pc3, x =  data_phuttha_pca$long, y = data_phuttha_pca$lat)
# no spatial autocorrelation 

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_glmm_p_pc3, time =  data_phuttha_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 

data_phuttha_pca$day=-1

data_phuttha_pca$day[1:5]=1
data_phuttha_pca$day[6:10]=2
data_phuttha_pca$day[11:12]=3
data_phuttha_pca$day[13:16]=4
data_phuttha_pca$day[17:21]=5
data_phuttha_pca$day[22:27]=6
data_phuttha_pca$day[28:30]=7
data_phuttha_pca$day[31:33]=8
data_phuttha_pca$day[34:38]=9
data_phuttha_pca$day[39:43]=10
data_phuttha_pca$day[44:45]=11


# aggregating residuals by time
res_dharma_glmm_p_pc3 = recalculateResiduals(res_dharma_glmm_p_pc3, group = data_phuttha_pca$day)
testTemporalAutocorrelation(res_dharma_glmm_p_pc3, time = unique(data_phuttha_pca$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_date_p_pc3) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.02 #curious 

r.squaredGLMM(glmm_date_p_pc3) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.01 et R2c = 0.03 

AIC ( glmm_date_p_pc3, model_phuttha_pc3) # non mixed model is better


##################################
#### 8) GLMM WITH SITES AS FIXED FACTOR AND DATE AS RANDOM PC3#######
###############################



####a) MODEL #####

#glmm_site_date_inter_pc3 = lmer(FIDi_sqrt ~ PC3 * site + (1|date), data=data_ld_pca, REML = F)
glmm_site_date_pc3 = lmer(FIDi_sqrt ~ PC3 + site + (1|date), data=data_ld_pca, REML = F)

#AIC(glmm_site_date_pc3,glmm_site_date_inter_pc3) 

# AIC is smaller when not accounting for the interaction so we only take this model from now on

####b) CONDITION OF APPLICATION ####

# residuals against fitted values 
Res = resid(glmm_site_date_pc3)
Fit = fitted(glmm_site_date_pc3)
par(mfrow = c(1,1), mar= c(5,5,2,2))
plot(x = Fit, y = Res, xlab = 'Fitted values', ylab = 'Residuals', pch = 16, cex = 1.5)
abline(h=0,lty=2) # no strong pattern I guess

# Residuals against covariates

#change the date format to %Y%M%D
data_ld_pca$date<- as.Date(data_ld_pca$date, format="%d-%m-%Y") # CAUTION, IF DATE ALREADY REFORMATED = > RUN THIS LINE = FILL COLUMN WITH NA 
#View(data_monitor)


data_ld_pca$date = as.factor(data_ld_pca$date)

par(mfrow = c(1,2), mar= c(5,5,2,2))
boxplot(Res ~ date, xlab = "date", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_ld_pca)
abline(h=0,lty=2)

boxplot(Res ~ site, xlab = "site", ylab = 'residuals', pch = 16, cex = 1.5, col = 'indianred3', data = data_ld_pca)
abline(h=0,lty=2)

plot (x = data_ld_pca$PC3,
      y = data_ld_pca$FIDi_sqrt,
      ylab = "FIDi_sqrt",
      xlab = "PC3",
      cex = 1.2 , pch = 16) #vertical line doesn't look good 

# normality of residuals 

p = ggplot()
p = p+ylab ("Frequency")
p = p + xlab ("Pearson residuals")
p = p + theme (panel.background = element_blank())
p = p + theme (panel.border = element_rect(fill = NA, colour = "black", linewidth = 1))
p = p + theme(strip.background = element_rect(fill="white", color ="white", linewidth = 1))
p = p + theme(text = element_text(size = 15))
p = p+ geom_histogram(colour = "black", fill = "white", data = data_ld_pca, aes(Res), bins = 12)
p  # looks ok

shapiro.test(Res) #not OK for some reason 


# outliers 

plot(data_ld_pca$PC3) # OK
plot(data_ld_pca$FIDi_sqrt) #OK 

# DHARMA test for autocorrelation ! 

# spatial 
res_dharma_site_date_pc3 = simulateResiduals(glmm_site_date_pc3)
testSpatialAutocorrelation(res_dharma_site_date_pc3, x =  data_ld_pca$long, y = data_ld_pca$lat)
# no spatial autocorrelation  

# temporal 

### on the scale of individual observations
testTemporalAutocorrelation(res_dharma_site_date_pc3, time =  data_ld_pca$fid)
# no temporal autocorrelation on the scale of individual observation !!! 

### on the scale of field day 

# put a number for each day because the function don't like strings 


data_ld_pca$day=-1

data_ld_pca$day[1:5]=1
data_ld_pca$day[6:16]=2
data_ld_pca$day[17:21]=3
data_ld_pca$day[22:23]=4
data_ld_pca$day[24:28]=5
data_ld_pca$day[29:30]=6
data_ld_pca$day[31:34]=7
data_ld_pca$day[35:39]=8
data_ld_pca$day[40]=9
data_ld_pca$day[41:45]=10
data_ld_pca$day[46:51]=11
data_ld_pca$day[52]=12
data_ld_pca$day[53:55]=13
data_ld_pca$day[56]=14
data_ld_pca$day[57:60]=15
data_ld_pca$day[61:63]=16
data_ld_pca$day[64:71]=17
data_ld_pca$day[72:76]=18
data_ld_pca$day[77:83]=19
data_ld_pca$day[84:88]=20
data_ld_pca$day[89:95]=21
data_ld_pca$day[96:98]=22

# aggregating residuals by time
res_dharma_site_date_pc3 = recalculateResiduals(res_dharma_site_date_pc3, group = data_ld_pca$day)
testTemporalAutocorrelation(res_dharma_site_date_pc3, time = unique(data_ld_pca$day))
# no autocorralation on a field day scale 

#####c) RESULTS #####

summary(glmm_site_date_pc3) # Variance date / (variance residual + variance date) = ICC = correlation among random terms = how strong is the importance of the random variable 
# ICC = 0.17
r.squaredGLMM(glmm_site_date_pc3) #R2m = var explained by fixed effect and R2c = var explained by fixed + random effect en faisant la soustraction on a la variance expliquée par le  random effect. 
#R2m = 0.22 et R2c = 0.35 


################################################
###########MODEL COMPARISON PC3 #####################
################################################

AIC(glmm_date_pc3, glmm_site_date_pc3, model_pc3, model_anc_pc3, model_inter_pc3)
min(AIC(glmm_date_pc3, glmm_site_date_pc3, model_pc3, model_anc_pc3, model_inter_pc3)[,2])

# lowest AIC is glmm_site_date_pc3

lme(fixed = FIDi_sqrt ~ PC3 + site , random= ~ 1 |date, data=data_ld_pca, method = "ML")
summary(lme(fixed = FIDi_sqrt ~ PC3 + site , random= ~ 1 |date, data=data_ld_pca, method = "ML")
)

# only site is significant 
