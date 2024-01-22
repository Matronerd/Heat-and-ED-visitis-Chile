## Asociación total consultas, temperatura Posta Central Temporada verano 2009-2014 SCL ##
install.packages("mgcv")

## Librerias ----
library(Epi); library(mgcv); library(dplyr); library (broom); library(tsibble); library(epiDisplay); library(lubridate); library(tsModel); library(splines); library(dlnm); library(ggplot2); library(tidyverse); library(zoo)

## BBDD Trabajo removiendo bases que no son necesarias ----
load("Out/SetHospitales.RData")
load("Out/t_dia_SCL_0918.RData")
options(na.action="na.exclude")
rm(HConce, HLCM, HRRIO, HSere, HSJ, HSJD)

## Posta Central (HUAP) todas las causas 2009-18 ----
PCentral$ano<- year(PCentral$fecha2)
PCentral_1<- subset(PCentral,IdCausa=="1" & (ano>2008 & ano<2019))
table(PCentral_1$ano)

## Unión con las temperaturas ----
PCentral_1temp<-merge(t_dia_SCL_0918,PCentral_1, by=c("fecha2"), all.x=T)
data<-PCentral_1temp

save.image("Out/HUAP_20231124.Rdata")

## Temporada Verano 2009 - 2014 ----

data0914 <- data %>%
  filter(year(fecha2) >=2009, year(fecha2)<=2014)

data_ver <- data %>%
  filter(month(fecha2) %in% c(10,11,12,1,2,3)) %>%
  filter(year(fecha2) >=2009, year(fecha2)<=2014)

#Convertir columnas a numéricas
data_ver <- data_ver %>%
  mutate(
    IdCausa = as.numeric(IdCausa),
    Col01 = as.numeric(Col01),
    Col02 = as.numeric(Col02),
    Col03 = as.numeric(Col03),
    Col04 = as.numeric(Col04),
    Col05 = as.numeric(Col05),
    Col06 = as.numeric(Col06),
  )

## Descriptivo ----
summary_sta<- summary(data_ver)
print(summary_sta)

ts_data <- as_tsibble(data_ver, index = fecha2)
ts_data <- ts_data %>% dplyr::select(fecha2,Col01)
view(ts_data)
##################################
# OPTION 1: TIME-STRATIFIED MODEL
# (SIMPLE INDICATOR VARIABLES)
##################################

# GENERATE MONTH AND YEAR # ya están contenidos en el data frame

# FIT A POISSON MODEL WITH A STRATUM FOR EACH MONTH NESTED IN YEAR
# (USE OF quasipoisson FAMILY FOR SCALING THE STANDARD ERRORS)

#Figure 1 - The long series justify to divide into two periods 2009-2014 and 2015-2018
oldpar <- par(no.readonly=TRUE)
par(mex=0.8,mfrow=c(2,1))

# SUB-PLOT FOR DAILY DEATHS, WITH VERTICAL LINES DEFINING YEARS
plot(data$fecha2,data$Col01,pch=".",main="Daily ED visits HUAP 2009-2018, Santiago",
     ylab="Daily number of ED Visits",xlab="Date")
abline(v=data$fecha2[grep("-01-01",data$fecha2)],col=grey(0.6),lty=2)

plot(data$fecha2,data$t_mean,pch=".",main="Mean Temperature over 2009-2018, Santiago",
     ylab="Daily Temperature (°C)",xlab="Date")
abline(v=data$fecha2[grep("-01-01",data$fecha2)],col=grey(0.6),lty=2)
par(oldpar)
layout(1)

#Figure 2 - 2009-2014 series
oldpar <- par(no.readonly=TRUE)
par(mex=0.8,mfrow=c(2,1))

# SUB-PLOT FOR DAILY DEATHS, WITH VERTICAL LINES DEFINING YEARS
plot(data0914$fecha2,data0914$Col01,pch=".",main="Daily ED visits HUAP 2009-2014, Santiago",
     ylab="Daily number of ED Visits",xlab="Date")
abline(v=data0914$fecha2[grep("-01-01",data0914$fecha2)],col=grey(0.6),lty=2)

# Temperaturas
plot(data0914$fecha2,data0914$t_mean,pch=".",main="Mean Temperature over 2009-2014, Santiago",
     ylab="Daily Temperature (°C)",xlab="Date")
abline(v=data0914$fecha2[grep("-01-01",data0914$fecha2)],col=grey(0.6),lty=2)
par(oldpar)
layout(1)

#Modelo con mes, año y consultas
model1 <- glm(Col01 ~ month/year,data = data_ver,family=quasipoisson)
model1a <- glm(Col01 ~ month+year,data = data_ver,family=quasipoisson)
summary(model1)
summary(model1a)

#Modelo con mes, año y temperatura media y consultas
model_2 <- glm(Col01 ~ month+year + t_mean, family = quasipoisson, data = data_ver)
summary(model_2)
pred1 <- predict(model_2,type="response")

plot(data_ver$fecha2,data_ver$Col01,ylim=c(100,500),pch=19,cex=0.2,col=grey(0.6),
     main="Time-stratified model (month strata)",ylab="Daily ED Visit",
     xlab="Date")
lines(data_ver$fecha2,pred1,lwd=2)

plot(data0914$fecha2,data0914$Col01,ylim=c(100,500),pch=19,cex=0.2,col=grey(0.6),
     main="Time-stratified model (month strata)",ylab="Daily ED Visit",
     xlab="Date")
lines(data0914$fecha2,pred1,lwd=2)

#Splines ---- cúbica con 4 grados de libertad prpopuesto por Thomas y Sun
data_ver$time <- seq(nrow(data_ver))

spl_ver <- bs(data_ver$time,degree=3,df=4) 


data0914$time <- as.numeric(data0914$fecha2 - min(data0914$fecha2))
data0914$Col01 <- as.numeric(data0914$Col01)

spl <- bs(data0914$time,degree=3,df=4) 

model3 <- glm(Col01 ~ spl_ver,data = data_ver,family=quasipoisson)
summary(model3)

pred3 <- predict(model3,type="response")

plot(data_ver$fecha2,data_ver$Col01,ylim=c(100,500),pch=19,cex=0.2,col=grey(0.6),
     main="Flexible cubic spline model",ylab="Daily ED Visits During Summer",
     xlab="Date")
lines(data_ver$fecha2,pred3,lwd=2)


model4 <- glm(Col01 ~ spl_ver,data = data_ver,family=quasipoisson)
summary(model4)
pred4 <- predict(model4,type="response")

plot(data_ver$fecha2,data_ver$Col01,ylim=c(100,500),pch=19,cex=0.2,col=grey(0.6),
     main="Flexible cubic spline model",ylab="Daily ED Visits During Summer",
     xlab="Date")
lines(data_ver$fecha2,pred4,lwd=2)

m4 <- glm(Col01 ~ spl_ver + t_mean,data = data_ver,family=quasipoisson)
summary(m4)
(eff4 <- ci.lin(m4,subset="t_mean",Exp=T))

m4a <- glm(Col01 ~ spl_ver + t_max + dow, data = data_ver,family=quasipoisson)
summary(m4a)
(eff4a <- ci.lin(m4a,subset="t_max",Exp=T))

# CONTROLLING FOR SEASONALITY (WITH SPLINE AS IN MODEL 3)
m5 <- glm(Col01 ~ t_mean + spl_ver,data = data_ver,family=quasipoisson)
summary(m5)
(eff5 <- ci.lin(m5,subset="t_mean",Exp=T))


#DNLM ----



