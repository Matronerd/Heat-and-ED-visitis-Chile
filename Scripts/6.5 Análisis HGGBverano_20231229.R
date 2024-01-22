## Asociación total consultas, temperatura HConce Temporada verano 2009-2014 SCL ##

## Librerias ----
library(Epi); library(mgcv); library(dplyr); library (broom); library(tsibble); library(epiDisplay); library(lubridate); library(tsModel); library(splines); library(dlnm); library(ggplot2); library(tidyverse); library(zoo)

## BBDD Trabajo removiendo bases que no son necesarias ----
load("Out/SetHospitales.RData")
load("Out/t_dia_CCP_0918.RData")
options(na.action="na.exclude")
rm(HSJD, HLCM, HRRIO, HSere, HSJ, PCentral)

## HSJ todas las causas 2009-18 ----
HConce$ano<- year(HConce$fecha2)
HConce_1<- subset(HConce,IdCausa=="1" & (ano>2008 & ano<2019))
table(HConce_1$ano)

## Unión con las temperaturas ----
HConce_1temp<-merge(t_dia_CCP_0918,HConce_1, by=c("fecha2"), all.x=T)
data_HConce<-HConce_1temp

save.image("Out/HConce_20231228.Rdata")

## Temporada Verano 2009 - 2014 ----

data0914_HConce <- data_HConce %>%
  filter(year(fecha2) >=2009, year(fecha2)<=2014)

#Sólo Verano 2009-2014
data_ver_HConce <- data_HConce %>%
  filter(month(fecha2) %in% c(10,11,12,1,2,3)) %>%
  filter(year(fecha2) >=2009, year(fecha2)<=2014)

## Temporada Verano 2015 - 2018 ----

data1519_HConce <- data_HConce %>%
  filter(year(fecha2) >=2015, year(fecha2)<=2019)

#Sólo Verano 2015-2019

data_ver1519_HConce <- data1519_HConce %>%
  filter(month(fecha2) %in% c(10,11,12,1,2,3)) %>%
  filter(year(fecha2) >=2015, year(fecha2)<=2019)

#Convertir columnas a numéricas ----
data_ver_HConce <- data_ver_HConce %>%
  mutate(
    IdCausa = as.numeric(IdCausa),
    Col01 = as.numeric(Col01),
    Col02 = as.numeric(Col02),
    Col03 = as.numeric(Col03),
    Col04 = as.numeric(Col04),
    Col05 = as.numeric(Col05),
    Col06 = as.numeric(Col06),
  )

data_ver1519_HConce <- data_ver1519_HConce %>%
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
summary_sta<- summary(data_ver_HConce)
print(summary_sta)

summary_sta1519<- summary(data_ver1519_HConce)
print(summary_sta1519)

ts_data <- as_tsibble(data_ver_HConce, index = fecha2)
ts_data <- ts_data %>% dplyr::select(fecha2,Col01)
view(ts_data)

ts_data1519_HConce <- as_tsibble(data_ver1519_HConce, index = fecha2)
ts_data1519_HConce <- ts_data1519_HConce %>% dplyr::select(fecha2,Col01)
view(ts_data1519_HConce)

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

# SUB-PLOT FOR DAILY ED VISITS, WITH VERTICAL LINES DEFINING YEARS
plot(data_HConce$fecha2,data_HConce$Col01,pch=".",main="Daily ED visits HGGB 2009-2018, Concepción",
     ylab="Daily number of ED Visits",xlab="Date")
abline(v=data_HConce$fecha2[grep("-01-01",data_HConce$fecha2)],col=grey(0.6),lty=2)

plot(data_HConce$fecha2,data_HConce$t_mean,pch=".",main="Mean Temperature over 2009-2018, Concepción",
     ylab="Daily Temperature (°C)",xlab="Date")
abline(v=data_HConce$fecha2[grep("-01-01",data_HConce$fecha2)],col=grey(0.6),lty=2)
par(oldpar)
layout(1)

#Figure 2 - 2009-2014 series ----
oldpar <- par(no.readonly=TRUE)
par(mex=0.8,mfrow=c(2,1))

# SUB-PLOT FOR DAILY CONSULTS, WITH VERTICAL LINES DEFINING YEARS
plot(data0914_HConce$fecha2,data0914_HConce$Col01,pch=".",main="Daily ED visits HGGB 2009-2014, Concepción",
     ylab="Daily number of ED Visits",xlab="Date")
abline(v=data0914_HConce$fecha2[grep("-01-01",data0914_HConce$fecha2)],col=grey(0.6),lty=2)

# SUB-PLOT FOR DAILY TEMPERATURE, WITH VERTICAL LINES DEFINING YEARS 
plot(data0914_HConce$fecha2,data0914_HConce$t_mean,pch=".",main="Mean Temperature over 2009-2014, Concepción",
     ylab="Daily Temperature (°C)",xlab="Date")
abline(v=data0914_HConce$fecha2[grep("-01-01",data0914_HConce$fecha2)],col=grey(0.6),lty=2)
par(oldpar)
layout(1)

#Figure 3 - 2015-2018 series ----
oldpar <- par(no.readonly=TRUE)
par(mex=0.8,mfrow=c(2,1))

# SUB-PLOT FOR DAILY CONSULTS, WITH VERTICAL LINES DEFINING YEARS
plot(data1519_HConce$fecha2,data1519_HConce$Col01,pch=".",main="Daily ED visits HGGB 2015-2018, Concepción",
     ylab="Daily number of ED Visits",xlab="Date")
abline(v=data1519_HConce$fecha2[grep("-01-01",data1519_HConce$fecha2)],col=grey(0.6),lty=2)

# SUB-PLOT FOR DAILY TEMPERATURE, WITH VERTICAL LINES DEFINING YEARS 
plot(data1519_HConce$fecha2,data1519_HConce$t_mean,pch=".",main="Mean Temperature over 2015-2018, Concepción",
     ylab="Daily Temperature (°C)",xlab="Date")
abline(v=data1519_HConce$fecha2[grep("-01-01",data1519_HConce$fecha2)],col=grey(0.6),lty=2)
par(oldpar)
layout(1)

# Modelo Verano 2009-2014 ----

#Modelo con mes, año y consultas
model1 <- glm(Col01 ~ month/year,data = data_ver_HConce,family=quasipoisson)
model1a <- glm(Col01 ~ month+year,data = data_ver_HConce,family=quasipoisson)
model1b <- glm(Col01 ~ month+year+dow,data = data_ver_HConce,family=quasipoisson)
summary(model1)
summary(model1a) #MEJORA DISPERSIÓN
summary(model1b) #IGUAL DISPERSIÓN QUE ANTERIOR

#Modelo con mes, año, día y temperatura media y consultas
model2 <- glm(Col01 ~ month+year+dow + t_mean, family = quasipoisson, data = data_ver_HConce)
summary(model2)
pred2 <- predict(model2,type="response")
(eff2 <- ci.lin(model2,subset="t_mean",Exp=T))

#Figure 4 - Modelo estratificado por mes de las consultas predichas por modelo 2
plot(data_ver_HConce$fecha2,data_ver_HConce$Col01,ylim=c(100,600),pch=19,cex=0.2,col=grey(0.6),
     main="Time-stratified model (month strata)",ylab="Daily ED Visit",
     xlab="Date")
lines(data_ver_HConce$fecha2,pred2,lwd=2)

#Splines ---- cúbica con 4 grados de libertad propuesto por Thomas y Sun

#Creación de splines
data0914_HConce$time <- seq(nrow(data0914_HConce))
spl <- bs(data0914_HConce$time,degree=3,df=4)

data_ver_HConce$time <- seq(nrow(data_ver_HConce))
spl_ver <- bs(data_ver_HConce$time,degree=3,df=4) 

#Modelo con spline cúbico, temp media y consultas #MEJORA MUCHO LA DISPERSIÓN (reduce 2 puntos)
model4 <- glm(Col01 ~ spl_ver + t_mean,data = data_ver_HConce,family=quasipoisson)
summary(model4)
pred4 <- predict(model4,type="response")
(eff4 <- ci.lin(model4,subset="t_mean",Exp=T))

plot(data_ver_HConce$fecha2,data_ver_HConce$Col01,ylim=c(100,300),pch=19,cex=0.2,col=grey(0.6),
     main="Flexible cubic spline model",ylab="Daily ED Visits During Summer",
     xlab="Date")
lines(data_ver_HConce$fecha2,pred4,lwd=2)

# BUILD A SUMMARY TABLE WITH EFFECT AS PERCENT INCREASE
tabeff_ver <- rbind(eff2,eff4)[,5:7]
tabeff_ver <- (tabeff_ver-1)*100
dimnames(tabeff_ver) <- list(c("Unadjusted","Plus Seasonal Trend"),
                             c("RR","ci.low","ci.hi"))
round(tabeff_ver,2)


# Modelo Verano 2015-2018 ----

#Modelo con mes, año y consultas
model_1 <- glm(Col01 ~ month/year,data = data_ver1519_HConce,family=quasipoisson)
model_1a <- glm(Col01 ~ month+year,data = data_ver1519_HConce,family=quasipoisson)
model_1b <- glm(Col01 ~ month+year+dow,data = data_ver1519_HConce,family=quasipoisson)
summary(model_1) #Mejor dispersión que el resto
summary(model_1a) 
summary(model_1b) 

#Modelo con mes, año, día y temperatura media y consultas
model_2 <- glm(Col01 ~ month+year+dow + t_mean, family = quasipoisson, data = data_ver1519_HConce)
summary(model_2)
pred_2 <- predict(model_2,type="response")
(eff_2 <- ci.lin(model_2,subset="t_mean",Exp=T))

#Figure 4 - Modelo estratificado por mes de las consultas predichas por modelo 2
plot(data_ver1519_HConce$fecha2,data_ver1519_HConce$Col01,ylim=c(100,600),pch=19,cex=0.2,col=grey(0.6),
     main="Time-stratified model (month strata)",ylab="Daily ED Visit",
     xlab="Date")
lines(data_ver1519_HConce$fecha2,pred_2,lwd=2)

#Splines ---- cúbica con 4 grados de libertad propuesto por Thomas y Sun

#Creación de splines
data0914_HConce$time <- seq(nrow(data0914_HConce))
spl <- bs(data0914_HConce$time,degree=3,df=4)

data_ver1519_HConce$time <- seq(nrow(data_ver1519_HConce))
spl_ver1519_HConce <- bs(data_ver1519_HConce$time,degree=3,df=4) 

#Modelo con spline cúbico, temp media y consultas
model_4 <- glm(Col01 ~ spl_ver1519_HConce + t_mean,data = data_ver1519_HConce,family=quasipoisson)
summary(model_4)
pred_4 <- predict(model_4,type="response")
(eff_4 <- ci.lin(model_4,subset="t_mean",Exp=T))

plot(data_ver1519_HConce$fecha2,data_ver1519_HConce$Col01,ylim=c(100,600),pch=19,cex=0.2,col=grey(0.6),
     main="Flexible cubic spline model",ylab="Daily ED Visits During Summer",
     xlab="Date")
lines(data_ver1519_HConce$fecha2,pred_4,lwd=2)

# BUILD A SUMMARY TABLE WITH EFFECT AS PERCENT INCREASE
tabeff_ver1519 <- rbind(eff_2,eff_4)[,5:7]
tabeff_ver1519 <- (tabeff_ver1519-1)*100
dimnames(tabeff_ver1519) <- list(c("Unadjusted","Plus Seasonal Trednd"),
                                 c("RR","ci.hi","ci.low"))
round(tabeff_ver1519,2)