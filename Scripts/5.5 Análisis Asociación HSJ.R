## Asociaci�n total consultas, temperatura Hospital San Jos� - Santiago##

## Librerias ----
library(lubridate); library(tsModel); library(splines); library(dlnm); library(dplyr)

# Remover objetos del espacio de trabajo ----
rm(list = ls())
options(na.action="na.exclude")

#Cargar funciones para series de tiempo y temperatura ----
source("qAIC.R")
source("findmin.R")

## BBDD Trabajo removiendo bases que no son necesarias ----
load("Out/SetHospitales.RData")
load("Out/t_dia_SCL_0918.RData")
rm(HConce, HLCM, HRRIO, HSere, PCentral, HSJD)

## Hospital de La Serena todas las causas 2009-18 ----
HSJ$ano<- year(HSJ$fecha2)
HSJ_1<- subset(HSJ,IdCausa=="1" & (ano>2008 & ano<2019))
table(HSJ_1$ano)

## Uni�n con las temperaturas ----
HSJ_1temp<-merge(t_dia_SCL_0918,HSJ_1, by=c("fecha2"), all.x=T)
data<-HSJ_1temp

save.image("Out/SCL_HSJ_20230903.RData")

## Revisar m�nimos y m�ximos de consulta para manejo temporales de 0 ----
HSJ_1temp_f <- HSJ_1temp %>%
  filter(fecha2 >= as.Date("2009-01-01") & fecha2 <= as.Date("2018-12-31"))
tabla_resumen <- HSJ_1temp_f %>%
  group_by(year) %>%
  summarise(
    MinConsultas = min(n, na.rm = TRUE),  # M�nimo, ignorando NAs
    MaxConsultas = max(n, na.rm = TRUE)   # M�ximo, ignorando NAs
  )

#NA los valores inferiores a 1 (1 datos), hasta no encontrar una mejor forma de manejar los 0 ----
which(data$Col01<1)
#data$Col01<-ifelse(data$Col01<1, NA , data$Col01)
#Si se asigna NA se pierden 1 datos entre 2014-2018, esa puede ser la raz�n por la baja en 2014,

####################################################################################
###  Gr�ficos de series de tiempo para Hospital de La Serena
####################################################################################

# Figure for Time series plots.
pdf("Fig/STConsultasTemp_HSJ.pdf")
par(mex=0.8,mfrow=c(2,1))
# Consultas
plot(data$fecha2, data$Col01, type="l", col= "black", ylab="N�consultas", xlab="Fecha", main="N�mero de Consultas 2009-2018 HSJ-Santiago")
# Temperatura
plot(data$fecha2, data$t_mean, type="l", col= "red4", ylab="Temperatura (�C)", xlab="Fecha", main="Temperatura (�C) 2009-2018, Santiago")
dev.off()

####################################################################################
###  Descomposici�n de las series de tiempo
####################################################################################

# Figura para la descomposici�n
par(mex=0.8,mfrow=c(2,3))

# Consultas
all.cu <- tsdecomp(as.numeric(data$Col01), c(1,2,15,nrow(data)))
plot(data$fecha2, all.cu[,1], xlab="Fecha", ylab="Tendencia a Largo Plazo", main="Consultas")
plot(data$fecha2, all.cu[,2], xlab="Fecha", ylab="Estacionalidad")
plot(data$fecha2, all.cu[,3], xlab="Fecha",  ylab="Residuales")

# Temperatura
tmean.cu <- tsdecomp(data$t_mean, c(1,2,15,nrow(data)))
plot(data$fecha2, tmean.cu[,1], xlab="Fecha", ylab="Tendencia a Largo Plazo", main="Temperatura (�C)")
plot(data$fecha2, tmean.cu[,2], xlab="Fecha", ylab="Estacionalidad")
plot(data$fecha2, tmean.cu[,3], xlab="Fecha", ylab="Residuales")

# Close figure.
layout(1)

####################################################################################
###  Gr�ficos de Caja y Bigotes
####################################################################################

# Figure for boxplots.
par(mex=0.8,mfrow=c(2,3))

# Consultas
boxplot(as.numeric(Col01)~year, data=data, ylab="N� consultas", xlab="A�o")
boxplot(as.numeric(Col01)~month, data=data,ylab="N� consultas", xlab="Mes")
boxplot(as.numeric(Col01)~dow, data=data, ylab="N� consultas", xlab="D�a de la Semana")

# Temperatura
boxplot(t_mean~year, data=data, ylab="Temperatura (�C)", xlab="A�o")
boxplot(t_mean~month, data=data, ylab="Temperatura (�C)", xlab="Mes")
boxplot(t_mean~dow, data=data, ylab="Temperatura (�C)", xlab="D�a de la Semana")

# Close figure.
layout(1)

####################################################################################
###  Modelamiento del marco para las time-series regression
####################################################################################

# Sobredispersi�n y Autocorrelaci�n de las consultas time-series
#################################################################

data$Col01 <- as.numeric(data$Col01)

# Revisar par�metro de sobredispersi�n (5,6)
model0 <- glm(Col01 ~ 1, data=data, family=quasipoisson)
summary(model0)
mean(data$Col01, na.rm = T)
var(data$Col01, na.rm = T)

# Autocorrelation plot - mucha autocrrelaci�n, en los primeros d�as del mes. 
pacf(data$Col01, lag=30, ylim=c(0,1), na.action = na.pass)

# Modelos Time-stratified 
########################

# Figura para time stratified models.
par(mex=0.8,mfrow=c(3,1))

# Modelo con a�o (5,0)
data_clean <- data[!is.na(data$Col01) & !is.na(data$year), ]

model1a <- glm(Col01 ~ factor(year), data=data_clean, family=quasipoisson)
summary(model1a)
pred1a <- predict(model1a, type="response")

plot(data_clean$fecha2, data_clean$Col01, ylim=c(0,500), pch=19, cex=0.5, col=grey(0.6),
     ylab="N� consultas", xlab="Fecha")
lines(data_clean$fecha2, pred1a, lwd=5, col="darkgreen")

# Modelo con mes (5,3)
model1b <- glm(Col01 ~ factor(month), data=data_clean, family=quasipoisson)
summary(model1b)
pred1b <- predict(model1b, type="response")

plot(data_clean$fecha2, data_clean$Col01, ylim=c(0,500), pch=19, cex=0.5, col=grey(0.6),
     ylab="N� de consultas", xlab="Fecha")
lines(data_clean$fecha2, pred1b, lwd=5, col="darkgreen")

# Modelo con mes y a�o. (4,71)
model1c <- glm(Col01 ~ factor(year) + factor(month), data=data_clean, family=quasipoisson)
summary(model1c)
pred1c <- predict(model1c, type="response")

plot(data_clean$fecha2, data_clean$Col01, ylim=c(0,500), pch=19, cex=0.5, col=grey(0.6),
     ylab="N� de consultas", xlab="Fecha")
lines(data_clean$fecha2, pred1c, lwd=5, col="darkgreen")

# Close figure.
layout(1)

# Funciones Periodicas 
####################

# Figura para funciones periodicas
par(mex=0.8,mfrow=c(4,1))

# Modelo con un par de seno-coseno (4,9)
data_clean$time <- seq(nrow(data_clean))
fourier <- harmonic(data_clean$time, nfreq=1, period=365.25)

model2a <- glm(Col01 ~ fourier + time, data=data_clean, family=quasipoisson)
summary(model2a)
pred2a <- predict(model2a, type="response")

plot(data_clean$fecha2, data_clean$Col01, ylim=c(0,500), pch=19, cex=0.5, col=grey(0.6),
     ylab="Num. of all", xlab="Date")
lines(data_clean$fecha2, pred2a, lwd=5, col="blue")

# Model with two sine-cosine pairs. (4,9)
fourier <- harmonic(data_clean$time, nfreq=2, period=365.25)

model2b <- glm(Col01 ~ fourier + time, data=data_clean, family=quasipoisson)
summary(model2b)
pred2b <- predict(model2b, type="response")

plot(data_clean$fecha2, data_clean$Col01, ylim=c(0,500), pch=19, cex=0.5, col=grey(0.6),
     ylab="Num. of all", xlab="Date")
lines(data_clean$fecha2, pred2b, lwd=5, col="blue")

# Model with four sine-cosine pairs (parametro 4,8)
fourier <- harmonic(data_clean$time, nfreq=4, period=365.25)

model2c <- glm(Col01 ~ fourier + time, data=data_clean, family=quasipoisson)
summary(model2c)
pred2c <- predict(model2c, type="response")

plot(data_clean$fecha2, data_clean$Col01, ylim=c(0,500), pch=19, cex=0.5, col=grey(0.6),
     ylab="Num. of all", xlab="Date")
lines(data_clean$fecha2, pred2c, lwd=5, col="blue")

# Model with 8 sine-cosine pairs. (Parametro de dispersi�n = 4,8)
fourier <- harmonic(data_clean$time, nfreq=8, period=365.25)

model2d <- glm(Col01 ~ fourier + time, data=data_clean, family=quasipoisson)
summary(model2c)
pred2d <- predict(model2d, type="response")

plot(data_clean$fecha2, data_clean$Col01, ylim=c(0,500), pch=19, cex=0.5, col=grey(0.6),
     ylab="Num. of all", xlab="Date")
lines(data_clean$fecha2, pred2d, lwd=5, col="blue")

# Close figure.
layout(1)

# Model with a natural cubic spline with 1 df/year (4,95)
numyears <- length(unique(data_clean$year))
numyears
spl <- ns(data_clean$time, df=1*numyears)

model3a <- glm(Col01 ~ spl , data=data_clean, family=quasipoisson)
summary(model3a)
pred3a <- predict(model3a, type="response")

plot(data_clean$fecha2, data_clean$Col01, ylim=c(0,500), pch=19, cex=0.5, col=grey(0.6),
     ylab="N� de consultas", xlab="Fecha")
lines(data_clean$fecha2, pred3a, lwd=5, col="blue")

# Model with a natural cubic spline with 6 df/year. (4,64)
spl <- ns(data_clean$time, df=6*numyears)

model3b <- glm(Col01 ~ spl , data=data_clean, family=quasipoisson)
summary(model3b)
pred3b <- predict(model3b, type="response")

plot(data_clean$fecha2, data_clean$Col01, ylim=c(0,500), pch=19, cex=0.5, col=grey(0.6),
     ylab="N� de consultas", xlab="Fecha")
lines(data_clean$fecha2, pred3b, lwd=5, col="blue")

# Model with a natural cubic spline with 8 df/year. (4,60)
spl <- ns(data_clean$time, df=8*numyears)

model3c <- glm(Col01 ~ spl , data=data_clean, family=quasipoisson)
summary(model3c)
pred3c <- predict(model3c, type="response")

plot(data_clean$fecha2, data_clean$Col01, ylim=c(0,500), pch=19, cex=0.5, col=grey(0.6),
     ylab="N� de consultas", xlab="Fecha")
lines(data_clean$fecha2, pred3c, lwd=5, col="blue")

# Model with a natural cubic spline with 12 df/year. (4,55)
spl <- ns(data_clean$time, df=12*numyears)

model3d <- glm(Col01 ~ spl , data=data_clean, family=quasipoisson)
summary(model3d)
pred3d <- predict(model3d, type="response")

plot(data_clean$fecha2, data_clean$Col01, ylim=c(0,500), pch=19, cex=0.5, col=grey(0.6),
     ylab="N� de consultas", xlab="Fecha")
lines(data_clean$fecha2, pred3d, lwd=5, col="blue")

# Model with a natural cubic spline with 24 df/year. (4,52)
spl <- ns(data_clean$time, df=24*numyears)

model3e <- glm(Col01 ~ spl , data=data_clean, family=quasipoisson)
summary(model3e)
pred3e <- predict(model3e, type="response")

plot(data_clean$fecha2, data_clean$Col01, ylim=c(0,500), pch=19, cex=0.5, col=grey(0.6),
     ylab="N� de consultas", xlab="Fecha")
lines(data_clean$fecha2, pred3e, lwd=5, col="blue")

####################################################################################
###  Comparing modeling strategies
####################################################################################

# Joint figure for Autocorrelation functions.
par(mex=0.8,mfrow=c(2,2))

# Unadjusted model.
qaic0 <- qAIC(model0, type="dev")
disp0 <- sqrt(summary(model0)$dispersion)
res0 <- residuals(model0, type="response")
pacf(res0, na.action=na.pass, ylim=c(0,1), 
     main=paste("Unadjusted | ", "qAIC=" , round(qaic0,1) , ", overdisp=" , round(disp0,2)))

# Time-stratified model.
qaic1 <- qAIC(model1c, type="dev")
disp1 <- sqrt(summary(model1c)$dispersion)
res1 <- residuals(model1c, type="response")
pacf(res1, na.action=na.pass, ylim=c(0,1), 
     main=paste("Time stratified | ", "qAIC=" , round(qaic1,1) , ", overdisp=" , round(disp1,2)))

# Periodic functions.
qaic2 <- qAIC(model2c, type="dev")
disp2 <- sqrt(summary(model2c)$dispersion)
res2 <- residuals(model2c, type="response")
pacf(res2, na.action=na.pass, ylim=c(0,1), 
     main=paste("Periodic functions | ", "qAIC=" , round(qaic2,1) , ", overdisp=" , round(disp2,2)))

# Spline function. (modelo con 24 grados de libertad)
qaic3 <- qAIC(model3e, type="dev")
disp3 <- sqrt(summary(model3e)$dispersion)
res3 <- residuals(model3e, type="response")
pacf(res3, na.action=na.pass, ylim=c(0,1), 
     main=paste("Splines | ", "qAIC=" , round(qaic3,1) , ", overdisp=" , round(disp3,2)))

# Close figure.
layout(1)

####################################################################################
###  Exposure-response for temperature
####################################################################################

# Linear association
df <- 6*numyears
df
spl <- ns(data_clean$time, df)

model1 <- glm(Col01 ~ spl + t_mean , data=data_clean, family=quasipoisson)
coefficients <- coef(model1)
pred_data <- data.frame(ns(mean(data_clean$time), df), data_clean$t_mean)
linear_predictor <- as.vector(as.matrix(pred_data)%*%coefficients[-1]) + coefficients[1]
predicted1 <- exp(linear_predictor)
plot(data_clean$t_mean, predicted1, type="p", col="blue",
     ylab="N� consultas", xlab="Temperature (�C)")

# Non-linear association
model2 <- glm(Col01 ~ spl + ns(t_mean, 4) , data=data_clean, family=quasipoisson)
coefficients <- coef(model2)
pred_data <- data.frame(ns(mean(data_clean$time), df), ns(data_clean$t_mean, 4))
linear_predictor <- as.vector(as.matrix(pred_data)%*%coefficients[-1]) + coefficients[1]
predicted2 <- exp(linear_predictor)
pdf("Fig/ConsultasyTempLineal_HSJ.pdf")
plot(data_clean$t_mean, predicted2, type="p", col="red",
     ylab="N� consultas", xlab="Temperature (�C)")
points(data_clean$t_mean, predicted1, type="p", col="blue")
dev.off()

# Find the MMT.(minimum mortality temperature (MMT))
mmt <- data$t_mean[which.min(predicted2)]
mmt

# Generate RR centered at MMT.
data_clean$rr <- predicted2/predicted2[mmt]
data_clean <- data_clean[order(data_clean$t_mean),]
pdf("Fig/RR_HSJ.pdf")
plot(data_clean$t_mean, data_clean$rr, type="l", col="red", lwd=3,
     ylab="Riesgo Relativo", xlab="Temperature (�C)")
abline(h=1)
dev.off()