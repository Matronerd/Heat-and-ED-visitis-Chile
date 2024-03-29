
####################################################################################
###  DLNM para HUAP solo verano 
####################################################################################


# Remove all objects in workspace.
rm(list = ls())
options(na.action="na.exclude")

# Install packages.
#install.packages("tsModel","splines","dlnm","Epi")

# Load libraries.
library(tsModel); library(splines); library(dlnm); library(Epi); library(tidyverse)

# Load functions.
source("qAIC.R"); source("findmin.R"); source("attrdl.R")

####################################################################################
###  Load HUAP 
####################################################################################

# Load and inspect the dataset.
load("Out/HUAP_20231228.RData")

## Temporada Verano 2009 - 2014 ----

dataHUAP0914 <- data%>%
  filter(year(fecha2) >=2009, year(fecha2)<=2014)

#S�lo Verano 2009-2014
data_ver_HUAP <- dataHUAP0914 %>%
  filter(month(fecha2) %in% c(10,11,12,1,2,3)) %>%
  filter(year(fecha2) >=2009, year(fecha2)<=2014)

## Temporada Verano 2015 - 2018 ----

data1519_HUAP <- data %>%
  filter(year(fecha2) >=2015, year(fecha2)<=2019)

#S�lo Verano 2015-2019

data_ver1519_HUAP <- data1519_HUAP %>%
  filter(month(fecha2) %in% c(10,11,12,1,2,3)) %>%
  filter(year(fecha2) >=2015, year(fecha2)<=2019)

# Generate time variable
data_ver_HUAP$time <- seq(nrow(data_ver_HUAP))

data_ver1519_HUAP$time <- seq(nrow(data_ver1519_HUAP))

#Numeric transformation
data_ver_HUAP <- data_ver_HUAP %>%
  mutate(
    IdCausa = as.numeric(IdCausa),
    Col01 = as.numeric(Col01),
    Col02 = as.numeric(Col02),
    Col03 = as.numeric(Col03),
    Col04 = as.numeric(Col04),
    Col05 = as.numeric(Col05),
    Col06 = as.numeric(Col06),
  )

data_ver1519_HUAP <- data_ver1519_HUAP %>%
  mutate(
    IdCausa = as.numeric(IdCausa),
    Col01 = as.numeric(Col01),
    Col02 = as.numeric(Col02),
    Col03 = as.numeric(Col03),
    Col04 = as.numeric(Col04),
    Col05 = as.numeric(Col05),
    Col06 = as.numeric(Col06),
  )
####################################################################################
### Single lag exposure-response relationship
####################################################################################

# Temperature-morbidity association.

numyears <- length(unique(data_ver1519_HUAP$year))
df <- 4*numyears
spl <- ns(data_ver1519_HUAP$time, df)
model <- glm(Col01 ~ spl + ns(t_mean, 4), data=data_ver1519_HUAP, family=quasipoisson)
summary(model)

coefficients <- coef(model)
pred_data <- data.frame(ns(mean(data_ver1519_HUAP$time), df), ns(data_ver1519_HUAP$t_mean, 4))
linear_predictor <- as.vector(as.matrix(pred_data)%*%coefficients[-1]) + coefficients[1]
predicted <- exp(linear_predictor)

# Generate RR centered at MMT.
mmt <- data_ver1519_HUAP$t_mean[which.min(predicted)]
data_ver1519_HUAP$rr <- predicted/predicted[mmt]
data_ver1519_HUAP2 <- data_ver1519_HUAP[order(data_ver1519_HUAP$t_mean),]
plot(data_ver1519_HUAP2$t_mean, data_ver1519_HUAP2$rr, type="l", col="red", lwd=3,
     ylab="Relative Risk", xlab="Temperature (�C)")
abline(h=1)
title("Relative Risk of ED Consults during summer 2015-2018, HUAP, Santiago")

# Using the onebasis function.
basis <- onebasis(data_ver1519_HUAP$t_mean, fun="ns", df=4)
model <- glm(Col01 ~ spl + basis, data=data_ver1519_HUAP, family=quasipoisson)
mmt <- findmin(basis, model)
pred <- crosspred(basis, model, cen=mmt, by=.1)
plot(pred)
title("Single Lag Exposure Relative Risk of ED Consults during summer 2009-2014, HUAP, Santiago")

####################################################################################
### Distributed lag exposure-response relationship
####################################################################################

# Crossbasis for temperature.
maxlag <- 7
varknots <- quantile(data_ver1519_HUAP$t_mean, c(10,75,95)/100, na.rm=T)
lagknots <- logknots(maxlag, 3) 
cbasis <- crossbasis(data_ver1519_HUAP$t_mean, lag=maxlag, 
                     argvar=list(fun="ns", knots=varknots), 
                     arglag=list(fun="ns", knots=lagknots))
summary(cbasis)

# Fit quasi-poisson regression model.
model <- glm(Col01 ~ spl + dow + cbasis, data=data_ver1519_HUAP, family=quasipoisson)

# Get predictions centered at the MMT.
mmt <- findmin(cbasis, model)
pred <- crosspred(cbasis, model, cen=mmt, by=1)

# Plot exposure-lag-response risk surface.
plot3d <- plot(pred, shade=0.05, col="white",
               xlab="Temperature (�C)", zlab="Relative Risk", ylab="Lag")

# Lag-exposure curve at temperature 12�C and 25�C
quantile(data_ver1519_HUAP$t_mean, c(2.5,97.5)/100, na.rm=T)
lines(trans3d(x= 12, y=0:maxlag, z=pred$matRRfit["12",],  pmat=plot3d), lwd=3, col="blue")
lines(trans3d(x= 25, y=0:maxlag, z=pred$matRRfit["25",], pmat=plot3d), lwd=3, col="red")

# Figure for lag-response curves for cold and heat temperatures.
par(mex=0.8,mfrow=c(1,2))

# Cold.
plot(pred, var=12, type="l", ci="area", lwd=2, col="blue", 
     xlim=c(0,maxlag), ylim=c(0.85,1.2), 
     main="Lag-response at 12�C", xlab="Lag", ylab="Relative Risk")

# Heat.
plot(pred, var=25, type="l", ci="area", lwd=2, col="red", 
     xlim=c(0,maxlag), ylim=c(0.85,1.2), 
     main="Lag-response at 25�C", xlab="Lag", ylab="Relative Risk")

mtext("Lag Response to the minimum and maximum T (�C) for summer 2015-2019, HUAP", 
      outer = TRUE, cex = 1.5, line = -1)

# Close figure.
layout(1)

# Figure for exposure-response curves at short and long-lags.
par(mex=0.8,mfrow=c(2,1))

# Lag 0.
plot(pred, lag=0, type="l", ci="area", lwd=2, col="black", 
     xlim=c(0,maxlag), ylim=c(0.85,1.2), 
     main="Exposure-response at lag 0", xlab="Lag", ylab="Relative Risk")

# One-week lag.
plot(pred, lag=7, type="l", ci="area", lwd=2, col="black", 
     xlim=c(0,maxlag), ylim=c(0.85,1.2), 
     main="Exposure-response at lag 7", xlab="Lag", ylab="Relative Risk")

# Close figure.
layout(1)

# Overall cumulative exposure-response curve.
plot(pred, "overall", lwd=2, col=1,
     main="Overall cumulative exposure-response",
     xlab="Temperature (�C)", ylab="Relative Risk")
abline(v=c(mmt,12,25), lty=(c(2,1,2)), lwd=c(1.2,1,1.2))
box()

# Extract the cold and heat risk estimates.
# Cold.
with(pred, cbind(allRRfit, allRRlow, allRRhigh))["12",]
# Heat.
with(pred, cbind(allRRfit, allRRlow, allRRhigh))["25",] 

# Extreme Heat recentered at 90th percentile.
quantile(data_ver1519_HUAP$t_mean, c(90,99)/100, na.rm=T)
pred.heat <- crosspred(cbasis, model, cen=26, by=1)
with(pred.heat, cbind(allRRfit, allRRlow, allRRhigh))["25",]

####################################################################################
### Sensitivity analysis using quadratic B-splines for temperature. 
####################################################################################

# Model goodnes of fit, qAIC.
qAIC(model, type="dev")

# New crossbasis for temperature.
varknots2 <- equalknots(data_ver1519_HUAP$t_mean, nk=2) 
cbasis2 <- crossbasis(data_ver1519_HUAP$t_mean, lag=maxlag, 
                     argvar=list(fun="bs", degree=2, knots=varknots2),
                     arglag=list(fun="ns", knots=lagknots))
summary(cbasis2)

# Fit new quasi-poisson regression model.
model2 <- glm(Col01 ~ spl + dow + cbasis2, data=data_ver1519_HUAP, family=quasipoisson)

# Get predictions centered at the MMT.
mmt2 <- findmin(cbasis2, model2)
pred2 <- crosspred(cbasis2, model2, cen=mmt2, by=1)

# Overall cumulative exposure-response curve.
plot(pred2, "overall", lwd=2, col=1,
     main="Overall cumulative exposure-response",
     xlab="Temperature (�C)", ylab="Relative Risk")
abline(v=c(12,mmt2,25), lty=(c(2,1,2)), lwd=c(1.2,1,1.2))
box()

# Heat risk estimate.
with(pred2, cbind(allRRfit, allRRlow, allRRhigh))["25",] 

# Model goodness of fit, qAIC.
qAIC(model2, type="dev")

####################################################################################
### Short-term association between temperature and morbidity
####################################################################################

# Temperature-morbidity association.
numyears <- length(unique(data_ver1519_HUAP$year))
df <- 4*numyears
spl <- ns(data_ver1519_HUAP$time, df)

# Crossbasis for temperature.
maxlag <- 7
varknots <- quantile(data_ver1519_HUAP$t_mean, c(10,75,95)/100, na.rm=T)
lagknots <- logknots(maxlag, 3) 

cbasis <- crossbasis(data_ver1519_HUAP$t_mean, lag=maxlag, 
                     argvar=list(fun="ns", knots=varknots), 
                     arglag=list(fun="ns", knots=lagknots))
summary(cbasis)

# Fit quasi-Poisson regression model.
model <- glm(Col01 ~ spl + dow + cbasis, data=data_ver1519_HUAP, family=quasipoisson)

# Get predictions centered at the MMT.
mmt <- findmin(cbasis, model)
pred <- crosspred(cbasis, model, cen=mmt, by=1)

# Overall cumulative exposure-response curve.
plot(pred, "overall", lwd=2, col=1,
     main="Overall cumulative exposure-response",
     xlab="Temperature (�C)", ylab="Relative Risk")
abline(v=c(mmt,12,25), lty=(c(2,1,2)), lwd=c(1.2,1,1.2))
box()

# Heat risk estimate.
with(pred, cbind(allRRfit, allRRlow, allRRhigh))["25",] 

####################################################################################
### Calculating heat-attributable risk
####################################################################################

# Defining maximum temperature for heat exposure.
max <- max(data_ver1519_HUAP$t_mean, na.rm=T)

# Heat-attributable fraction backward perspective.
attrdl(data_ver1519_HUAP$t_mean, cbasis, data_ver1519_HUAP$Col01, model, cen=mmt, dir="back", range=c(mmt,max))

# Heat-attributable fraction forward perspective.
attrdl(data_ver1519_HUAP$t_mean, cbasis, data_ver1519_HUAP$Col01, model, cen=mmt, dir="forw", range=c(mmt,max))

# Heat-attributable consults backward perspective. 
attrdl(data_ver1519_HUAP$t_mean, cbasis, data_ver1519_HUAP$Col01, model, cen=mmt, dir="back", range=c(mmt,max),
       type="an")

# 95% empirical confidence interval backward perspective.
quantile(attrdl(data_ver1519_HUAP$t_mean, cbasis, data_ver1519_HUAP$Col01, model, cen=mmt, dir="back", 
                range=c(mmt,max), type="an", sim=T,nsim=1000),c(0.025,0.975))

# Heat-attributable ED visits forward perspective. 
attrdl(data_ver1519_HUAP$t_mean, cbasis, data_ver1519_HUAP$Col01, model, cen=mmt, dir="forw", range=c(mmt,max),
       type="an")

# 95% empirical confidence interval forward perspective.
quantile(attrdl(data_ver1519_HUAP$t_mean, cbasis, data_ver1519_HUAP$Col01, model, cen=mmt, dir="forw", 
                range=c(mmt,max), type="an", sim=T,nsim=1000),c(0.025,0.975))

####################################################################################
### Calculating contributions to heat-attributable risk
####################################################################################

# Attributable risk and deaths due to moderate heat.
p95 <- quantile(data_ver1519_HUAP$t_mean, 95/100, na.rm=T)
attrdl(data_ver1519_HUAP$t_mean, cbasis, data_ver1519_HUAP$Col01, model, cen=mmt, dir="forw", range=c(mmt,p95))
attrdl(data_ver1519_HUAP$t_mean, cbasis, data_ver1519_HUAP$Col01, model, cen=mmt, dir="forw", range=c(mmt,p95), type="an")
quantile(attrdl(data_ver1519_HUAP$t_mean, cbasis, data_ver1519_HUAP$Col01, model, cen=mmt, dir="forw", range=c(mmt,p95), type="an", 
                sim=T,nsim=1000),c(0.025,0.975))

# Attributable risk and deaths due to extreme heat.
attrdl(data_ver1519_HUAP$t_mean, cbasis, data_ver1519_HUAP$Col01, model, cen=mmt, dir="forw", range=c(p95,max))
attrdl(data_ver1519_HUAP$t_mean, cbasis, data_ver1519_HUAP$Col01, model, cen=mmt, dir="forw", range=c(p95,max), type="an")
quantile(attrdl(data_ver1519_HUAP$t_mean, cbasis, data_ver1519_HUAP$Col01, model, cen=mmt, dir="forw", range=c(p95,max), type="an", 
                sim=T,nsim=1000),c(0.025,0.975))



