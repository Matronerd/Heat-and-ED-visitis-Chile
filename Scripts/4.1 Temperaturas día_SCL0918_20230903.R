####### Resumir la base de datos con datos diarios SCL######
load("Out/Periodo_2009_2018.RData")
library(tidyverse)

#valores diarios
t_dia_SCL_0918<- t1_SCL_0918%>% 
  group_by(fecha2) %>%
  summarise(n = n(), t_mean = mean(V3), t_max = max(V3),
            t_min = min(V3))

#variable año
t_dia_SCL_0918$year<-year(t_dia_SCL_0918$fecha2)
data.frame(table(t_dia_SCL_0918$year))

#variable mes
t_dia_SCL_0918$month<-month(t_dia_SCL_0918$fecha2)
data.frame(table(t_dia_SCL_0918$month))

#variable dia de la semana
t_dia_SCL_0918$dow<-wday(t_dia_SCL_0918$fecha2)
data.frame(table(t_dia_SCL_0918$dow))

save(t_dia_SCL_0918, file="Out/t_dia_SCL_0918.RData")

rm(list=ls())