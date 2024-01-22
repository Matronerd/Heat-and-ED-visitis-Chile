#### EXPLORATORIA DE BASE LA SERENA #####
#Recordar que se puede usar pacman p_load(librería1, librería2)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(ggplot2)

#Cargar Bases de datos
load("Out/SetHospitales.RData")

sapply(HConce, class)
HConce$ano<- year(HConce$fecha2)
HConce$mes<- month(HConce$fecha2)
HConce$dow<- wday(HConce$fecha2) #dia 1 es domingo

t1<-table(HConce$ano, HConce$IdCausa)
write.csv(t1,"Tablas/HConce_expl.csv")

serie<-function(causa, titulo)
{
  temp<-subset(HConce, IdCausa==causa)
  ggplot(data=temp, aes(x = fecha2, y = Col01))+
    geom_point(colour= "blue", size=0.5)+
    xlab("Fecha") +
    ylab("Consultas diarias") +
    ggtitle(titulo) +
    scale_x_date(date_labels = "%m/%Y", date_breaks = "1 year") +
    geom_vline(xintercept = as.Date(c("2009-01-01","2010-01-01", "2011-01-01", "2012-01-01",
                                      "2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01",
                                      "2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01",
                                      "2021-01-01")),
               linetype = "dashed", lwd=1) +
    theme_bw() +
    theme(axis.text.x = element_text(size=6, vjust = 0.5))
}

causa1<- serie("1", "Hospital de Concepción-Total causas")
causa2<- serie("2", "Hospital de Concepción-Respiratorias")
causa3<- serie("12", "Hospital de Concepción-Circulatorias")
causa4<- serie("18", "Hospital de Concepción-Traumatismos y envenenamiento")
causa5<- serie("21", "Hospital de Concepción-Otras causas")

ggexport(causa1,causa2,causa3, causa4, causa5, 
         filename = "Fig/HConce_causas.pdf",
         nrow = 5, ncol = 1)
