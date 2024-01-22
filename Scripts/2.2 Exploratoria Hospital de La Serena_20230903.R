#### EXPLORATORIA DE BASE LA SERENA #####
#Recordar que se puede usar pacman p_load(librería1, librería2)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(ggplot2)

#Cargar Bases de datos
load("Out/SetHospitales.RData")

#Análisis de datos que genera tabla en excel y gráficos 
sapply(HSere, class)
HSere$ano<- year(HSere$fecha2)
HSere$mes<- month(HSere$fecha2)
HSere$dow<- wday(HSere$fecha2) #dia 1 es domingo

t1<-table(HSere$ano, HSere$IdCausa)
write.csv(t1,"Tablas/HSere_expl.csv")

serie<-function(causa, titulo)
{
  temp<-subset(HSere, IdCausa==causa)
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

causa1<- serie("1", "Hospital de La Serena-Total causas")
causa2<- serie("2", "Hospital de La Serena-Respiratorias")
causa3<- serie("12", "Hospital de La Serena-Circulatorias")
causa4<- serie("18", "Hospital de La Serena-Traumatismos y envenenamiento")
causa5<- serie("21", "Hospital de La Serena-Otras causas")

ggexport(causa1,causa2,causa3, causa4, causa5, 
         filename = "Fig/HSere_causas.pdf",
         nrow = 5, ncol = 1)

