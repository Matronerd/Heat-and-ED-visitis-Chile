#### EXPLORATORIA DE BASE SAN JOS� #####
#Recordar que se puede usar pacman p_load(librer�a1, librer�a2)
library(pacman)
p_load (lubridate, ggplot2, ggpubr)


#Cargar Bases de datos
load("Out/SetHospitales.RData")

#An�lisis de datos que genera tabla en excel y gr�ficos 
sapply(HSJ, class)
HSJ$ano<- year(HSJ$fecha2)
HSJ$mes<- month(HSJ$fecha2)
HSJ$dow<- wday(HSJ$fecha2) #dia 1 es domingo

t1<-table(HSJ$ano, HSJ$IdCausa)
write.csv(t1,"Tablas/HSJ_expl.csv")

serie<-function(causa, titulo)
{
  temp<-subset(HSJ, IdCausa==causa)
  ggplot(data=temp, aes(x = fecha2, y = Col01))+
    geom_point(colour= "blue", size=0.5)+
    xlab("Fecha") +
    ylab("Consultas diarias") +
    ggtitle(titulo) +
    scale_x_date(date_labels = "%m/%Y", date_breaks = "1 year") +
    geom_vline(xintercept = as.Date(c("2009-01-01","2010-01-01", "2011-01-01", "2012-01-01",
                                      "2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01",
                                      "2017-01-01", "2018-01-01")),
               linetype = "dashed", lwd=1) +
    theme_bw() +
    theme(axis.text.x = element_text(size=6, vjust = 0.5))
}

causa1<- serie("1", "San Jos� - Total causas")
causa2<- serie("2", "San Jos� - Respiratorias")
causa3<- serie("12", "San Jos� - Circulatorias")
causa4<- serie("18", "San Jos� - Traumatismos y envenenamiento")
causa5<- serie("21", "San Jos� - Otras causas")

ggexport(causa1,causa2,causa3, causa4, causa5, 
         filename = "Fig/HSJ_causas.pdf",
         nrow = 5, ncol = 1)