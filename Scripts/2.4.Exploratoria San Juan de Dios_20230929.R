#### EXPLORATORIA DE BASE SAN JUAN DE DIOS #####
#Recordar que se puede usar pacman p_load(librería1, librería2)
library(pacman)
p_load (lubridate, ggplot2, ggpubr)


#Cargar Bases de datos
load("Out/SetHospitales.RData")

#Análisis de datos que genera tabla en excel y gráficos 
sapply(HSJD, class)
HSJD$ano<- year(HSJD$fecha2)
HSJD$mes<- month(HSJD$fecha2)
HSJD$dow<- wday(HSJD$fecha2) #dia 1 es domingo

t1<-table(HSJD$ano, HSJD$IdCausa)
write.csv(t1,"Tablas/HSJD_expl.csv")

serie<-function(causa, titulo)
{
  temp<-subset(HSJD, IdCausa==causa)
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

causa1<- serie("1", "San Juan de Dios - Total causas")
causa2<- serie("2", "San Juan de Dios - Respiratorias")
causa3<- serie("12", "San Juan de Dios - Circulatorias")
causa4<- serie("18", "San Juan de Dios - Traumatismos y envenenamiento")
causa5<- serie("21", "San Juan de Dios - Otras causas")

ggexport(causa1,causa2,causa3, causa4, causa5, 
         filename = "Fig/HSJD_causas.pdf",
         nrow = 5, ncol = 1)