#### ESTUDIO TEMPERATURA PARA CONCEPCI�N 2009-2018 ####
t1_CCP<-read.csv("~/OneDrive - Universidad de Chile/Doctorado/2023/Proyecto Olas de Calor/CS_XXXX_Temperatura_.csv", sep = ";", skip = 1, header = FALSE)
sapply(t1_CCP, class)

t1_CCP$fecha2<-substr(t1_CCP$V2, start = 1, stop = 10) # Para extraer la subcadena desde el primer al decimo caracter.
t1_CCP$fecha2<-as.Date(t1_CCP$fecha2, "%d-%m-%Y")
class(t1_CCP$fecha2)

#cambio a formato fecha-hora
t1_CCP$fechahora<-strptime(t1_CCP$V2, "%d-%m-%Y %H:%M:%S")
class(t1_CCP$fechahora)

#crea variable a�o lo que me permite evaluar la cantidad de datos presentes para cada a�o
t1_CCP$year<-year(t1_CCP$fecha2)
data.frame(table(t1_CCP$year))

save(t1_CCP, file="Out/Temperatura_CCP_6623.RData")

#Me permitir� trabajar con la visualizaci�n de datos del periodo completo y subsetear para el periodo de estudio
library(lubridate)
library(ggplot2)
library(gridExtra)
library(ggpubr)

#Selecciona temperaturas para el a�o
t1_CCP_0918<-subset(t1_CCP, year>2008 & year<2019, select=c(V2, V3, fecha2, year))

data.frame(table(t1_CCP_0918$year))

save(t1_CCP_0918, file="Out/PeriodoCCP_2009_2018.RData")

histog<-function(data, ano, titulo, color)
{
  year<-subset(data, year==ano)
  
  t1<- length(year[,2])
  t2<- sum(is.na(year[,2]))
  t3<- median(year[,2], na.rm = TRUE)
  t4<- min(year[,2], na.rm = TRUE)
  t5<- max(year[,2], na.rm = TRUE)
  
  ggplot(data = year, aes(x = V3)) +
    geom_histogram(bins = 25, col="black", fill=color, alpha=0.4) +
    xlab("Temperaturas horarias Concepci�n (�C)") +
    ylab("Frecuencia") +
    labs(title = titulo,
         subtitle = c(paste0("n=",t1,";", " NAs=",t2,"; P50 (min;max): ",t3," (",t4, "; ", t5, ")") ))+
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9),
    )
}

h1<-histog(t1_CCP_0918, 2018, "2018", "green")
h2<-histog(t1_CCP_0918, 2017, "2017", "green")
h3<-histog(t1_CCP_0918, 2016, "2016", "green")
h4<-histog(t1_CCP_0918, 2015, "2015", "green")
h5<-histog(t1_CCP_0918, 2014, "2014", "green")
h6<-histog(t1_CCP_0918, 2013, "2013", "green")
h7<-histog(t1_CCP_0918, 2012, "2012", "green")
h8<-histog(t1_CCP_0918, 2011, "2011", "green")
h9<-histog(t1_CCP_0918, 2010, "2010", "green")
h10<-histog(t1_CCP_0918, 2009, "2009", "green")


ggexport(h1,h2,h3, h4, h5, h6, h7, h8, h9, h10,
         filename = "Fig/Figura7.pdf",
         nrow = 3, ncol = 2)

p1<-ggplot(t1_CCP_0918, aes(x = fecha2, y = V3))+
  geom_point(colour= "blue", size=0.5)+
  xlab("Fecha") +
  ylab("Temperatura (�C)") +
  ggtitle("Temperaturas horarias Concepci�n 2009-2018") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 year") +
  geom_vline(xintercept = as.Date(c("2010-01-01","2011-01-01", "2012-01-01", "2013-01-01",
                                    "2014-01-01","2015-01-01", "2016-01-01", "2017-01-01",
                                    "2018-01-01")),
             linetype = "dashed", lwd=1) +
  theme_bw()
p1

ggexport(p1, filename = "Fig/Figura8.pdf",
         nrow = 1, ncol = 1)
rm(list=ls())