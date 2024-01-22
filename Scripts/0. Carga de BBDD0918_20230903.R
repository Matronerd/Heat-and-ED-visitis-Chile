#Package
#install.packages("RODBC")
#install.packages("odbc")
#install.packages("DBI")
#install.packages("dplyr")
#install.packages("lifecycle")

#Bibliotecas
library(RODBC) ##hace conexión con driver antiguos microsoft
library(odbc)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)

#Cargar BBDD sin modificaciones obtenidas entre 2008-2018
load("BBDD_Hosp_20230723.RData")
#Omitir 2008 por mala calidad de datos, recordar que esta base tiene todos los datos.

#Cargar datos después de 2018  -------------------------------------------------
#Para ampliar el periodo se puede utilizar el siguiente código, pero debe correr en MS
#Corre solamente en computador MS y permite obtener el canal de comunicaciÃ³n entre la BBDD debe cargarse 2019 y 2020 sde esta manera 
# Como la BBDD estÃ¡ en MS ACCESS se define el path al archivo creando el data frame que contendrÃ¡ las tablas
#data <- file.path("/Users/macamartinez/Library/CloudStorage/OneDrive-UniversidaddeChile/Doctorado/BBDD Atenciones de Urgencia/AtencionesUrgencia2019.mdb")
#Crea el canal que permite extraer los datos con SQL
#channel <- odbcConnectAccess2007(data)
#reading the individual files inside the Main #se iterÃ³ el procedimiento para cada una de las BBDD
#table19<- sqlFetch(channel,"C:/Users/Usuario/OneDrive - Universidad San Sebastian/2022/Doctorado/BBDD Atenciones de Urgencia/AtencionesUrgencia2019.mdb")
#Se removiÃ³ data del environment para cada oportunidad que se iteraron las bbdd
#rm(data)
#Importar con el formato de fecha para evitar problemas despuÃ©s.
# BBDD2021 <- read_delim("~/Library/CloudStorage/OneDrive-UniversidaddeChile/Doctorado/2023/Proyecto Olas de Calor/BBDD Atenciones de Urgencia/AtencionesUrgencia2021.csv", delim = ";", escape_double = FALSE, col_types = cols(fecha = col_datetime(format = " %a %b %d %H:%M:%S GMT%z %Y")), trim_ws = TRUE)
# BBDD2022 <- read_delim("~/Library/CloudStorage/OneDrive-UniversidaddeChile/Doctorado/2023/Proyecto Olas de Calor/BBDD Atenciones de Urgencia/AtencionesUrgencia2022.csv", delim = ";", escape_double = FALSE, col_types = cols(fecha = col_datetime(format = " %a %b %d %H:%M:%S GMT%z %Y")), trim_ws = TRUE)

rm(BBDD2008, BBDD2009, BBDD2009_small, BBDD2010, BBDD2010_small, BBDD2011, BBDD2011_small, BBDD2012,
   BBDD2012_small, BBDD2013, BBDD2014, BBDD2015, BBDD2016, BBDD2017, BBDD2018)

#Exploratoria base 2009  -------------------------------------------------
names(table09)
data.frame(table(table09$IdEstablecimiento))
data.frame(table(table09$NEstablecimiento))
data.frame(table(table09$IdCausa))
data.frame(table(table09$GlosaCausa))
causas<-data.frame(table(table09$GlosaCausa, table09$IdCausa))
sapply(table09, class)#fecha es caracter
data.frame(table(table09$semana))
data.frame(table(table09$GLOSATIPOESTABLECIMIENTO))
data.frame(table(table09$GLOSATIPOATENCION))
data.frame(table(table09$GlosaTipoCampana))

estab<-data.frame(table(table09$NEstablecimiento, table09$IdEstablecimiento))
#Complejo Hospitalario San José (Santiago, Independencia)  09-100 (hay 26 observaciones con id 09-100--, 2 espacios)
#Hospital Clínico de Niños Dr. Roberto del Río (Santiago, Independencia)  09-101
#Hospital San Juan de Dios (Santiago, Santiago)  10-100
#Hospital de Urgencia Asistencia Pública Dr. Alejandro del Río (Santiago, Santiago) 11-195
#Hospital de Niños Dr. Luis Calvo Mackenna (Santiago, Providencia) 12-102
#Hospital Clínico Regional Dr. Guillermo Grant Benavente (Concepción) 18-100
#Hospital San Juan de Dios (La Serena) 05-100

#Manejo fechas
table09$fecha2<-substr(table09$fecha, start = 1, stop = 10)    # Para extraer la subcadena desde el primer al decimo caracter.
table09$fecha2<-as.Date(table09$fecha2, "%Y-%m-%d")
class(table09$fecha2)

Graficos<-function(estab, causa, titulo)
{  
  q<-subset(table09, IdEstablecimiento==estab & IdCausa==causa, select=c(IdEstablecimiento,
                                                                         NEstablecimiento,
                                                                         IdCausa,
                                                                         GlosaCausa,
                                                                         Col01,
                                                                         fecha2))
  
  ggplot(q, aes(x = fecha2, y = Col01))+
    geom_point(colour= "purple", size=1)+
    xlab("Año 2009") +
    ylab("N°consultas diarias totales") +
    ggtitle(titulo) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_bw()                                                                       
  
}

#SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA
p1<-Graficos("09-100", "1", "HSJ-Total")
p2<-Graficos("09-101", "1", "HRRIO-Total")
p3<-Graficos("10-100", "1", "HSJD-Total")
p4<-Graficos("11-195", "1", "Posta central-Total")
p5<-Graficos("12-102", "1", "HLCM-Total")
p6<-Graficos("18-100", "1", "HConce-Total")
p7<-Graficos("05-100", "1", "HSere-Total")
p8<-Graficos("09-100  ", "1", "HSJ-Total(c)")

#TOTAL CAUSAS SISTEMA RESPIRATORIO (2)
p9<-Graficos("09-100", "2", "HSJ-Total_Resp")
p10<-Graficos("09-101", "2", "HRRIO-Total_Resp")
p11<-Graficos("10-100", "2", "HSJD-Total_Resp")
p12<-Graficos("11-195", "2", "Posta central-Total_Resp")
p13<-Graficos("12-102", "2", "HLCM-Total_Resp")
p14<-Graficos("18-100", "2", "HConce-Total_Resp")
p15<-Graficos("05-100", "2", "HSere-Total_Resp")
p16<-Graficos("09-100  ", "2", "HSJ-Total_Resp (c)")

#CAUSAS SISTEMA RESPIRATORIO (7)
p17<-Graficos("09-100", "7", "HSJ-Total_circ")
p18<-Graficos("09-101", "7", "HRRIO-Total_circ")
p19<-Graficos("10-100", "7", "HSJD-Total_circ")
p20<-Graficos("11-195", "7", "Posta central-Total_circ")
p21<-Graficos("12-102", "7", "HLCM-Total_circ")
p22<-Graficos("18-100", "7", "HConce-Total_circ")
p23<-Graficos("05-100", "7", "HSere-Total_circ")
p24<-Graficos("09-100  ", "7", "HSJ-Total_circ (c)")

#TOTAL CAUSAS SISTEMA CIRCULATORIO (12)
p25<-Graficos("09-100", "12", "HSJ-Circ")
p26<-Graficos("09-101", "12", "HRRIO-Circ")
p27<-Graficos("10-100", "12", "HSJD-Circ")
p28<-Graficos("11-195", "12", "Posta central-Circ")
p29<-Graficos("12-102", "12", "HLCM-Circ")
p30<-Graficos("18-100", "12", "HConce-Circ")
p31<-Graficos("05-100", "12", "HSere-Circ")
p32<-Graficos("09-100  ", "12", "HSJ-Circ (c)")

#TOTAL TRAUMATISMOS Y ENVENENAMIENTO (18)
p33<-Graficos("09-100", "18", "HSJ-Total_traum")
p34<-Graficos("09-101", "18", "HRRIO-Total_traum")
p35<-Graficos("10-100", "18", "HSJD-Total_traum")
p36<-Graficos("11-195", "18", "Posta central-Total_traum")
p37<-Graficos("12-102", "18", "HLCM-Total_traum")
p38<-Graficos("18-100", "18", "HConce-Total_traum")
p39<-Graficos("05-100", "18", "HSere-Total_traum")
p40<-Graficos("09-100  ", "18", "HSJ-Total_traum (c)")

#TOTAL DEMÁS CAUSAS (21)
p41<-Graficos("09-100", "21", "HSJ-Demas")
p42<-Graficos("09-101", "21", "HRRIO-Demas")
p43<-Graficos("10-100", "21", "HSJD-Demas")
p44<-Graficos("11-195", "21", "Posta central-Demas")
p45<-Graficos("12-102", "21", "HLCM-Demas")
p46<-Graficos("18-100", "21", "HConce-Demas")
p47<-Graficos("05-100", "21", "HSere-Demas")
p48<-Graficos("09-100  ", "21", "HSJ-Demas (c)")

#bases de datos con 8086 observaciones y 11 variables/cada causa esta repetida 311 veces ¿311 dias?
## Acá se crearon BBDD para CONCE: 9362, HLCM: 9464, HRRIO: 9464, HSere: 5018, HSJ: 9412, HSJD: 9464
save(HSJ2009, HRRIO2009, HSJD2009, PCentral2009, HLCM2009, HConce2009, HSere2009, file="Out/Set2009.RData")
rm(list=ls()) #Limpia environment completo 

# #Exploratoria base 2010 ---------
names(table10)
data.frame(table(table10$IdEstablecimiento))
data.frame(table(table10$NEstablecimiento))
data.frame(table(table10$IdCausa))
data.frame(table(table10$GlosaCausa))
causas<-data.frame(table(table10$GlosaCausa, table10$IdCausa))
sapply(table10, class)#fecha es caracter
data.frame(table(table10$semana))
data.frame(table(table10$GLOSATIPOESTABLECIMIENTO))
data.frame(table(table10$GLOSATIPOATENCION))
data.frame(table(table10$GlosaTipoCampana))

estab<-data.frame(table(table10$NEstablecimiento, table10$IdEstablecimiento))
#Complejo Hospitalario San José (Santiago, Independencia)  09-100 
#Hospital Clínico de Niños Dr. Roberto del Río (Santiago, Independencia)  09-101 (hay 27 observaciones con id 09-101--, 2 espacios)
#Hospital San Juan de Dios (Santiago, Santiago)  10-100 (hay 27 observaciones con id 10-100--, 2 espacios)
#Hospital de Urgencia Asistencia Pública Dr. Alejandro del Río (Santiago, Santiago) 11-195 (hay 53 observaciones con id 11-195--, 2 espacios)
#Hospital de Niños Dr. Luis Calvo Mackenna (Santiago, Providencia) 12-102 (hay 26 observaciones con id 12-102--, 2 espacios)
#Hospital Clínico Regional Dr. Guillermo Grant Benavente (Concepción) 18-100
#Hospital San Juan de Dios (La Serena) 05-100

#Manejo fechas
table10$fecha2<-substr(table10$fecha, start = 1, stop = 10)    # Para extraer la subcadena desde el primer al decimo caracter.
table10$fecha2<-as.Date(table10$fecha2, "%Y-%m-%d")
class(table10$fecha2)

Graficos<-function(estab, causa, titulo)
{  
  q<-subset(table10, IdEstablecimiento==estab & IdCausa==causa, select=c(IdEstablecimiento,
                                                                         NEstablecimiento,
                                                                         IdCausa,
                                                                         GlosaCausa,
                                                                         Col01,
                                                                         fecha2))
  
  ggplot(q, aes(x = fecha2, y = Col01))+
    geom_point(colour= "magenta", size=1)+
    xlab("Año 2009") +
    ylab("N°consultas diarias totales") +
    ggtitle(titulo) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_bw()                                                                       
  
}

#SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA
p1<-Graficos("09-100", "1", "HSJ-Total")
p2<-Graficos("09-101", "1", "HRRIO-Total")
p3<-Graficos("10-100", "1", "HSJD-Total")
p4<-Graficos("11-195", "1", "Posta central-Total")
p5<-Graficos("12-102", "1", "HLCM-Total")
p6<-Graficos("18-100", "1", "HConce-Total")
p7<-Graficos("05-100", "1", "HSere-Total")
p8<-Graficos("09-101  ", "1", "HRRIO-Total(c)")

#TOTAL CAUSAS SISTEMA RESPIRATORIO (2)
p9<-Graficos("09-100", "2", "HSJ-Total_Resp")
p10<-Graficos("09-101", "2", "HRRIO-Total_Resp")
p11<-Graficos("10-100", "2", "HSJD-Total_Resp")
p12<-Graficos("11-195", "2", "Posta central-Total_Resp")
p13<-Graficos("12-102", "2", "HLCM-Total_Resp")
p14<-Graficos("18-100", "2", "HConce-Total_Resp")
p15<-Graficos("05-100", "2", "HSere-Total_Resp")
p16<-Graficos("09-101  ", "2", "HRRIO-Total_Resp (c)")

#CAUSAS SISTEMA RESPIRATORIO (7)
p17<-Graficos("09-100", "7", "HSJ-Total_circ")
p18<-Graficos("09-101", "7", "HRRIO-Total_circ")
p19<-Graficos("10-100", "7", "HSJD-Total_circ")
p20<-Graficos("11-195", "7", "Posta central-Total_circ")
p21<-Graficos("12-102", "7", "HLCM-Total_circ")
p22<-Graficos("18-100", "7", "HConce-Total_circ")
p23<-Graficos("05-100", "7", "HSere-Total_circ")
p24<-Graficos("09-101  ", "7", "HRRIO-Total_circ (c)")

#TOTAL CAUSAS SISTEMA CIRCULATORIO (12)
p25<-Graficos("09-100", "12", "HSJ-Circ")
p26<-Graficos("09-101", "12", "HRRIO-Circ")
p27<-Graficos("10-100", "12", "HSJD-Circ")
p28<-Graficos("11-195", "12", "Posta central-Circ")
p29<-Graficos("12-102", "12", "HLCM-Circ")
p30<-Graficos("18-100", "12", "HConce-Circ")
p31<-Graficos("05-100", "12", "HSere-Circ")
p32<-Graficos("09-101  ", "12", "HRRIO-Circ (c)")

#TOTAL TRAUMATISMOS Y ENVENENAMIENTO (18)
p33<-Graficos("09-100", "18", "HSJ-Total_traum")
p34<-Graficos("09-101", "18", "HRRIO-Total_traum")
p35<-Graficos("10-100", "18", "HSJD-Total_traum")
p36<-Graficos("11-195", "18", "Posta central-Total_traum")
p37<-Graficos("12-102", "18", "HLCM-Total_traum")
p38<-Graficos("18-100", "18", "HConce-Total_traum")
p39<-Graficos("05-100", "18", "HSere-Total_traum")
p40<-Graficos("09-101  ", "18", "HRRIO-Total_traum (c)")

#TOTAL DEMÁS CAUSAS (21)
p41<-Graficos("09-100", "21", "HSJ-Demas")
p42<-Graficos("09-101", "21", "HRRIO-Demas")
p43<-Graficos("10-100", "21", "HSJD-Demas")
p44<-Graficos("11-195", "21", "Posta central-Demas")
p45<-Graficos("12-102", "21", "HLCM-Demas")
p46<-Graficos("18-100", "21", "HConce-Demas")
p47<-Graficos("05-100", "21", "HSere-Demas")
p48<-Graficos("09-101  ", "21", "HRRIO-Demas (c)")

ggexport(p1, p2, p3, p4, p5, p6, p7, p8,
         p9, p10, p11, p12, p13, p14, p15, p16,
         p17, p18, p19, p20, p21, p22, p23, p24,
         p25, p26, p27, p28, p29, p30, p31, p32,
         p33, p34, p35, p36, p37, p38, p39, p40,
         p41, p42, p43, p44, p45, p46, p47, p48,
         filename = "Fig/UE2010.pdf",
         nrow = 4, ncol = 2)

HSJ2010<-subset(table10, IdEstablecimiento=="09-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HRRIO2010<-subset(table10, IdEstablecimiento=="09-101", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSJD2010<-subset(table10, IdEstablecimiento=="10-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
PCentral2010<-subset(table10, IdEstablecimiento=="11-195", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HLCM2010<-subset(table10, IdEstablecimiento=="12-102", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HConce2010<-subset(table10, IdEstablecimiento=="18-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSere2010<-subset(table10, IdEstablecimiento=="05-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))

save(HSJ2010, HRRIO2010, HSJD2010, PCentral2010, HLCM2010, HConce2010, HSere2010, file="Out/Set2010.RData")

rm(list=ls())

#Exploratoria base 2011
load("~/OneDrive - Universidad de Chile/Doctorado/2023/Proyecto Olas de Calor/Limpieza de datos/BBDD_Hosp_20230723.RData")
names(table11)
data.frame(table(table11$IdEstablecimiento))
data.frame(table(table11$NEstablecimiento))
data.frame(table(table11$IdCausa))
data.frame(table(table11$GlosaCausa))
causas<-data.frame(table(table11$GlosaCausa, table11$IdCausa))
sapply(table11, class)#fecha es caracter
data.frame(table(table11$semana))
data.frame(table(table11$GLOSATIPOESTABLECIMIENTO))
data.frame(table(table11$GLOSATIPOATENCION))
data.frame(table(table11$GlosaTipoCampana))

estab<-data.frame(table(table11$NEstablecimiento, table11$IdEstablecimiento))
#Complejo Hospitalario San José (Santiago, Independencia)  09-100 
#Hospital Clínico de Niños Dr. Roberto del Río (Santiago, Independencia)  09-101 
#Hospital San Juan de Dios (Santiago, Santiago)  10-100 
#Hospital de Urgencia Asistencia Pública Dr. Alejandro del Río (Santiago, Santiago) 11-195 
#Hospital de Niños Dr. Luis Calvo Mackenna (Santiago, Providencia) 12-102 (hay 27 observaciones con id 12-102--, 2 espacios)
#Hospital Clínico Regional Dr. Guillermo Grant Benavente (Concepción) 18-100
#Hospital San Juan de Dios (La Serena) 05-100

#Manejo fechas
table11$fecha2<-substr(table11$fecha, start = 1, stop = 10)    # Para extraer la subcadena desde el primer al decimo caracter.
table11$fecha2<-as.Date(table11$fecha2, "%Y-%m-%d")
class(table11$fecha2)

Graficos<-function(estab, causa, titulo)
{  
  q<-subset(table11, IdEstablecimiento==estab & IdCausa==causa, select=c(IdEstablecimiento,
                                                                         NEstablecimiento,
                                                                         IdCausa,
                                                                         GlosaCausa,
                                                                         Col01,
                                                                         fecha2))
  
  ggplot(q, aes(x = fecha2, y = Col01))+
    geom_point(colour= "purple", size=1)+
    xlab("Año 2011") +
    ylab("N°consultas diarias totales") +
    ggtitle(titulo) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_bw()                                                                       
  
}


#SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA
p1<-Graficos("09-100", "1", "HSJ-Total")
p2<-Graficos("09-101", "1", "HRRIO-Total")
p3<-Graficos("10-100", "1", "HSJD-Total")
p4<-Graficos("11-195", "1", "Posta central-Total")
p5<-Graficos("12-102", "1", "HLCM-Total")
p6<-Graficos("18-100", "1", "HConce-Total")
p7<-Graficos("05-100", "1", "HSere-Total")
p8<-Graficos("12-102  ", "1", "HLCM-Total(c)")

#TOTAL CAUSAS SISTEMA RESPIRATORIO (2)
p9<-Graficos("09-100", "2", "HSJ-Total_Resp")
p10<-Graficos("09-101", "2", "HRRIO-Total_Resp")
p11<-Graficos("10-100", "2", "HSJD-Total_Resp")
p12<-Graficos("11-195", "2", "Posta central-Total_Resp")
p13<-Graficos("12-102", "2", "HLCM-Total_Resp")
p14<-Graficos("18-100", "2", "HConce-Total_Resp")
p15<-Graficos("05-100", "2", "HSere-Total_Resp")
p16<-Graficos("12-102  ", "2", "HLCM-Total_Resp (c)")

#CAUSAS SISTEMA RESPIRATORIO (7)
p17<-Graficos("09-100", "7", "HSJ-Total_circ")
p18<-Graficos("09-101", "7", "HRRIO-Total_circ")
p19<-Graficos("10-100", "7", "HSJD-Total_circ")
p20<-Graficos("11-195", "7", "Posta central-Total_circ")
p21<-Graficos("12-102", "7", "HLCM-Total_circ")
p22<-Graficos("18-100", "7", "HConce-Total_circ")
p23<-Graficos("05-100", "7", "HSere-Total_circ")
p24<-Graficos("12-102  ", "7", "HLCM-Total_circ (c)")

#TOTAL CAUSAS SISTEMA CIRCULATORIO (12)
p25<-Graficos("09-100", "12", "HSJ-Circ")
p26<-Graficos("09-101", "12", "HRRIO-Circ")
p27<-Graficos("10-100", "12", "HSJD-Circ")
p28<-Graficos("11-195", "12", "Posta central-Circ")
p29<-Graficos("12-102", "12", "HLCM-Circ")
p30<-Graficos("18-100", "12", "HConce-Circ")
p31<-Graficos("05-100", "12", "HSere-Circ")
p32<-Graficos("12-102  ", "12", "HLCM-Circ (c)")

#TOTAL TRAUMATISMOS Y ENVENENAMIENTO (18)
p33<-Graficos("09-100", "18", "HSJ-Total_traum")
p34<-Graficos("09-101", "18", "HRRIO-Total_traum")
p35<-Graficos("10-100", "18", "HSJD-Total_traum")
p36<-Graficos("11-195", "18", "Posta central-Total_traum")
p37<-Graficos("12-102", "18", "HLCM-Total_traum")
p38<-Graficos("18-100", "18", "HConce-Total_traum")
p39<-Graficos("05-100", "18", "HSere-Total_traum")
p40<-Graficos("12-102  ", "18", "HLCM-Total_traum (c)")

#TOTAL DEMÁS CAUSAS (21)
p41<-Graficos("09-100", "21", "HSJ-Demas")
p42<-Graficos("09-101", "21", "HRRIO-Demas")
p43<-Graficos("10-100", "21", "HSJD-Demas")
p44<-Graficos("11-195", "21", "Posta central-Demas")
p45<-Graficos("12-102", "21", "HLCM-Demas")
p46<-Graficos("18-100", "21", "HConce-Demas")
p47<-Graficos("05-100", "21", "HSere-Demas")
p48<-Graficos("12-102  ", "21", "HLCM-Demas (c)")

ggexport(p1, p2, p3, p4, p5, p6, p7, p8,
         p9, p10, p11, p12, p13, p14, p15, p16,
         p17, p18, p19, p20, p21, p22, p23, p24,
         p25, p26, p27, p28, p29, p30, p31, p32,
         p33, p34, p35, p36, p37, p38, p39, p40,
         p41, p42, p43, p44, p45, p46, p47, p48,
         filename = "Fig/UE2011.pdf",
         nrow = 4, ncol = 2)


HSJ2011<-subset(table11, IdEstablecimiento=="09-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HRRIO2011<-subset(table11, IdEstablecimiento=="09-101", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSJD2011<-subset(table11, IdEstablecimiento=="10-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
PCentral2011<-subset(table11, IdEstablecimiento=="11-195", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HLCM2011<-subset(table11, IdEstablecimiento=="12-102", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HConce2011<-subset(table11, IdEstablecimiento=="18-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSere2011<-subset(table11, IdEstablecimiento=="05-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))

save(HSJ2011, HRRIO2011, HSJD2011, PCentral2011, HLCM2011, HConce2011, HSere2011, file="Out/Set2011.RData")
rm(list=ls())

#Exploratoria base 2012
load("~/OneDrive - Universidad de Chile/Doctorado/2023/Proyecto Olas de Calor/Limpieza de datos/BBDD_Hosp_20230723.RData")
names(table12)
data.frame(table(table12$IdEstablecimiento))
data.frame(table(table12$NEstablecimiento))
data.frame(table(table12$IdCausa))
data.frame(table(table12$GlosaCausa))
causas<-data.frame(table(table12$GlosaCausa, table12$IdCausa))
sapply(table12, class)#fecha es caracter
data.frame(table(table12$semana))
data.frame(table(table12$GLOSATIPOESTABLECIMIENTO))
data.frame(table(table12$GLOSATIPOATENCION))
data.frame(table(table12$GlosaTipoCampana))

estab<-data.frame(table(table12$NEstablecimiento, table12$IdEstablecimiento))
#Complejo Hospitalario San José (Santiago, Independencia)  09-100 
#Hospital Clínico de Niños Dr. Roberto del Río (Santiago, Independencia)  09-101 
#Hospital San Juan de Dios (Santiago, Santiago)  10-100 
#Hospital de Urgencia Asistencia Pública Dr. Alejandro del Río (Santiago, Santiago) 11-195 
#Hospital de Niños Dr. Luis Calvo Mackenna (Santiago, Providencia) 12-102 
#Hospital Clínico Regional Dr. Guillermo Grant Benavente (Concepción) 18-100
#Hospital San Juan de Dios (La Serena) 05-100

#Manejo fechas
table12$fecha2<-substr(table12$fecha, start = 1, stop = 10)    # Para extraer la subcadena desde el primer al decimo caracter.
table12$fecha2<-as.Date(table12$fecha2, "%Y-%m-%d")
class(table12$fecha2)

Graficos<-function(estab, causa, titulo)
{  
  q<-subset(table12, IdEstablecimiento==estab & IdCausa==causa, select=c(IdEstablecimiento,
                                                                         NEstablecimiento,
                                                                         IdCausa,
                                                                         GlosaCausa,
                                                                         Col01,
                                                                         fecha2))
  
  ggplot(q, aes(x = fecha2, y = Col01))+
    geom_point(colour= "purple", size=1)+
    xlab("Año 2012") +
    ylab("N°consultas diarias totales") +
    ggtitle(titulo) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_bw()                                                                       
  
}


#SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA
p1<-Graficos("09-100", "1", "HSJ-Total")
p2<-Graficos("09-101", "1", "HRRIO-Total")
p3<-Graficos("10-100", "1", "HSJD-Total")
p4<-Graficos("11-195", "1", "Posta central-Total")
p5<-Graficos("12-102", "1", "HLCM-Total")
p6<-Graficos("18-100", "1", "HConce-Total")
p7<-Graficos("05-100", "1", "HSere-Total")
p8<-Graficos("12-102  ", "1", "HLCM-Total(c)")

#TOTAL CAUSAS SISTEMA RESPIRATORIO (2)
p9<-Graficos("09-100", "2", "HSJ-Total_Resp")
p10<-Graficos("09-101", "2", "HRRIO-Total_Resp")
p11<-Graficos("10-100", "2", "HSJD-Total_Resp")
p12<-Graficos("11-195", "2", "Posta central-Total_Resp")
p13<-Graficos("12-102", "2", "HLCM-Total_Resp")
p14<-Graficos("18-100", "2", "HConce-Total_Resp")
p15<-Graficos("05-100", "2", "HSere-Total_Resp")
p16<-Graficos("12-102  ", "2", "HLCM-Total_Resp (c)")

#CAUSAS SISTEMA RESPIRATORIO (7)
p17<-Graficos("09-100", "7", "HSJ-Total_circ")
p18<-Graficos("09-101", "7", "HRRIO-Total_circ")
p19<-Graficos("10-100", "7", "HSJD-Total_circ")
p20<-Graficos("11-195", "7", "Posta central-Total_circ")
p21<-Graficos("12-102", "7", "HLCM-Total_circ")
p22<-Graficos("18-100", "7", "HConce-Total_circ")
p23<-Graficos("05-100", "7", "HSere-Total_circ")
p24<-Graficos("12-102  ", "7", "HLCM-Total_circ (c)")

#TOTAL CAUSAS SISTEMA CIRCULATORIO (12)
p25<-Graficos("09-100", "12", "HSJ-Circ")
p26<-Graficos("09-101", "12", "HRRIO-Circ")
p27<-Graficos("10-100", "12", "HSJD-Circ")
p28<-Graficos("11-195", "12", "Posta central-Circ")
p29<-Graficos("12-102", "12", "HLCM-Circ")
p30<-Graficos("18-100", "12", "HConce-Circ")
p31<-Graficos("05-100", "12", "HSere-Circ")
p32<-Graficos("12-102  ", "12", "HLCM-Circ (c)")

#TOTAL TRAUMATISMOS Y ENVENENAMIENTO (18)
p33<-Graficos("09-100", "18", "HSJ-Total_traum")
p34<-Graficos("09-101", "18", "HRRIO-Total_traum")
p35<-Graficos("10-100", "18", "HSJD-Total_traum")
p36<-Graficos("11-195", "18", "Posta central-Total_traum")
p37<-Graficos("12-102", "18", "HLCM-Total_traum")
p38<-Graficos("18-100", "18", "HConce-Total_traum")
p39<-Graficos("05-100", "18", "HSere-Total_traum")
p40<-Graficos("12-102  ", "18", "HLCM-Total_traum (c)")

#TOTAL DEMÁS CAUSAS (21)
p41<-Graficos("09-100", "21", "HSJ-Demas")
p42<-Graficos("09-101", "21", "HRRIO-Demas")
p43<-Graficos("10-100", "21", "HSJD-Demas")
p44<-Graficos("11-195", "21", "Posta central-Demas")
p45<-Graficos("12-102", "21", "HLCM-Demas")
p46<-Graficos("18-100", "21", "HConce-Demas")
p47<-Graficos("05-100", "21", "HSere-Demas")
p48<-Graficos("12-102  ", "21", "HLCM-Demas (c)")

ggexport(p1, p2, p3, p4, p5, p6, p7, p8,
         p9, p10, p11, p12, p13, p14, p15, p16,
         p17, p18, p19, p20, p21, p22, p23, p24,
         p25, p26, p27, p28, p29, p30, p31, p32,
         p33, p34, p35, p36, p37, p38, p39, p40,
         p41, p42, p43, p44, p45, p46, p47, p48,
         filename = "Fig/UE2012.pdf",
         nrow = 4, ncol = 2)


HSJ2012<-subset(table12, IdEstablecimiento=="09-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HRRIO2012<-subset(table12, IdEstablecimiento=="09-101", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSJD2012<-subset(table12, IdEstablecimiento=="10-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
PCentral2012<-subset(table12, IdEstablecimiento=="11-195", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HLCM2012<-subset(table12, IdEstablecimiento=="12-102", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HConce2012<-subset(table12, IdEstablecimiento=="18-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSere2012<-subset(table12, IdEstablecimiento=="05-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))

save(HSJ2012, HRRIO2012, HSJD2012, PCentral2012, HLCM2012, HConce2012, HSere2012, file="Out/Set2012.RData")
rm(list=ls())

#Exploratoria base 2013 ----
load("~/OneDrive - Universidad de Chile/Doctorado/2023/Proyecto Olas de Calor/Limpieza de datos/BBDD_Hosp_20230723.RData")
names(table13)
#cambio nombres
names (table13)[5] = "Col01"
names (table13)[6] = "Col02"
names (table13)[7] = "Col03"
names (table13)[8] = "Col04"
names (table13)[9] = "Col05"
names (table13)[10] = "Col06"
names(table13)
data.frame(table(table13$IdEstablecimiento))
data.frame(table(table13$NEstablecimiento))
data.frame(table(table13$IdCausa))
data.frame(table(table13$GlosaCausa))
causas<-data.frame(table(table13$GlosaCausa, table13$IdCausa))
sapply(table13, class)#fecha es caracter
data.frame(table(table13$semana))
data.frame(table(table13$GLOSATIPOESTABLECIMIENTO))
data.frame(table(table13$GLOSATIPOATENCION))
data.frame(table(table13$GlosaTipoCampana))

estab<-data.frame(table(table13$NEstablecimiento, table13$IdEstablecimiento))
#Complejo Hospitalario San José (Santiago, Independencia)  09-100 
#Hospital Clínico de Niños Dr. Roberto del Río (Santiago, Independencia)  09-101 
#Hospital San Juan de Dios (Santiago, Santiago)  10-100 
#Hospital de Urgencia Asistencia Pública Dr. Alejandro del Río (Santiago, Santiago) 11-195 
#Hospital de Niños Dr. Luis Calvo Mackenna (Santiago, Providencia) 12-102 
#Hospital Clínico Regional Dr. Guillermo Grant Benavente (Concepción) 18-100
#Hospital San Juan de Dios (La Serena) 05-100

#Manejo fechas
table13$fecha2<-substr(table13$fecha, start = 1, stop = 10)    # Para extraer la subcadena desde el primer al decimo caracter.
table13$fecha2<-as.Date(table13$fecha2, "%Y-%m-%d")
class(table13$fecha2)

Graficos<-function(estab, causa, titulo)
{  
  q<-subset(table13, IdEstablecimiento==estab & IdCausa==causa, select=c(IdEstablecimiento,
                                                                         NEstablecimiento,
                                                                         IdCausa,
                                                                         GlosaCausa,
                                                                         Col01,
                                                                         fecha2))
  
  ggplot(q, aes(x = fecha2, y = Col01))+
    geom_point(colour= "purple", size=1)+
    xlab("Año 2013") +
    ylab("N°consultas diarias totales") +
    ggtitle(titulo) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_bw()                                                                       
  
}


#SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA
p1<-Graficos("09-100", "1", "HSJ-Total")
p2<-Graficos("09-101", "1", "HRRIO-Total")
p3<-Graficos("10-100", "1", "HSJD-Total")
p4<-Graficos("11-195", "1", "Posta central-Total")
p5<-Graficos("12-102", "1", "HLCM-Total")
p6<-Graficos("18-100", "1", "HConce-Total")
p7<-Graficos("05-100", "1", "HSere-Total")
p8<-Graficos("12-102  ", "1", "HLCM-Total(c)")

#TOTAL CAUSAS SISTEMA RESPIRATORIO (2)
p9<-Graficos("09-100", "2", "HSJ-Total_Resp")
p10<-Graficos("09-101", "2", "HRRIO-Total_Resp")
p11<-Graficos("10-100", "2", "HSJD-Total_Resp")
p12<-Graficos("11-195", "2", "Posta central-Total_Resp")
p13<-Graficos("12-102", "2", "HLCM-Total_Resp")
p14<-Graficos("18-100", "2", "HConce-Total_Resp")
p15<-Graficos("05-100", "2", "HSere-Total_Resp")
p16<-Graficos("12-102  ", "2", "HLCM-Total_Resp (c)")

#CAUSAS SISTEMA RESPIRATORIO (7)
p17<-Graficos("09-100", "7", "HSJ-Total_circ")
p18<-Graficos("09-101", "7", "HRRIO-Total_circ")
p19<-Graficos("10-100", "7", "HSJD-Total_circ")
p20<-Graficos("11-195", "7", "Posta central-Total_circ")
p21<-Graficos("12-102", "7", "HLCM-Total_circ")
p22<-Graficos("18-100", "7", "HConce-Total_circ")
p23<-Graficos("05-100", "7", "HSere-Total_circ")
p24<-Graficos("12-102  ", "7", "HLCM-Total_circ (c)")

#TOTAL CAUSAS SISTEMA CIRCULATORIO (12)
p25<-Graficos("09-100", "12", "HSJ-Circ")
p26<-Graficos("09-101", "12", "HRRIO-Circ")
p27<-Graficos("10-100", "12", "HSJD-Circ")
p28<-Graficos("11-195", "12", "Posta central-Circ")
p29<-Graficos("12-102", "12", "HLCM-Circ")
p30<-Graficos("18-100", "12", "HConce-Circ")
p31<-Graficos("05-100", "12", "HSere-Circ")
p32<-Graficos("12-102  ", "12", "HLCM-Circ (c)")

#TOTAL TRAUMATISMOS Y ENVENENAMIENTO (18)
p33<-Graficos("09-100", "18", "HSJ-Total_traum")
p34<-Graficos("09-101", "18", "HRRIO-Total_traum")
p35<-Graficos("10-100", "18", "HSJD-Total_traum")
p36<-Graficos("11-195", "18", "Posta central-Total_traum")
p37<-Graficos("12-102", "18", "HLCM-Total_traum")
p38<-Graficos("18-100", "18", "HConce-Total_traum")
p39<-Graficos("05-100", "18", "HSere-Total_traum")
p40<-Graficos("12-102  ", "18", "HLCM-Total_traum (c)")

#TOTAL DEMÁS CAUSAS (21)
p41<-Graficos("09-100", "21", "HSJ-Demas")
p42<-Graficos("09-101", "21", "HRRIO-Demas")
p43<-Graficos("10-100", "21", "HSJD-Demas")
p44<-Graficos("11-195", "21", "Posta central-Demas")
p45<-Graficos("12-102", "21", "HLCM-Demas")
p46<-Graficos("18-100", "21", "HConce-Demas")
p47<-Graficos("05-100", "21", "HSere-Demas")
p48<-Graficos("12-102  ", "21", "HLCM-Demas (c)")

ggexport(p1, p2, p3, p4, p5, p6, p7, p8,
         p9, p10, p11, p12, p13, p14, p15, p16,
         p17, p18, p19, p20, p21, p22, p23, p24,
         p25, p26, p27, p28, p29, p30, p31, p32,
         p33, p34, p35, p36, p37, p38, p39, p40,
         p41, p42, p43, p44, p45, p46, p47, p48,
         filename = "Fig/UE2013.pdf",
         nrow = 4, ncol = 2)

HSJ2013<-subset(table13, IdEstablecimiento=="09-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HRRIO2013<-subset(table13, IdEstablecimiento=="09-101", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSJD2013<-subset(table13, IdEstablecimiento=="10-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
PCentral2013<-subset(table13, IdEstablecimiento=="11-195", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HLCM2013<-subset(table13, IdEstablecimiento=="12-102", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HConce2013<-subset(table13, IdEstablecimiento=="18-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSere2013<-subset(table13, IdEstablecimiento=="05-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))

save(HSJ2013, HRRIO2013, HSJD2013, PCentral2013, HLCM2013, HConce2013, HSere2013, file="Out/Set2013.RData")
rm(list=ls())

#Exploratoria base 2014 ------
load("~/OneDrive - Universidad de Chile/Doctorado/2023/Proyecto Olas de Calor/Limpieza de datos/BBDD_Hosp_20230723.RData")
names(table14)
#cambio nombres
names (table14)[5] = "Col01"
names (table14)[6] = "Col02"
names (table14)[7] = "Col03"
names (table14)[8] = "Col04"
names (table14)[9] = "Col05"
names (table14)[10] = "Col06"
names(table14)
data.frame(table(table14$IdEstablecimiento))
data.frame(table(table14$NEstablecimiento))
data.frame(table(table14$IdCausa))
data.frame(table(table14$GlosaCausa))
causas<-data.frame(table(table14$GlosaCausa, table14$IdCausa))
sapply(table14, class)#fecha es caracter
data.frame(table(table14$semana))
data.frame(table(table14$GLOSATIPOESTABLECIMIENTO))
data.frame(table(table14$GLOSATIPOATENCION))
data.frame(table(table14$GlosaTipoCampana))

estab<-data.frame(table(table14$NEstablecimiento, table14$IdEstablecimiento))
#Complejo Hospitalario San José (Santiago, Independencia)  09-100 
#Hospital Clínico de Niños Dr. Roberto del Río (Santiago, Independencia)  09-101 
#Hospital San Juan de Dios (Santiago, Santiago)  10-100 
#Hospital de Urgencia Asistencia Pública Dr. Alejandro del Río (Santiago, Santiago) 11-195 
#Hospital de Niños Dr. Luis Calvo Mackenna (Santiago, Providencia) 12-102 
#Hospital Clínico Regional Dr. Guillermo Grant Benavente (Concepción) 18-100
#Hospital San Juan de Dios (La Serena) 05-100

#Manejo fechas
table14$fecha2<-substr(table14$fecha, start = 1, stop = 10)    # Para extraer la subcadena desde el primer al decimo caracter.
table14$fecha2<-as.Date(table14$fecha2, "%Y-%m-%d")
class(table14$fecha2)

Graficos<-function(estab, causa, titulo)
{  
  q<-subset(table14, IdEstablecimiento==estab & IdCausa==causa, select=c(IdEstablecimiento,
                                                                         NEstablecimiento,
                                                                         IdCausa,
                                                                         GlosaCausa,
                                                                         Col01,
                                                                         fecha2))
  
  ggplot(q, aes(x = fecha2, y = Col01))+
    geom_point(colour= "purple", size=1)+
    xlab("Año 2014") +
    ylab("N°consultas diarias totales") +
    ggtitle(titulo) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_bw()                                                                       
  
}


#SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA
p1<-Graficos("09-100", "1", "HSJ-Total")
p2<-Graficos("09-101", "1", "HRRIO-Total")
p3<-Graficos("10-100", "1", "HSJD-Total")
p4<-Graficos("11-195", "1", "Posta central-Total")
p5<-Graficos("12-102", "1", "HLCM-Total")
p6<-Graficos("18-100", "1", "HConce-Total")
p7<-Graficos("05-100", "1", "HSere-Total")
p8<-Graficos("12-102  ", "1", "HLCM-Total(c)")

#TOTAL CAUSAS SISTEMA RESPIRATORIO (2)
p9<-Graficos("09-100", "2", "HSJ-Total_Resp")
p10<-Graficos("09-101", "2", "HRRIO-Total_Resp")
p11<-Graficos("10-100", "2", "HSJD-Total_Resp")
p12<-Graficos("11-195", "2", "Posta central-Total_Resp")
p13<-Graficos("12-102", "2", "HLCM-Total_Resp")
p14<-Graficos("18-100", "2", "HConce-Total_Resp")
p15<-Graficos("05-100", "2", "HSere-Total_Resp")
p16<-Graficos("12-102  ", "2", "HLCM-Total_Resp (c)")

#CAUSAS SISTEMA RESPIRATORIO (7)
p17<-Graficos("09-100", "7", "HSJ-Total_circ")
p18<-Graficos("09-101", "7", "HRRIO-Total_circ")
p19<-Graficos("10-100", "7", "HSJD-Total_circ")
p20<-Graficos("11-195", "7", "Posta central-Total_circ")
p21<-Graficos("12-102", "7", "HLCM-Total_circ")
p22<-Graficos("18-100", "7", "HConce-Total_circ")
p23<-Graficos("05-100", "7", "HSere-Total_circ")
p24<-Graficos("12-102  ", "7", "HLCM-Total_circ (c)")

#TOTAL CAUSAS SISTEMA CIRCULATORIO (12)
p25<-Graficos("09-100", "12", "HSJ-Circ")
p26<-Graficos("09-101", "12", "HRRIO-Circ")
p27<-Graficos("10-100", "12", "HSJD-Circ")
p28<-Graficos("11-195", "12", "Posta central-Circ")
p29<-Graficos("12-102", "12", "HLCM-Circ")
p30<-Graficos("18-100", "12", "HConce-Circ")
p31<-Graficos("05-100", "12", "HSere-Circ")
p32<-Graficos("12-102  ", "12", "HLCM-Circ (c)")

#TOTAL TRAUMATISMOS Y ENVENENAMIENTO (18)
p33<-Graficos("09-100", "18", "HSJ-Total_traum")
p34<-Graficos("09-101", "18", "HRRIO-Total_traum")
p35<-Graficos("10-100", "18", "HSJD-Total_traum")
p36<-Graficos("11-195", "18", "Posta central-Total_traum")
p37<-Graficos("12-102", "18", "HLCM-Total_traum")
p38<-Graficos("18-100", "18", "HConce-Total_traum")
p39<-Graficos("05-100", "18", "HSere-Total_traum")
p40<-Graficos("12-102  ", "18", "HLCM-Total_traum (c)")

#TOTAL DEMÁS CAUSAS (21)
p41<-Graficos("09-100", "21", "HSJ-Demas")
p42<-Graficos("09-101", "21", "HRRIO-Demas")
p43<-Graficos("10-100", "21", "HSJD-Demas")
p44<-Graficos("11-195", "21", "Posta central-Demas")
p45<-Graficos("12-102", "21", "HLCM-Demas")
p46<-Graficos("18-100", "21", "HConce-Demas")
p47<-Graficos("05-100", "21", "HSere-Demas")
p48<-Graficos("12-102  ", "21", "HLCM-Demas (c)")

ggexport(p1, p2, p3, p4, p5, p6, p7, p8,
         p9, p10, p11, p12, p13, p14, p15, p16,
         p17, p18, p19, p20, p21, p22, p23, p24,
         p25, p26, p27, p28, p29, p30, p31, p32,
         p33, p34, p35, p36, p37, p38, p39, p40,
         p41, p42, p43, p44, p45, p46, p47, p48,
         filename = "Fig/UE2014.pdf",
         nrow = 4, ncol = 2)

HSJ2014<-subset(table14, IdEstablecimiento=="09-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HRRIO2014<-subset(table14, IdEstablecimiento=="09-101", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSJD2014<-subset(table14, IdEstablecimiento=="10-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
PCentral2014<-subset(table14, IdEstablecimiento=="11-195", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HLCM2014<-subset(table14, IdEstablecimiento=="12-102", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HConce2014<-subset(table14, IdEstablecimiento=="18-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSere2014<-subset(table14, IdEstablecimiento=="05-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))

save(HSJ2014, HRRIO2014, HSJD2014, PCentral2014, HLCM2014, HConce2014, HSere2014, file="Out/Set2014.RData")
rm(list=ls())

#Exploratoria base 2015 ------
load("~/OneDrive - Universidad de Chile/Doctorado/2023/Proyecto Olas de Calor/Limpieza de datos/BBDD_Hosp_20230723.RData")
names(table15)
#cambio nombres
names (table15)[5] = "Col01"
names (table15)[6] = "Col02"
names (table15)[7] = "Col03"
names (table15)[8] = "Col04"
names (table15)[9] = "Col05"
names (table15)[10] = "Col06"
names(table15)
data.frame(table(table15$IdEstablecimiento))
data.frame(table(table15$NEstablecimiento))
data.frame(table(table15$IdCausa))
data.frame(table(table15$GlosaCausa))
causas<-data.frame(table(table15$GlosaCausa, table15$IdCausa))
sapply(table15, class)#fecha es caracter
data.frame(table(table15$semana))
data.frame(table(table15$GLOSATIPOESTABLECIMIENTO))
data.frame(table(table15$GLOSATIPOATENCION))
data.frame(table(table15$GlosaTipoCampana))

estab<-data.frame(table(table15$NEstablecimiento, table15$IdEstablecimiento))
#Complejo Hospitalario San José (Santiago, Independencia)  09-100 
#Hospital Clínico de Niños Dr. Roberto del Río (Santiago, Independencia)  09-101 
#Hospital San Juan de Dios (Santiago, Santiago)  10-100 
#Hospital de Urgencia Asistencia Pública Dr. Alejandro del Río (Santiago, Santiago) 11-195 
#Hospital de Niños Dr. Luis Calvo Mackenna (Santiago, Providencia) 12-102 
#Hospital Clínico Regional Dr. Guillermo Grant Benavente (Concepción) 18-100
#Hospital San Juan de Dios (La Serena) 05-100

#Manejo fechas
table15$fecha2<-substr(table15$fecha, start = 1, stop = 10)    # Para extraer la subcadena desde el primer al decimo caracter.
table15$fecha2<-as.Date(table15$fecha2, "%d/%m/%Y")
class(table15$fecha2)

Graficos<-function(estab, causa, titulo)
{  
  q<-subset(table15, IdEstablecimiento==estab & IdCausa==causa, select=c(IdEstablecimiento,
                                                                         NEstablecimiento,
                                                                         IdCausa,
                                                                         GlosaCausa,
                                                                         Col01,
                                                                         fecha2))
  
  ggplot(q, aes(x = fecha2, y = Col01))+
    geom_point(colour= "purple", size=1)+
    xlab("Año 2015") +
    ylab("N°consultas diarias totales") +
    ggtitle(titulo) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_bw()                                                                       
  
}


#SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA
p1<-Graficos("09-100", "1", "HSJ-Total")
p2<-Graficos("09-101", "1", "HRRIO-Total")
p3<-Graficos("10-100", "1", "HSJD-Total")
p4<-Graficos("11-195", "1", "Posta central-Total")
p5<-Graficos("12-102", "1", "HLCM-Total")
p6<-Graficos("18-100", "1", "HConce-Total")
p7<-Graficos("05-100", "1", "HSere-Total")
p8<-Graficos("12-102  ", "1", "HLCM-Total(c)")

#TOTAL CAUSAS SISTEMA RESPIRATORIO (2)
p9<-Graficos("09-100", "2", "HSJ-Total_Resp")
p10<-Graficos("09-101", "2", "HRRIO-Total_Resp")
p11<-Graficos("10-100", "2", "HSJD-Total_Resp")
p12<-Graficos("11-195", "2", "Posta central-Total_Resp")
p13<-Graficos("12-102", "2", "HLCM-Total_Resp")
p14<-Graficos("18-100", "2", "HConce-Total_Resp")
p15<-Graficos("05-100", "2", "HSere-Total_Resp")
p16<-Graficos("12-102  ", "2", "HLCM-Total_Resp (c)")

#CAUSAS SISTEMA RESPIRATORIO (7)
p17<-Graficos("09-100", "7", "HSJ-Total_circ")
p18<-Graficos("09-101", "7", "HRRIO-Total_circ")
p19<-Graficos("10-100", "7", "HSJD-Total_circ")
p20<-Graficos("11-195", "7", "Posta central-Total_circ")
p21<-Graficos("12-102", "7", "HLCM-Total_circ")
p22<-Graficos("18-100", "7", "HConce-Total_circ")
p23<-Graficos("05-100", "7", "HSere-Total_circ")
p24<-Graficos("12-102  ", "7", "HLCM-Total_circ (c)")

#TOTAL CAUSAS SISTEMA CIRCULATORIO (12)
p25<-Graficos("09-100", "12", "HSJ-Circ")
p26<-Graficos("09-101", "12", "HRRIO-Circ")
p27<-Graficos("10-100", "12", "HSJD-Circ")
p28<-Graficos("11-195", "12", "Posta central-Circ")
p29<-Graficos("12-102", "12", "HLCM-Circ")
p30<-Graficos("18-100", "12", "HConce-Circ")
p31<-Graficos("05-100", "12", "HSere-Circ")
p32<-Graficos("12-102  ", "12", "HLCM-Circ (c)")

#TOTAL TRAUMATISMOS Y ENVENENAMIENTO (18)
p33<-Graficos("09-100", "18", "HSJ-Total_traum")
p34<-Graficos("09-101", "18", "HRRIO-Total_traum")
p35<-Graficos("10-100", "18", "HSJD-Total_traum")
p36<-Graficos("11-195", "18", "Posta central-Total_traum")
p37<-Graficos("12-102", "18", "HLCM-Total_traum")
p38<-Graficos("18-100", "18", "HConce-Total_traum")
p39<-Graficos("05-100", "18", "HSere-Total_traum")
p40<-Graficos("12-102  ", "18", "HLCM-Total_traum (c)")

#TOTAL DEMÁS CAUSAS (21)
p41<-Graficos("09-100", "21", "HSJ-Demas")
p42<-Graficos("09-101", "21", "HRRIO-Demas")
p43<-Graficos("10-100", "21", "HSJD-Demas")
p44<-Graficos("11-195", "21", "Posta central-Demas")
p45<-Graficos("12-102", "21", "HLCM-Demas")
p46<-Graficos("18-100", "21", "HConce-Demas")
p47<-Graficos("05-100", "21", "HSere-Demas")
p48<-Graficos("12-102  ", "21", "HLCM-Demas (c)")

ggexport(p1, p2, p3, p4, p5, p6, p7, p8,
         p9, p10, p11, p12, p13, p14, p15, p16,
         p17, p18, p19, p20, p21, p22, p23, p24,
         p25, p26, p27, p28, p29, p30, p31, p32,
         p33, p34, p35, p36, p37, p38, p39, p40,
         p41, p42, p43, p44, p45, p46, p47, p48,
         filename = "Fig/UE2015.pdf",
         nrow = 4, ncol = 2)

HSJ2015<-subset(table15, IdEstablecimiento=="09-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HRRIO2015<-subset(table15, IdEstablecimiento=="09-101", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSJD2015<-subset(table15, IdEstablecimiento=="10-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
PCentral2015<-subset(table15, IdEstablecimiento=="11-195", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HLCM2015<-subset(table15, IdEstablecimiento=="12-102", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HConce2015<-subset(table15, IdEstablecimiento=="18-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSere2015<-subset(table15, IdEstablecimiento=="05-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))

save(HSJ2015, HRRIO2015, HSJD2015, PCentral2015, HLCM2015, HConce2015, HSere2015, file="Out/Set2015.RData")
rm(list=ls())

#Exploratoria base 2016 ------
load("~/OneDrive - Universidad de Chile/Doctorado/2023/Proyecto Olas de Calor/Limpieza de datos/BBDD_Hosp_20230723.RData")
names(table16)
#cambio nombres
names (table16)[5] = "Col01"
names (table16)[6] = "Col02"
names (table16)[7] = "Col03"
names (table16)[8] = "Col04"
names (table16)[9] = "Col05"
names (table16)[10] = "Col06"
names(table16)
data.frame(table(table16$IdEstablecimiento))
data.frame(table(table16$NEstablecimiento))
data.frame(table(table16$IdCausa))
data.frame(table(table16$GlosaCausa))
causas<-data.frame(table(table16$GlosaCausa, table16$IdCausa))
sapply(table16, class)#fecha es caracter
data.frame(table(table16$semana))
data.frame(table(table16$GLOSATIPOESTABLECIMIENTO))
data.frame(table(table16$GLOSATIPOATENCION))
data.frame(table(table16$GlosaTipoCampana))

estab<-data.frame(table(table16$NEstablecimiento, table16$IdEstablecimiento))
#Complejo Hospitalario San José (Santiago, Independencia)  09-100 
#Hospital Clínico de Niños Dr. Roberto del Río (Santiago, Independencia)  09-101 
#Hospital San Juan de Dios (Santiago, Santiago)  10-100 
#Hospital de Urgencia Asistencia Pública Dr. Alejandro del Río (Santiago, Santiago) 11-195 
#Hospital de Niños Dr. Luis Calvo Mackenna (Santiago, Providencia) 12-102 
#Hospital Clínico Regional Dr. Guillermo Grant Benavente (Concepción) 18-100
#Hospital San Juan de Dios (La Serena) 05-100

#Manejo fechas
table16$fecha2<-substr(table16$fecha, start = 1, stop = 10)    # Para extraer la subcadena desde el primer al decimo caracter.
table16$fecha2<-as.Date(table16$fecha2, "%d/%m/%Y")
class(table16$fecha2)

Graficos<-function(estab, causa, titulo)
{  
  q<-subset(table16, IdEstablecimiento==estab & IdCausa==causa, select=c(IdEstablecimiento,
                                                                         NEstablecimiento,
                                                                         IdCausa,
                                                                         GlosaCausa,
                                                                         Col01,
                                                                         fecha2))
  
  ggplot(q, aes(x = fecha2, y = Col01))+
    geom_point(colour= "purple", size=1)+
    xlab("Año 2016") +
    ylab("N°consultas diarias totales") +
    ggtitle(titulo) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_bw()                                                                       
  
}


#SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA
p1<-Graficos("09-100", "1", "HSJ-Total")
p2<-Graficos("09-101", "1", "HRRIO-Total")
p3<-Graficos("10-100", "1", "HSJD-Total")
p4<-Graficos("11-195", "1", "Posta central-Total")
p5<-Graficos("12-102", "1", "HLCM-Total")
p6<-Graficos("18-100", "1", "HConce-Total")
p7<-Graficos("05-100", "1", "HSere-Total")
p8<-Graficos("12-102  ", "1", "HLCM-Total(c)")

#TOTAL CAUSAS SISTEMA RESPIRATORIO (2)
p9<-Graficos("09-100", "2", "HSJ-Total_Resp")
p10<-Graficos("09-101", "2", "HRRIO-Total_Resp")
p11<-Graficos("10-100", "2", "HSJD-Total_Resp")
p12<-Graficos("11-195", "2", "Posta central-Total_Resp")
p13<-Graficos("12-102", "2", "HLCM-Total_Resp")
p14<-Graficos("18-100", "2", "HConce-Total_Resp")
p15<-Graficos("05-100", "2", "HSere-Total_Resp")
p16<-Graficos("12-102  ", "2", "HLCM-Total_Resp (c)")

#CAUSAS SISTEMA RESPIRATORIO (7)
p17<-Graficos("09-100", "7", "HSJ-Total_circ")
p18<-Graficos("09-101", "7", "HRRIO-Total_circ")
p19<-Graficos("10-100", "7", "HSJD-Total_circ")
p20<-Graficos("11-195", "7", "Posta central-Total_circ")
p21<-Graficos("12-102", "7", "HLCM-Total_circ")
p22<-Graficos("18-100", "7", "HConce-Total_circ")
p23<-Graficos("05-100", "7", "HSere-Total_circ")
p24<-Graficos("12-102  ", "7", "HLCM-Total_circ (c)")

#TOTAL CAUSAS SISTEMA CIRCULATORIO (12)
p25<-Graficos("09-100", "12", "HSJ-Circ")
p26<-Graficos("09-101", "12", "HRRIO-Circ")
p27<-Graficos("10-100", "12", "HSJD-Circ")
p28<-Graficos("11-195", "12", "Posta central-Circ")
p29<-Graficos("12-102", "12", "HLCM-Circ")
p30<-Graficos("18-100", "12", "HConce-Circ")
p31<-Graficos("05-100", "12", "HSere-Circ")
p32<-Graficos("12-102  ", "12", "HLCM-Circ (c)")

#TOTAL TRAUMATISMOS Y ENVENENAMIENTO (18)
p33<-Graficos("09-100", "18", "HSJ-Total_traum")
p34<-Graficos("09-101", "18", "HRRIO-Total_traum")
p35<-Graficos("10-100", "18", "HSJD-Total_traum")
p36<-Graficos("11-195", "18", "Posta central-Total_traum")
p37<-Graficos("12-102", "18", "HLCM-Total_traum")
p38<-Graficos("18-100", "18", "HConce-Total_traum")
p39<-Graficos("05-100", "18", "HSere-Total_traum")
p40<-Graficos("12-102  ", "18", "HLCM-Total_traum (c)")

#TOTAL DEMÁS CAUSAS (21)
p41<-Graficos("09-100", "21", "HSJ-Demas")
p42<-Graficos("09-101", "21", "HRRIO-Demas")
p43<-Graficos("10-100", "21", "HSJD-Demas")
p44<-Graficos("11-195", "21", "Posta central-Demas")
p45<-Graficos("12-102", "21", "HLCM-Demas")
p46<-Graficos("18-100", "21", "HConce-Demas")
p47<-Graficos("05-100", "21", "HSere-Demas")
p48<-Graficos("12-102  ", "21", "HLCM-Demas (c)")

ggexport(p1, p2, p3, p4, p5, p6, p7, p8,
         p9, p10, p11, p12, p13, p14, p15, p16,
         p17, p18, p19, p20, p21, p22, p23, p24,
         p25, p26, p27, p28, p29, p30, p31, p32,
         p33, p34, p35, p36, p37, p38, p39, p40,
         p41, p42, p43, p44, p45, p46, p47, p48,
         filename = "Fig/UE2016.pdf",
         nrow = 4, ncol = 2)

HSJ2016<-subset(table16, IdEstablecimiento=="09-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HRRIO2016<-subset(table16, IdEstablecimiento=="09-101", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSJD2016<-subset(table16, IdEstablecimiento=="10-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
PCentral2016<-subset(table16, IdEstablecimiento=="11-195", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HLCM2016<-subset(table16, IdEstablecimiento=="12-102", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HConce2016<-subset(table16, IdEstablecimiento=="18-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSere2016<-subset(table16, IdEstablecimiento=="05-100", select=c(IdEstablecimiento, NEstablecimiento, IdCausa, GlosaCausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))

save(HSJ2016, HRRIO2016, HSJD2016, PCentral2016, HLCM2016, HConce2016, HSere2016, file="Out/Set2016.RData")
rm(list=ls())

#Exploratoria base 2017 ------
load("~/OneDrive - Universidad de Chile/Doctorado/2023/Proyecto Olas de Calor/Limpieza de datos/BBDD_Hosp_20230723.RData")
names(table17)
#cambio nombres
names (table17)[5] = "Col01"
names (table17)[6] = "Col02"
names (table17)[7] = "Col03"
names (table17)[8] = "Col04"
names (table17)[9] = "Col05"
names (table17)[10] = "Col06"
names(table17)
data.frame(table(table17$idestablecimiento))
data.frame(table(table17$nestablecimiento))
data.frame(table(table17$Idcausa))
data.frame(table(table17$glosacausa))
causas<-data.frame(table(table17$glosacausa, table17$Idcausa))
sapply(table17, class)#fecha es caracter
data.frame(table(table17$semana))
data.frame(table(table17$GLOSATIPOESTABLECIMIENTO))
data.frame(table(table17$GLOSATIPOATENCION))
data.frame(table(table17$GlosaTipoCampana))

estab<-data.frame(table(table17$nestablecimiento, table17$idestablecimiento))
#Complejo Hospitalario San José (Santiago, Independencia)  09-100 
#Hospital Clínico de Niños Dr. Roberto del Río (Santiago, Independencia)  09-101 
#Hospital San Juan de Dios (Santiago, Santiago)  10-100 
#Hospital de Urgencia Asistencia Pública Dr. Alejandro del Río (Santiago, Santiago) 11-195 
#Hospital de Niños Dr. Luis Calvo Mackenna (Santiago, Providencia) 12-102 
#Hospital Clínico Regional Dr. Guillermo Grant Benavente (Concepción) 18-100
#Hospital San Juan de Dios (La Serena) 05-100

#Manejo fechas
table17$fecha2<-substr(table17$fecha, start = 1, stop = 10)    # Para extraer la subcadena desde el primer al decimo caracter.
table17$fecha2<-as.Date(table17$fecha2, "%d/%m/%Y")
class(table17$fecha2)

Graficos<-function(estab, causa, titulo)
{  
  q<-subset(table17, idestablecimiento==estab & Idcausa==causa, select=c(idestablecimiento,
                                                                         nestablecimiento,
                                                                         Idcausa,
                                                                         glosacausa,
                                                                         Col01,
                                                                         fecha2))
  
  ggplot(q, aes(x = fecha2, y = Col01))+
    geom_point(colour= "purple", size=1)+
    xlab("Año 2017") +
    ylab("N°consultas diarias totales") +
    ggtitle(titulo) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_bw()                                                                       
  
}


#SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA
p1<-Graficos("09-100", "1", "HSJ-Total")
p2<-Graficos("09-101", "1", "HRRIO-Total")
p3<-Graficos("10-100", "1", "HSJD-Total")
p4<-Graficos("11-195", "1", "Posta central-Total")
p5<-Graficos("12-102", "1", "HLCM-Total")
p6<-Graficos("18-100", "1", "HConce-Total")
p7<-Graficos("05-100", "1", "HSere-Total")
p8<-Graficos("12-102  ", "1", "HLCM-Total(c)")

#TOTAL CAUSAS SISTEMA RESPIRATORIO (2)
p9<-Graficos("09-100", "2", "HSJ-Total_Resp")
p10<-Graficos("09-101", "2", "HRRIO-Total_Resp")
p11<-Graficos("10-100", "2", "HSJD-Total_Resp")
p12<-Graficos("11-195", "2", "Posta central-Total_Resp")
p13<-Graficos("12-102", "2", "HLCM-Total_Resp")
p14<-Graficos("18-100", "2", "HConce-Total_Resp")
p15<-Graficos("05-100", "2", "HSere-Total_Resp")
p16<-Graficos("12-102  ", "2", "HLCM-Total_Resp (c)")

#CAUSAS SISTEMA RESPIRATORIO (7)
p17<-Graficos("09-100", "7", "HSJ-Total_circ")
p18<-Graficos("09-101", "7", "HRRIO-Total_circ")
p19<-Graficos("10-100", "7", "HSJD-Total_circ")
p20<-Graficos("11-195", "7", "Posta central-Total_circ")
p21<-Graficos("12-102", "7", "HLCM-Total_circ")
p22<-Graficos("18-100", "7", "HConce-Total_circ")
p23<-Graficos("05-100", "7", "HSere-Total_circ")
p24<-Graficos("12-102  ", "7", "HLCM-Total_circ (c)")

#TOTAL CAUSAS SISTEMA CIRCULATORIO (12)
p25<-Graficos("09-100", "12", "HSJ-Circ")
p26<-Graficos("09-101", "12", "HRRIO-Circ")
p27<-Graficos("10-100", "12", "HSJD-Circ")
p28<-Graficos("11-195", "12", "Posta central-Circ")
p29<-Graficos("12-102", "12", "HLCM-Circ")
p30<-Graficos("18-100", "12", "HConce-Circ")
p31<-Graficos("05-100", "12", "HSere-Circ")
p32<-Graficos("12-102  ", "12", "HLCM-Circ (c)")

#TOTAL TRAUMATISMOS Y ENVENENAMIENTO (18)
p33<-Graficos("09-100", "18", "HSJ-Total_traum")
p34<-Graficos("09-101", "18", "HRRIO-Total_traum")
p35<-Graficos("10-100", "18", "HSJD-Total_traum")
p36<-Graficos("11-195", "18", "Posta central-Total_traum")
p37<-Graficos("12-102", "18", "HLCM-Total_traum")
p38<-Graficos("18-100", "18", "HConce-Total_traum")
p39<-Graficos("05-100", "18", "HSere-Total_traum")
p40<-Graficos("12-102  ", "18", "HLCM-Total_traum (c)")

#TOTAL DEMÁS CAUSAS (21)
p41<-Graficos("09-100", "21", "HSJ-Demas")
p42<-Graficos("09-101", "21", "HRRIO-Demas")
p43<-Graficos("10-100", "21", "HSJD-Demas")
p44<-Graficos("11-195", "21", "Posta central-Demas")
p45<-Graficos("12-102", "21", "HLCM-Demas")
p46<-Graficos("18-100", "21", "HConce-Demas")
p47<-Graficos("05-100", "21", "HSere-Demas")
p48<-Graficos("12-102  ", "21", "HLCM-Demas (c)")

ggexport(p1, p2, p3, p4, p5, p6, p7, p8,
         p9, p10, p11, p12, p13, p14, p15, p16,
         p17, p18, p19, p20, p21, p22, p23, p24,
         p25, p26, p27, p28, p29, p30, p31, p32,
         p33, p34, p35, p36, p37, p38, p39, p40,
         p41, p42, p43, p44, p45, p46, p47, p48,
         filename = "Fig/UE2017.pdf",
         nrow = 4, ncol = 2)

HSJ2017<-subset(table17, idestablecimiento=="09-100", select=c(idestablecimiento, nestablecimiento, Idcausa, glosacausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HRRIO2017<-subset(table17, idestablecimiento=="09-101", select=c(idestablecimiento, nestablecimiento, Idcausa, glosacausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSJD2017<-subset(table17, idestablecimiento=="10-100", select=c(idestablecimiento, nestablecimiento, Idcausa, glosacausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
PCentral2017<-subset(table17, idestablecimiento=="11-195", select=c(idestablecimiento, nestablecimiento, Idcausa, glosacausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HLCM2017<-subset(table17, idestablecimiento=="12-102", select=c(idestablecimiento, nestablecimiento, Idcausa, glosacausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HConce2017<-subset(table17, idestablecimiento=="18-100", select=c(idestablecimiento, nestablecimiento, Idcausa, glosacausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSere2017<-subset(table17, idestablecimiento=="05-100", select=c(idestablecimiento, nestablecimiento, Idcausa, glosacausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))

save(HSJ2017, HRRIO2017, HSJD2017, PCentral2017, HLCM2017, HConce2017, HSere2017, file="Out/Set2017.RData")
rm(list=ls())

#Exploratoria base 2018 ------
load("~/OneDrive - Universidad de Chile/Doctorado/2023/Proyecto Olas de Calor/Limpieza de datos/BBDD_Hosp_20230723.RData")
names(table18)
#cambio nombres
names (table18)[5] = "Col01"
names (table18)[6] = "Col02"
names (table18)[7] = "Col03"
names (table18)[8] = "Col04"
names (table18)[9] = "Col05"
names (table18)[10] = "Col06"
names(table18)
data.frame(table(table18$idestablecimiento))
data.frame(table(table18$nestablecimiento))
data.frame(table(table18$Idcausa))
data.frame(table(table18$glosacausa))
causas<-data.frame(table(table18$glosacausa, table18$Idcausa))
sapply(table18, class)#fecha es caracter
data.frame(table(table18$semana))
data.frame(table(table18$GLOSATIPOESTABLECIMIENTO))
data.frame(table(table18$GLOSATIPOATENCION))
data.frame(table(table18$GlosaTipoCampana))

estab<-data.frame(table(table18$nestablecimiento, table18$idestablecimiento))
#Complejo Hospitalario San José (Santiago, Independencia)  09-100 
#Hospital Clínico de Niños Dr. Roberto del Río (Santiago, Independencia)  09-101 
#Hospital San Juan de Dios (Santiago, Santiago)  10-100 
#Hospital de Urgencia Asistencia Pública Dr. Alejandro del Río (Santiago, Santiago) 11-195 
#Hospital de Niños Dr. Luis Calvo Mackenna (Santiago, Providencia) 12-102 
#Hospital Clínico Regional Dr. Guillermo Grant Benavente (Concepción) 18-100
#Hospital San Juan de Dios (La Serena) 05-100

#Manejo fechas
table18$fecha2<-substr(table18$fecha, start = 1, stop = 10)    # Para extraer la subcadena desde el primer al decimo caracter.
table18$fecha2<-as.Date(table18$fecha2, "%d/%m/%Y")
class(table18$fecha2)

Graficos<-function(estab, causa, titulo)
{  
  q<-subset(table18, idestablecimiento==estab & Idcausa==causa, select=c(idestablecimiento,
                                                                         nestablecimiento,
                                                                         Idcausa,
                                                                         glosacausa,
                                                                         Col01,
                                                                         fecha2))
  
  ggplot(q, aes(x = fecha2, y = Col01))+
    geom_point(colour= "purple", size=1)+
    xlab("Año 2018") +
    ylab("N°consultas diarias totales") +
    ggtitle(titulo) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_bw()                                                                       
  
}


#SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA
p1<-Graficos("09-100", "1", "HSJ-Total")
p2<-Graficos("09-101", "1", "HRRIO-Total")
p3<-Graficos("10-100", "1", "HSJD-Total")
p4<-Graficos("11-195", "1", "Posta central-Total")
p5<-Graficos("12-102", "1", "HLCM-Total")
p6<-Graficos("18-100", "1", "HConce-Total")
p7<-Graficos("05-100", "1", "HSere-Total")
p8<-Graficos("12-102  ", "1", "HLCM-Total(c)")

#TOTAL CAUSAS SISTEMA RESPIRATORIO (2)
p9<-Graficos("09-100", "2", "HSJ-Total_Resp")
p10<-Graficos("09-101", "2", "HRRIO-Total_Resp")
p11<-Graficos("10-100", "2", "HSJD-Total_Resp")
p12<-Graficos("11-195", "2", "Posta central-Total_Resp")
p13<-Graficos("12-102", "2", "HLCM-Total_Resp")
p14<-Graficos("18-100", "2", "HConce-Total_Resp")
p15<-Graficos("05-100", "2", "HSere-Total_Resp")
p16<-Graficos("12-102  ", "2", "HLCM-Total_Resp (c)")

#CAUSAS SISTEMA RESPIRATORIO (7)
p17<-Graficos("09-100", "7", "HSJ-Total_circ")
p18<-Graficos("09-101", "7", "HRRIO-Total_circ")
p19<-Graficos("10-100", "7", "HSJD-Total_circ")
p20<-Graficos("11-195", "7", "Posta central-Total_circ")
p21<-Graficos("12-102", "7", "HLCM-Total_circ")
p22<-Graficos("18-100", "7", "HConce-Total_circ")
p23<-Graficos("05-100", "7", "HSere-Total_circ")
p24<-Graficos("12-102  ", "7", "HLCM-Total_circ (c)")

#TOTAL CAUSAS SISTEMA CIRCULATORIO (12)
p25<-Graficos("09-100", "12", "HSJ-Circ")
p26<-Graficos("09-101", "12", "HRRIO-Circ")
p27<-Graficos("10-100", "12", "HSJD-Circ")
p28<-Graficos("11-195", "12", "Posta central-Circ")
p29<-Graficos("12-102", "12", "HLCM-Circ")
p30<-Graficos("18-100", "12", "HConce-Circ")
p31<-Graficos("05-100", "12", "HSere-Circ")
p32<-Graficos("12-102  ", "12", "HLCM-Circ (c)")

#TOTAL TRAUMATISMOS Y ENVENENAMIENTO (18)
p33<-Graficos("09-100", "18", "HSJ-Total_traum")
p34<-Graficos("09-101", "18", "HRRIO-Total_traum")
p35<-Graficos("10-100", "18", "HSJD-Total_traum")
p36<-Graficos("11-195", "18", "Posta central-Total_traum")
p37<-Graficos("12-102", "18", "HLCM-Total_traum")
p38<-Graficos("18-100", "18", "HConce-Total_traum")
p39<-Graficos("05-100", "18", "HSere-Total_traum")
p40<-Graficos("12-102  ", "18", "HLCM-Total_traum (c)")

#TOTAL DEMÁS CAUSAS (21)
p41<-Graficos("09-100", "21", "HSJ-Demas")
p42<-Graficos("09-101", "21", "HRRIO-Demas")
p43<-Graficos("10-100", "21", "HSJD-Demas")
p44<-Graficos("11-195", "21", "Posta central-Demas")
p45<-Graficos("12-102", "21", "HLCM-Demas")
p46<-Graficos("18-100", "21", "HConce-Demas")
p47<-Graficos("05-100", "21", "HSere-Demas")
p48<-Graficos("12-102  ", "21", "HLCM-Demas (c)")

ggexport(p1, p2, p3, p4, p5, p6, p7, p8,
         p9, p10, p11, p12, p13, p14, p15, p16,
         p17, p18, p19, p20, p21, p22, p23, p24,
         p25, p26, p27, p28, p29, p30, p31, p32,
         p33, p34, p35, p36, p37, p38, p39, p40,
         p41, p42, p43, p44, p45, p46, p47, p48,
         filename = "Fig/UE2018.pdf",
         nrow = 4, ncol = 2)

HSJ2018<-subset(table18, idestablecimiento=="09-100", select=c(idestablecimiento, nestablecimiento, Idcausa, glosacausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HRRIO2018<-subset(table18, idestablecimiento=="09-101", select=c(idestablecimiento, nestablecimiento, Idcausa, glosacausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSJD2018<-subset(table18, idestablecimiento=="10-100", select=c(idestablecimiento, nestablecimiento, Idcausa, glosacausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
PCentral2018<-subset(table18, idestablecimiento=="11-195", select=c(idestablecimiento, nestablecimiento, Idcausa, glosacausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HLCM2018<-subset(table18, idestablecimiento=="12-102", select=c(idestablecimiento, nestablecimiento, Idcausa, glosacausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HConce2018<-subset(table18, idestablecimiento=="18-100", select=c(idestablecimiento, nestablecimiento, Idcausa, glosacausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))
HSere2018<-subset(table18, idestablecimiento=="05-100", select=c(idestablecimiento, nestablecimiento, Idcausa, glosacausa, Col01, Col02, Col03, Col04, Col05, Col06, fecha2))

save(HSJ2018, HRRIO2018, HSJD2018, PCentral2018, HLCM2018, HConce2018, HSere2018, file="Out/Set2018.RData")
rm(list=ls())

