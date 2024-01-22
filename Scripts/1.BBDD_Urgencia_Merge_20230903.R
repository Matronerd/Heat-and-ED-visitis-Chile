#Unión de bases de datos por centro de salud
#Cargar bases para análisis 2009-2018

load("Out/Set2009.RData")
load("Out/Set2010.RData")
load("Out/Set2011.RData")
load("Out/Set2012.RData")
load("Out/Set2013.RData")
load("Out/Set2014.RData")
load("Out/Set2015.RData")
load("Out/Set2016.RData")
load("Out/Set2017.RData")
load("Out/Set2018.RData")

#Unir hospitales de adultos ----
HSJ <- rbind(HSJ2009, HSJ2010, HSJ2011, HSJ2012,
             HSJ2013, HSJ2014, HSJ2015, HSJ2016,
             HSJ2017, HSJ2018) #arroja error si no se corrigen nombre 2017 y 2018
#cambiar nombres BBDD 2017, 2018
names (HSJ2017)[1] = "IdEstablecimiento"
names (HSJ2017)[2] = "NEstablecimiento"
names (HSJ2017)[3] = "IdCausa"
names (HSJ2017)[4] = "GlosaCausa"

names (HSJ2018)[1] = "IdEstablecimiento"
names (HSJ2018)[2] = "NEstablecimiento"
names (HSJ2018)[3] = "IdCausa"
names (HSJ2018)[4] = "GlosaCausa"

HSJ <- rbind(HSJ2009, HSJ2010, HSJ2011, HSJ2012, HSJ2013, HSJ2014,
             HSJ2015, HSJ2016, HSJ2017, HSJ2018)

HSJD <- rbind(HSJD2009, HSJD2010, HSJD2011, HSJD2012, HSJD2013, HSJD2014,
              HSJD2015, HSJD2016, HSJD2017, HSJD2018)
names (HSJD2017)[1] = "IdEstablecimiento"
names (HSJD2017)[2] = "NEstablecimiento"
names (HSJD2017)[3] = "IdCausa"
names (HSJD2017)[4] = "GlosaCausa"

names (HSJD2018)[1] = "IdEstablecimiento"
names (HSJD2018)[2] = "NEstablecimiento"
names (HSJD2018)[3] = "IdCausa"
names (HSJD2018)[4] = "GlosaCausa"

PCentral <- rbind(PCentral2009, PCentral2010, PCentral2011, PCentral2012, PCentral2013, PCentral2014,
                  PCentral2015, PCentral2016, PCentral2017, PCentral2018)

names (PCentral2017)[1] = "IdEstablecimiento"
names (PCentral2017)[2] = "NEstablecimiento"
names (PCentral2017)[3] = "IdCausa"
names (PCentral2017)[4] = "GlosaCausa"

names (PCentral2018)[1] = "IdEstablecimiento"
names (PCentral2018)[2] = "NEstablecimiento"
names (PCentral2018)[3] = "IdCausa"
names (PCentral2018)[4] = "GlosaCausa"

HConce <- rbind(HConce2009, HConce2010, HConce2011, HConce2012, HConce2013, HConce2014,
                HConce2015, HConce2016, HConce2017, HConce2018)
names (HConce2017)[1] = "IdEstablecimiento"
names (HConce2017)[2] = "NEstablecimiento"
names (HConce2017)[3] = "IdCausa"
names (HConce2017)[4] = "GlosaCausa"

names (HConce2018)[1] = "IdEstablecimiento"
names (HConce2018)[2] = "NEstablecimiento"
names (HConce2018)[3] = "IdCausa"
names (HConce2018)[4] = "GlosaCausa"

HSere <- rbind(HSere2009, HSere2010, HSere2011, HSere2012, HSere2013, HSere2014,
               HSere2015, HSere2016, HSere2017, HSere2018)

names (HSere2017)[1] = "IdEstablecimiento"
names (HSere2017)[2] = "NEstablecimiento"
names (HSere2017)[3] = "IdCausa"
names (HSere2017)[4] = "GlosaCausa"

names (HSere2018)[1] = "IdEstablecimiento"
names (HSere2018)[2] = "NEstablecimiento"
names (HSere2018)[3] = "IdCausa"
names (HSere2018)[4] = "GlosaCausa"

#Unir hospitales de niños ----
HRRIO <- rbind(HRRIO2009, HRRIO2010, HRRIO2011, HRRIO2012, HRRIO2013, HRRIO2014,
               HRRIO2015, HRRIO2016, HRRIO2017, HRRIO2018)

names (HRRIO2017)[1] = "IdEstablecimiento"
names (HRRIO2017)[2] = "NEstablecimiento"
names (HRRIO2017)[3] = "IdCausa"
names (HRRIO2017)[4] = "GlosaCausa"

names (HRRIO2018)[1] = "IdEstablecimiento"
names (HRRIO2018)[2] = "NEstablecimiento"
names (HRRIO2018)[3] = "IdCausa"
names (HRRIO2018)[4] = "GlosaCausa"

HLCM <- rbind(HLCM2009, HLCM2010, HLCM2011, HLCM2012, HLCM2013, HLCM2014,
              HLCM2015, HLCM2016, HLCM2017, HLCM2018)

names (HLCM2017)[1] = "IdEstablecimiento"
names (HLCM2017)[2] = "NEstablecimiento"
names (HLCM2017)[3] = "IdCausa"
names (HLCM2017)[4] = "GlosaCausa"

names (HLCM2018)[1] = "IdEstablecimiento"
names (HLCM2018)[2] = "NEstablecimiento"
names (HLCM2018)[3] = "IdCausa"
names (HLCM2018)[4] = "GlosaCausa"

save(HSJ, HRRIO, HSJD, PCentral, HLCM, HConce, HSere, file="Out/SetHospitales.RData")
rm(list=ls())
