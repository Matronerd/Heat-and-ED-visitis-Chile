# Merge Exploratorias Tabla descriptivas de todas las causas y totales consultas----
library(readr); library(dplyr)
load("Out/SetHospitales.RData")

#Análisis descriptivo HConce
HConce <- HConce %>%
  mutate(
    IdCausa = as.numeric(IdCausa),
    Col01 = as.numeric(Col01),
    Col02 = as.numeric(Col02),
    Col03 = as.numeric(Col03),
    Col04 = as.numeric(Col04),
    Col05 = as.numeric(Col05),
    Col06 = as.numeric(Col06))

summary(HConce)

HConce <- HConce %>%
  mutate (adultos=Col04+Col05+Col06)

HConce_consultas <- HConce %>%
  group_by(IdCausa) %>%
  summarize(
    Total_Col01 = sum(Col01, na.rm = TRUE),
    Total_adultos = sum(adultos, na.rm = TRUE),
    Median_Col01 = median(Col01, na.rm = T),
    Median_adultos = median(adultos, na.rm = T),
    End_Date = max(fecha2, na.rm = TRUE),
    Duration = as.numeric(difftime(max(fecha2, na.rm = TRUE), min(fecha2, na.rm = TRUE), units = "days"))
  ) %>%
  mutate(
    Avg_Daily_Col01 = Total_Col01 / Duration,
    Avg_Daily_Adultos = Total_adultos / Duration,
  )

hist(HConce_consultas$Total_Col01, main = "Histograma", xlab = "Datos", ylab = "Frecuencia")
qqnorm(HConce_consultas$Total_Col01)
qqline(HConce_consultas$Total_Col01, col = "red")

# View the summarized data with daily averages
print(HConce_consultas, n=29)

# Optionally, export to a CSV file
write_csv(HConce_consultas, "Tablas/Conce_total.csv")

#Análisis descriptivo La Serena
HSere <- HSere %>%
  mutate(
    IdCausa = as.numeric(IdCausa),
    Col01 = as.numeric(Col01),
    Col02 = as.numeric(Col02),
    Col03 = as.numeric(Col03),
    Col04 = as.numeric(Col04),
    Col05 = as.numeric(Col05),
    Col06 = as.numeric(Col06))

summary(HSere)

HSere <- HSere %>%
  mutate (adultos=Col04+Col05+Col06)

HSere_consultas <- HSere %>%
  group_by(IdCausa) %>%
  summarize(
    Total_Col01 = sum(Col01, na.rm = TRUE),
    Total_adultos = sum(adultos, na.rm = TRUE),
    Median_Col01 = median(Col01, na.rm = T),
    Median_adultos = median(adultos, na.rm = T),
    End_Date = max(fecha2, na.rm = TRUE),
    Duration = as.numeric(difftime(max(fecha2, na.rm = TRUE), min(fecha2, na.rm = TRUE), units = "days"))
  ) %>%
  mutate(
    Avg_Daily_Col01 = Total_Col01 / Duration,
    Avg_Daily_Adultos = Total_adultos / Duration,
  )

hist(HSere_consultas$Total_Col01, main = "Histograma", xlab = "Datos", ylab = "Frecuencia")
qqnorm(HSere_consultas$Total_Col01)
qqline(HSere_consultas$Total_Col01, col = "red")
print(HSere_consultas, n=29)

# Guardar como csv
write_csv(HSere_consultas, "Tablas/HSere_total.csv")

#Análisis descriptivo HSJ
HSJ <- HSJ %>%
  mutate(
    IdCausa = as.numeric(IdCausa),
    Col01 = as.numeric(Col01),
    Col02 = as.numeric(Col02),
    Col03 = as.numeric(Col03),
    Col04 = as.numeric(Col04),
    Col05 = as.numeric(Col05),
    Col06 = as.numeric(Col06))

summary(HSJ)

HSJ <- HSJ %>%
  mutate (adultos=Col04+Col05+Col06)

HSJ_consultas <- HSJ %>%
  group_by(IdCausa) %>%
  summarize(
    Total_Col01 = sum(Col01, na.rm = TRUE),
    Total_adultos = sum(adultos, na.rm = TRUE),
    Median_Col01 = median(Col01, na.rm = T),
    Median_adultos = median(adultos, na.rm = T),
    End_Date = max(fecha2, na.rm = TRUE),
    Duration = as.numeric(difftime(max(fecha2, na.rm = TRUE), min(fecha2, na.rm = TRUE), units = "days"))
  ) %>%
  mutate(
    Avg_Daily_Col01 = Total_Col01 / Duration,
    Avg_Daily_Adultos = Total_adultos / Duration,
  )

qqnorm(HSJ_consultas$Total_Col01)
qqline(HSJ_consultas$Total_Col01, col = "red")

print(HSJ_consultas, n = 29)

# Guardar como csv
write_csv(HSJ_consultas, "Tablas/HSJ_Total.csv")

#San Juan de Dios
HSJD <- HSJD %>%
  mutate(
    IdCausa = as.numeric(IdCausa),
    Col01 = as.numeric(Col01),
    Col02 = as.numeric(Col02),
    Col03 = as.numeric(Col03),
    Col04 = as.numeric(Col04),
    Col05 = as.numeric(Col05),
    Col06 = as.numeric(Col06))

summary(HSJD)

HSJD <- HSJD %>%
  mutate (adultos=Col04+Col05+Col06)

HSJD_consultas <- HSJD %>%
  group_by(IdCausa) %>%
  summarize(
    Total_Col01 = sum(Col01, na.rm = TRUE),
    Total_adultos = sum(adultos, na.rm = TRUE),
    Median_Col01 = median(Col01, na.rm = T),
    Median_adultos = median(adultos, na.rm = T),
    End_Date = max(fecha2, na.rm = TRUE),
    Duration = as.numeric(difftime(max(fecha2, na.rm = TRUE), min(fecha2, na.rm = TRUE), units = "days"))
  ) %>%
  mutate(
    Avg_Daily_Col01 = Total_Col01 / Duration,
    Avg_Daily_Adultos = Total_adultos / Duration,
  )

print(HSJD_consultas, n = 29)

# Guardar como csv
write_csv(HSJD_consultas, "Tablas/HSJD_Total.csv")

#HUAP
PCentral <- PCentral %>%
  mutate(
    IdCausa = as.numeric(IdCausa),
    Col01 = as.numeric(Col01),
    Col02 = as.numeric(Col02),
    Col03 = as.numeric(Col03),
    Col04 = as.numeric(Col04),
    Col05 = as.numeric(Col05),
    Col06 = as.numeric(Col06))

summary(PCentral)

PCentral <- PCentral %>%
  mutate (adultos=Col04+Col05+Col06)

PCentral_consultas <- PCentral %>%
  group_by(IdCausa) %>%
  summarize(
    Total_Col01 = sum(Col01, na.rm = TRUE),
    Total_adultos = sum(adultos, na.rm = TRUE),
    Median_Col01 = median(Col01, na.rm = T),
    Median_adultos = median(adultos, na.rm = T),
    End_Date = max(fecha2, na.rm = TRUE),
    Duration = as.numeric(difftime(max(fecha2, na.rm = TRUE), min(fecha2, na.rm = TRUE), units = "days"))
  ) %>%
  mutate(
    Avg_Daily_Col01 = Total_Col01 / Duration,
    Avg_Daily_Adultos = Total_adultos / Duration,
  )

print(PCentral_consultas, n = 29)

# Guardar como csv
write_csv(PCentral_consultas, "Tablas/PCentral_Total.csv")

## Temperaturas ----
load("Out/t_dia_CCP_0918.RData")

temp_CCP_ver0918 <- t_dia_CCP_0918 %>%
  mutate(mes = month(fecha2)) %>%
  filter(mes >= 10 | mes <= 3) %>%
  summarize(
    Mean_Max_Temp = mean(t_max, na.rm = TRUE),
    SD_Max_Temp = sd(t_max, na.rm = TRUE),
    Mean_Avg_Temp = mean(t_mean, na.rm = TRUE),
    SD_Avg_Temp = sd(t_mean, na.rm = TRUE),
    Mean_Min_Temp = mean(t_min, na.rm = TRUE),
    SD_Min_Temp = sd(t_min, na.rm = TRUE)
  )
print(temp_CCP_ver0918)

temp_CCP_0914 <- t_dia_CCP_0918%>%
  filter(year(fecha2)>= 2009, year(fecha2) <=2014) %>%
  summarize(
    Mean_Max_Temp = mean(t_max, na.rm = TRUE),
    SD_Max_Temp = sd(t_max, na.rm = TRUE),
    Mean_Avg_Temp = mean(t_mean, na.rm = TRUE),
    SD_Avg_Temp = sd(t_mean, na.rm = TRUE),
    Mean_Min_Temp = mean(t_min, na.rm = TRUE),
    SD_Min_Temp = sd(t_min, na.rm = TRUE)
  )
print(temp_CCP_0914)

temp_CCP_1518 <- t_dia_CCP_0918%>%
  filter(year(fecha2)>= 2015, year(fecha2) <=2018) %>%
  summarize(
    Mean_Max_Temp = mean(t_max, na.rm = TRUE),
    SD_Max_Temp = sd(t_max, na.rm = TRUE),
    Mean_Avg_Temp = mean(t_mean, na.rm = TRUE),
    SD_Avg_Temp = sd(t_mean, na.rm = TRUE),
    Mean_Min_Temp = mean(t_min, na.rm = TRUE),
    SD_Min_Temp = sd(t_min, na.rm = TRUE)
  )
print(temp_CCP_1518)


load("Out/t_dia_LS_0918.RData")

temp_LS_ver0918 <- t_dia_LS_0918 %>%
  mutate(mes = month(fecha2)) %>%
  filter(mes >= 10 | mes <= 3) %>%
  summarize(
    Mean_Max_Temp = mean(t_max, na.rm = TRUE),
    SD_Max_Temp = sd(t_max, na.rm = TRUE),
    Mean_Avg_Temp = mean(t_mean, na.rm = TRUE),
    SD_Avg_Temp = sd(t_mean, na.rm = TRUE),
    Mean_Min_Temp = mean(t_min, na.rm = TRUE),
    SD_Min_Temp = sd(t_min, na.rm = TRUE)
  )
print(temp_LS_ver0918)

temp_LS <- t_dia_LS_0918%>%
  summarize(
    Mean_Max_Temp = mean(t_max, na.rm = TRUE),
    SD_Max_Temp = sd(t_max, na.rm = TRUE),
    Mean_Avg_Temp = mean(t_mean, na.rm = TRUE),
    SD_Avg_Temp = sd(t_mean, na.rm = TRUE),
    Mean_Min_Temp = mean(t_min, na.rm = TRUE),
    SD_Min_Temp = sd(t_min, na.rm = TRUE)
  )
print(temp_LS)

temp_LS_0914 <- t_dia_LS_0918%>%
  filter(year(fecha2)>= 2009, year(fecha2) <=2014) %>%
  summarize(
    Mean_Max_Temp = mean(t_max, na.rm = TRUE),
    SD_Max_Temp = sd(t_max, na.rm = TRUE),
    Mean_Avg_Temp = mean(t_mean, na.rm = TRUE),
    SD_Avg_Temp = sd(t_mean, na.rm = TRUE),
    Mean_Min_Temp = mean(t_min, na.rm = TRUE),
    SD_Min_Temp = sd(t_min, na.rm = TRUE)
  )
print(temp_LS_0914)

temp_LS_1518 <- t_dia_LS_0918%>%
  filter(year(fecha2)>= 2015, year(fecha2) <=2018) %>%
  summarize(
    Mean_Max_Temp = mean(t_max, na.rm = TRUE),
    SD_Max_Temp = sd(t_max, na.rm = TRUE),
    Mean_Avg_Temp = mean(t_mean, na.rm = TRUE),
    SD_Avg_Temp = sd(t_mean, na.rm = TRUE),
    Mean_Min_Temp = mean(t_min, na.rm = TRUE),
    SD_Min_Temp = sd(t_min, na.rm = TRUE)
  )
print(temp_LS_1518)


load("Out/t_dia_SCL_0918.RData")

temp_SCL_ver0918 <- t_dia_SCL_0918 %>%
  mutate(mes = month(fecha2)) %>%
  filter(mes >= 10 | mes <= 3) %>%
  summarize(
    Mean_Max_Temp = mean(t_max, na.rm = TRUE),
    SD_Max_Temp = sd(t_max, na.rm = TRUE),
    Mean_Avg_Temp = mean(t_mean, na.rm = TRUE),
    SD_Avg_Temp = sd(t_mean, na.rm = TRUE),
    Mean_Min_Temp = mean(t_min, na.rm = TRUE),
    SD_Min_Temp = sd(t_min, na.rm = TRUE)
  )
print(temp_SCL_ver0918)


temp_SCL <- t_dia_SCL_0918%>%
  summarize(
    Mean_Max_Temp = mean(t_max, na.rm = TRUE),
    SD_Max_Temp = sd(t_max, na.rm = TRUE),
    Mean_Avg_Temp = mean(t_mean, na.rm = TRUE),
    SD_Avg_Temp = sd(t_mean, na.rm = TRUE),
    Mean_Min_Temp = mean(t_min, na.rm = TRUE),
    SD_Min_Temp = sd(t_min, na.rm = TRUE)
  )
print(temp_SCL)

temp_SCL_0914 <- t_dia_SCL_0918%>%
  filter(year(fecha2)>= 2009, year(fecha2) <=2014) %>%
  summarize(
    Mean_Max_Temp = mean(t_max, na.rm = TRUE),
    SD_Max_Temp = sd(t_max, na.rm = TRUE),
    Mean_Avg_Temp = mean(t_mean, na.rm = TRUE),
    SD_Avg_Temp = sd(t_mean, na.rm = TRUE),
    Mean_Min_Temp = mean(t_min, na.rm = TRUE),
    SD_Min_Temp = sd(t_min, na.rm = TRUE)
  )
print(temp_SCL_0914)

temp_SCL_1518 <- t_dia_SCL_0918%>%
  filter(year(fecha2)>= 2015, year(fecha2) <=2018) %>%
  summarize(
    Mean_Max_Temp = mean(t_max, na.rm = TRUE),
    SD_Max_Temp = sd(t_max, na.rm = TRUE),
    Mean_Avg_Temp = mean(t_mean, na.rm = TRUE),
    SD_Avg_Temp = sd(t_mean, na.rm = TRUE),
    Mean_Min_Temp = mean(t_min, na.rm = TRUE),
    SD_Min_Temp = sd(t_min, na.rm = TRUE)
  )
print(temp_SCL_1518)