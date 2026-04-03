#Espacio de Yalda
library(clickR)
library(rio)
library(ggplot2)
library(plotly)
library(GGally)
datos <- read.csv("estudiantes.csv", sep = ";", header = TRUE)
descriptive(datos)
#Análisis inicial de la variable Target

#Tabla de frecuencias absolutas
freq_target <- table(datos$Target)
freq_target
#Frecuencias relativas
prop.table(freq_target)

#Porcentajes
porcentajes = prop.table(freq_target) * 100
porcentajes 

#Valores atípicos en variables numéricas
boxplot(datos$Previous.qualification..grade., main = "Calificación de admisión (del 0 a 200)")
boxplot(datos$Unemployment.rate, main= "Tasa de desmpleo")
boxplot(datos$Inflation.rate, main= "Tasa de inflación")
boxplot(datos$GDP, main= "PIB")

#No tiene sentido hacer boxplot de variables numericas discretas!!!!


#Transformación lineal de la variable Calificación de admisión, en concreto, un cambio de escala (del 1 al 10)
datos$NotaAdmisión <- (datos$Previous.qualification..grade. / 200) * 10
descriptive(datos)
boxplot(datos$NotaAdmisión, main="Nota de Admisión (del 0 al 10)")
