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

#Objetivo: Comparar la media del PIB entre los estudiantes que abandonan y los que se gradúan.


# Filtramos los estudiantes cuyo estado final es "Dropout"
# Esto crea un subconjunto del dataset solo con los alumnos que abandonaron
dropouts <- datos[datos$Target == "Dropout", ]


# Filtramos los estudiantes cuyo estado final es "Graduate"
# Este subconjunto contiene únicamente a los alumnos que se graduaron  
graduates <- datos[datos$Target == "Graduate", ]

# Calculamos la media del PIB para los estudiantes que abandonaron
mean_pib_dropouts <- mean(dropouts$GDP)

# Calculamos la media del PIB para los estudiantes que se graduaron
mean_pib_graduates <- mean(graduates$GDP)

# Mostramos los resultados en pantalla

mean_pib_dropouts #-0.1508586
mean_pib_graduates #0.08183341

#Interpretación sencilla:

#Los estudiantes que abandonaron entraron en años económicamente peores
#Los que se graduaron entraron en años mejores.

#Objetivo: comparar la medias de tasa de desempleo entre los estudiantes que abandonan y los que se graduan

mean_desempleo_dropouts <- mean(dropouts$Unemployment.rate)
mean_desempleo_graduates <- mean(graduates$Unemployment.rate)

mean_desempleo_dropouts #11.6164
mean_desempleo_graduates #11.63934

#Interpretación: 
#Vemos que las medias de desempleo son muy parecidas, siendo un poco mayor la de los estudiantes que se graduaron
#lo que significa que entraron cuando la tasa de desempleo era un poco más alta


#Objetivo: comparar la medias de tasa de inflacio entre los estudiantes que abandonan y los que se graduan


mean_inflacion_dropouts <- mean(dropouts$Inflation.rate)
mean_inflacion_graduates <- mean(graduates$Inflation.rate)

mean_inflacion_dropouts #1.283955
mean_inflacion_graduates #1.197918

#Interpretación:
#Los estudiantes que abandonaron entraron en años donde la tasa de inflación era más alta
#Los que se graduaron entraron en años donde la tasa de inflación era un poco más baja 


