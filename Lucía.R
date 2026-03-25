#espacio para Lucía

#BASE DE DATOS CON DATOS IMPUTADOS POR MODA:
library(clickR)

datos_imputados <- read.csv("estudiantes.csv", header = TRUE, sep = ";")

# Convertimos los códigos de desconocido a NA
datos_imputados$Mother.s.qualification[datos_imputados$Mother.s.qualification == 34] <- NA
datos_imputados$Father.s.qualification[datos_imputados$Father.s.qualification == 34] <- NA
datos_imputados$Mother.s.occupation[datos_imputados$Mother.s.occupation == 99] <- NA
datos_imputados$Father.s.occupation[datos_imputados$Father.s.occupation == 99] <- NA

# Comprobación
sum(is.na(datos_imputados$Mother.s.occupation))
sum(is.na(datos_imputados$Father.s.occupation))
sum(is.na(datos_imputados$Mother.s.qualification))
sum(is.na(datos_imputados$Father.s.qualification))
sum(is.na(datos_imputados))

descriptive(datos_imputados)

# Imputación por moda (sin comillas)
datos_imputados$Mother.s.occupation[is.na(datos_imputados$Mother.s.occupation)] <- "9"
datos_imputados$Father.s.occupation[is.na(datos_imputados$Father.s.occupation)] <- "9"
datos_imputados$Mother.s.qualification[is.na(datos_imputados$Mother.s.qualification)] <- "1"
datos_imputados$Father.s.qualification[is.na(datos_imputados$Father.s.qualification)] <- "37"

# Comprobación
sum(is.na(datos_imputados$Mother.s.occupation))
sum(is.na(datos_imputados$Father.s.occupation))
sum(is.na(datos_imputados$Mother.s.qualification))
sum(is.na(datos_imputados$Father.s.qualification))
sum(is.na(datos_imputados))

descriptive(datos_imputados)



#hasta aquí

library(clickR)
datos <- read.csv("estudiantes.csv", header=TRUE, sep=";")
datos_imputados <- read.csv("estudiantes.csv", header=TRUE, sep=";")
dim(datos)
head(datos)



table(datos$Father.s.occupation)
table(datos$Mother.s.occupation)

#Vemos que hay 19 faltantes en ocupación de padre y 17 en ocupacion de la madre
#siendo aproximadamente un 0,9% de todos los datos

#vemos cuantos valores desconocidos hay
table(datos$Mother.s.qualification)
#hay 130 valores desconocidos

table(datos$Father.s.qualification)
#hay 112 valores desconocidos

#hay un total de 6,28% de faltantes si contamos los desconocidos como faltantes


mine.plot(datos)


#Convertir "unknown" en NA:
unique(datos_imputados$Mother.s.qualification)
datos_imputados$Mother.s.qualification[datos_imputados$Mother.s.qualification == 34] <- NA
datos_imputados$Father.s.qualification[datos_imputados$Father.s.qualification == 34] <- NA
datos_imputados$Mother.s.occupation[datos_imputados$Mother.s.occupation == 99] <- NA
datos_imputados$Father.s.occupation[datos_imputados$Father.s.occupation == 99] <- NA


descriptive(datos_imputados)


#Seleccionar moda de Mother.s.occupation y Father.s.occupation:

#usamos descriptive y table para ver la moda de Mother.s.occupation
descriptive(datos$Mother.s.occupation)
table(datos$Mother.s.occupation) 

#para ver cuantos faltantes tenemos
colSums(is.na(datos_imputados))

#imputamos los NA de esta variable con su moda y así con las otras tres variables
#(Father.s.occupation, Mother.s.qualification, Father.s.qualification)
datos_imputados$Mother.s.occupation[is.na(datos_imputados$Mother.s.occupation)] <- "9"
descriptive(datos_imputados$Father.s.occupation)
datos_imputados$Father.s.occupation[is.na(datos_imputados$Father.s.occupation)] <- "9"

table(datos_imputados$Mother.s.qualification)
datos_imputados$Mother.s.qualification[is.na(datos_imputados$Mother.s.qualification)] <- "1"

table(datos_imputados$Father.s.qualification)
datos_imputados$Father.s.qualification[is.na(datos_imputados$Father.s.qualification)] <- "37"

#vemos que en efecto ya no quedan faltantes
colSums(is.na(datos_imputados))
sum(is.na(datos_imputados))



descriptive(datos_imputados)
