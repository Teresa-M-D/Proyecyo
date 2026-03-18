#espacio para Lucía

#probando
#Lo hemos conseguidooooo


library(clickR)
datos <- read.csv("estudiantes.csv", header=TRUE, sep=";")
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
unique(datos$Mother.s.qualification)
datos$Mother.s.qualification[datos$Mother.s.qualification == 34] <- NA
datos$Father.s.qualification[datos$Father.s.qualification == 34] <- NA
datos$Mother.s.occupation[datos$Mother.s.occupation == 99] <- NA
datos$Father.s.occupation[datos$Father.s.occupation == 99] <- NA


descriptive(datos)


#Seleccionar moda de Mother.s.occupation y Father.s.occupation:


descriptive(datos$Mother.s.occupation)
table(datos$Mother.s.occupation)
datos$Mother.s.occupation[is.na(datos$Mother.s.occupation)] <- "9"
descriptive(datos$Father.s.occupation)
datos$Father.s.occupation[is.na(datos$Father.s.occupation)] <- "9"


colSums(is.na(datos))
sum(is.na(datos))

