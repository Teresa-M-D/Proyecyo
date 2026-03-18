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