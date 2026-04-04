#espacio para Lucía




#BASE DE DATOS CON DATOS IMPUTADOS POR MODA:
library(clickR)

datos_imputados <- read.csv("estudiantes.csv", header = TRUE, sep = ";")
datos_sin_imputar <- read.csv("estudiantes.csv", header=TRUE, sep=";")
descriptive(datos_sin_imputar)
dim(datos_sin_imputar)
names(datos_sin_imputar)
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

#Vamos a comparar estudios con datos imputados por moda y sin imputar

#imputados

datos_imputados$Mother.s.qualification <- as.numeric(datos_imputados$Mother.s.qualification)
freq_mother_qualification_imputados <- table(datos_imputados$Mother.s.qualification)
freq_mother_qualification_imputados

prop.table(freq_mother_qualification_imputados)
barplot(freq_mother_qualification_imputados)



#sin imputar
freq_mother_qualification <- table(datos_sin_imputar$Mother.s.qualification)
freq_mother_qualification

prop.table(freq_mother_qualification)
barplot(freq_mother_qualification)


prop.table(table(datos_sin_imputar$Mother.s.qualification))
prop.table(table(datos_imputados$Mother.s.qualification))
prop.table(table(datos_sin_imputar$Target))
prop.table(table(datos_imputados$Target))
prop.table(table(datos_sin_na$Target, datos_sin_imputar$Gender), 1)
prop.table(table(datos_imputados$Target, datos_imputados$Gender), 1)

abs(
  prop.table(table(datos_sin_imputar$Mother.s.qualification)) -
    prop.table(table(datos_imputados$Mother.s.qualification))
)


#imputacion por moda condicionada
library(clickR)

datos_prueba <- read.csv("estudiantes.csv", header = TRUE, sep = ";")
descriptive(datos_prueba)
# Convertimos los códigos de desconocido a NA
datos_prueba$Mother.s.qualification[datos_prueba$Mother.s.qualification == 34] <- NA
datos_prueba$Father.s.qualification[datos_prueba$Father.s.qualification == 34] <- NA
datos_prueba$Mother.s.occupation[datos_prueba$Mother.s.occupation == 99] <- NA
datos_prueba$Father.s.occupation[datos_prueba$Father.s.occupation == 99] <- NA

#funcion para calcular la moda
moda <- function(x) {
  ux <- na.omit(x)
  ux[which.max(tabulate(match(ux, ux)))]
}
#imputar por target

#mother.s.occupation
datos_prueba$Mother.s.occupation <- ave(
  datos_prueba$Mother.s.occupation,
  datos_prueba$Target,
  FUN = function(x) {
    x[is.na(x)] <- moda(x)
    return(x)
  }
)

#father.s.occupation
datos_prueba$Father.s.occupation <- ave(
  datos_prueba$Father.s.occupation,
  datos_prueba$Target,
  FUN = function(x) {
    x[is.na(x)] <- moda(x)
    return(x)
  }
)

#mother.s.qualification
datos_prueba$Mother.s.qualification <- ave(
  datos_prueba$Mother.s.qualification,
  datos_prueba$Target,
  FUN = function(x) {
    x[is.na(x)] <- moda(x)
    return(x)
  }
)

#father.s.qualification

datos_prueba$Father.s.qualification <- ave(
  datos_prueba$Father.s.qualification,
  datos_prueba$Target,
  FUN = function(x) {
    x[is.na(x)] <- moda(x)
    return(x)
  }
)
colSums(is.na(datos_prueba))


datos_prueba$Mother.s.qualification <- as.numeric(datos_prueba$Mother.s.qualification)
freq_mother_qualification_prueba <- table(datos_prueba$Mother.s.qualification)
freq_mother_qualification_prueba

prop.table(freq_mother_qualification_prueba)
barplot(freq_mother_qualification_prueba)
