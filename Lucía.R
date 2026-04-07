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




######
prop.table(table(datos_sin_imputar$Mother.s.qualification))
prop.table(table(datos_imputados$Mother.s.qualification))



#Comparar diferencia de proporciones antes y después de la imputación
niveles_comunes <- sort(unique(c(
  datos_sin_imputar$Father.s.qualification,
  datos_imputados$Father.s.qualification
)))

tabla_sin <- prop.table(table(factor(datos_sin_imputar$Father.s.qualification, levels = niveles_comunes)))
tabla_imp <- prop.table(table(factor(datos_imputados$Father.s.qualification, levels = niveles_comunes)))

abs(tabla_sin - tabla_imp)

#imputacion por moda condicionada
library(clickR)

datos_moda_condicionada<- read.csv("estudiantes.csv", header = TRUE, sep = ";")
descriptive(datos_moda_condicionada)
# Convertimos los códigos de desconocido a NA
datos_moda_condicionada$Mother.s.qualification[datos_moda_condicionada$Mother.s.qualification == 34] <- NA
datos_moda_condicionada$Father.s.qualification[datos_moda_condicionada$Father.s.qualification == 34] <- NA
datos_moda_condicionada$Mother.s.occupation[datos_moda_condicionada$Mother.s.occupation == 99] <- NA
datos_moda_condicionada$Father.s.occupation[datos_moda_condicionada$Father.s.occupation == 99] <- NA

#funcion para calcular la moda
moda <- function(x) {
  ux <- na.omit(x)
  ux[which.max(tabulate(match(ux, ux)))]
}
#imputar por target

#mother.s.occupation
datos_moda_condicionada$Mother.s.occupation <- ave(
  datos_moda_condicionada$Mother.s.occupation,
  datos_moda_condicionada$Target,
  FUN = function(x) {
    x[is.na(x)] <- moda(x)
    return(x)
  }
)

#father.s.occupation
datos_moda_condicionada$Father.s.occupation <- ave(
  datos_moda_condicionada$Father.s.occupation,
  datos_moda_condicionada$Target,
  FUN = function(x) {
    x[is.na(x)] <- moda(x)
    return(x)
  }
)

#mother.s.qualification
datos_moda_condicionada$Mother.s.qualification <- ave(
  datos_moda_condicionada$Mother.s.qualification,
  datos_moda_condicionada$Target,
  FUN = function(x) {
    x[is.na(x)] <- moda(x)
    return(x)
  }
)

#father.s.qualification

datos_moda_condicionada$Father.s.qualification <- ave(
  datos_moda_condicionada$Father.s.qualification,
  datos_moda_condicionada$Target,
  FUN = function(x) {
    x[is.na(x)] <- moda(x)
    return(x)
  }
)
colSums(is.na(datos_moda_condicionada))


datos_moda_condicionada$Mother.s.qualification<- as.numeric(datos_moda_condicionada$Mother.s.qualification)
freq_mother_qualification_prueba <- table(datos_moda_condicionada$Mother.s.qualification)
freq_mother_qualification_prueba

prop.table(freq_mother_qualification_prueba)
barplot(freq_mother_qualification_prueba)


#mirando a ver si se porque faltan (no hacer caso de momento):

datos_faltantes_motivo <- read.csv("estudiantes.csv", header=TRUE, sep=";")

# Convertimos los códigos de desconocido a NA
datos_faltantes_motivo$Mother.s.qualification[datos_faltantes_motivo$Mother.s.qualification == 34] <- NA
datos_faltantes_motivo$Father.s.qualification[datos_faltantes_motivo$Father.s.qualification == 34] <- NA
datos_faltantes_motivo$Mother.s.occupation[datos_faltantes_motivo$Mother.s.occupation == 99] <- NA
datos_faltantes_motivo$Father.s.occupation[datos_faltantes_motivo$Father.s.occupation == 99] <- NA

# Creamos indicador de faltante
datos_faltantes_motivo$NA_mother_occupation <- is.na(datos_faltantes_motivo$Mother.s.occupation)

# Tabla
table(datos_faltantes_motivo$NA_mother_occupation,
      datos_faltantes_motivo$Mother.s.qualification)

# Proporciones por columna
prop.table(
  table(datos_faltantes_motivo$NA_mother_occupation,
        datos_faltantes_motivo$Target),
  2
)
#VALORES ATÍPICOS Y TRATAMIENTO:

#Variables numéricas continuas:
descriptive(datos_moda_condicionada$Admission.grade)
boxplot(datos_moda_condicionada$Admission.grade)

descriptive(datos_moda_condicionada$Unemployment.rate)
boxplot(datos_moda_condicionada$Unemployment.rate)

descriptive(datos_moda_condicionada$Inflation.rate)
boxplot(datos_moda_condicionada$Inflation.rate)

descriptive(datos_moda_condicionada$GDP)
boxplot(datos_moda_condicionada$GDP)

#Variables numéricas discretas:


descriptive(datos_moda_condicionada$Curricular.units.1st.sem..credited.)
boxplot((datos_moda_condicionada$Curricular.units.2nd.sem..grade.))

descriptive(datos_moda_condicionada$Curricular.units.1st.sem..grade.)
boxplot(datos_moda_condicionada$Curricular.units.1st.sem..grade.)

#Probando transformaciones:
library(ggplot2)
library(clickR)


datos_moda_condicionada$Admission.grade_transformado <- sqrt(datos_moda_condicionada$Admission.grade)
descriptive(datos_moda_condicionada$Admission.grade_transformado)
boxplot(datos_moda_condicionada$Admission.grade_transformado)

#se supone que esta transformación no la usaremos ya que afecta a la interpretación de la variable
#además de que, los atípicos de esta variable no distorsionan tanto la distribución como para que sea
#necesario realizar una transformación



