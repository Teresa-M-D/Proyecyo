#imputación de datos faltantes por moda condicionada:


library(clickR)

datos_sin_imputar <- read.csv("estudiantes.csv", header=TRUE, sep=";")

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



