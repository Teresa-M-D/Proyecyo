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

descriptive(datos_moda_condicionada$Age.at.enrollment)
boxplot(datos_moda_condicionada$Age.at.enrollment)

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



#Recodificación de variables:

install.packages("dplyr")
library(dplyr)
datos_recodificados<-datos_moda_condicionada

#Nacionality:

datos_recodificados$Nationality<-recode(datos_recodificados$Nacionality, 
                                        `1` = "Portugués",
                                        `2` = "Alemán",
                                        `6` = "Español",
                                        `11` = "Italiano",
                                        `13` = "Neerlandés",
                                        `14` = "Inglés",
                                        `17` = "Lituano",
                                        `21` = "Angoleño",
                                        `22` = "Caboverdiano",
                                        `24` = "Guineano",
                                        `25` = "Mozambiqueño",
                                        `26` = "Santotomense",
                                        `32` = "Turco",
                                        `41` = "Brasileño",
                                        `62` = "Rumano",
                                        `100` = "Moldavo (República de Moldavia)",
                                        `101` = "Mexicano",
                                        `103` = "Ucraniano",
                                        `105` = "Ruso",
                                        `108` = "Cubano",
                                        `109` = "Colombiano")

descriptive(datos_recodificados)

#Course
datos_recodificados$Course<-recode(datos_recodificados$Course, 
                                   `33` = "Tecnologías de Producción de Biocombustibles",
                                   `171` = "Diseño de Animación y Multimedia",
                                   `8014` = "Trabajo Social (turno de tarde)",
                                   `9003` = "Agronomía",
                                   `9070` = "Diseño de Comunicación",
                                   `9085` = "Enfermería Veterinaria",
                                   `9119` = "Ingeniería Informática",
                                   `9130` = "Equinocultura",
                                   `9147` = "Gestión",
                                   `9238` = "Trabajo Social",
                                   `9254` = "Turismo",
                                   `9500` = "Enfermería",
                                   `9556` = "Higiene Bucodental",
                                   `9670` = "Gestión de Publicidad y Marketing",
                                   `9773` = "Periodismo y Comunicación",
                                   `9853` = "Educación Básica",
                                   `9991` = "Gestión (turno de tarde)")
#Mother's Cualification
datos_recodificados$Mother.s.qualification<-recode(datos_recodificados$Mother.s.qualification,
                                                   `1` = "Educación secundaria - 12º año o equivalente",
                                                   `2` = "Educación superior - Grado (Bachelor)",
                                                   `3` = "Educación superior - Grado",
                                                   `4` = "Educación superior - Máster",
                                                   `5` = "Educación superior - Doctorado",
                                                   `6` = "Asistencia a educación superior",
                                                   `9` = "12º año de escolarización - No completado",
                                                   `10` = "11º año de escolarización - No completado",
                                                   `11` = "7º año (sistema antiguo)",
                                                   `12` = "Otro - 11º año de escolarización",
                                                   `14` = "10º año de escolarización",
                                                   `18` = "Curso general de comercio",
                                                   `19` = "Educación básica 3er ciclo (9º/10º/11º año) o equivalente",
                                                   `22` = "Curso técnico-profesional",
                                                   `26` = "7º año de escolarización",
                                                   `27` = "2º ciclo del bachillerato general",
                                                   `29` = "9º año de escolarización - No completado",
                                                   `30` = "8º año de escolarización",
                                                   `34` = "Desconocido",
                                                   `35` = "No sabe leer ni escribir",
                                                   `36` = "Sabe leer sin haber completado 4º año",
                                                   `37` = "Educación básica 1er ciclo (4º/5º año) o equivalente",
                                                   `38` = "Educación básica 2º ciclo (6º/7º/8º año) o equivalente",
                                                   `39` = "Curso de especialización tecnológica",
                                                   `40` = "Educación superior - Grado (1er ciclo)",
                                                   `41` = "Curso de estudios superiores especializados",
                                                   `42` = "Curso técnico superior profesional",
                                                   `43` = "Educación superior - Máster (2º ciclo)",
                                                   `44` = "Educación superior - Doctorado (3er ciclo)")


#Father qualification:
datos_recodificados$Father.s.qualification <- recode(datos_recodificados$Father.s.qualification,
                                                     `1` = "Educación secundaria - 12º año o equivalente",
                                                     `2` = "Educación superior - Grado (Bachelor)",
                                                     `3` = "Educación superior - Grado",
                                                     `4` = "Educación superior - Máster",
                                                     `5` = "Educación superior - Doctorado",
                                                     `6` = "Asistencia a educación superior",
                                                     `9` = "12º año de escolarización - No completado",
                                                     `10` = "11º año de escolarización - No completado",
                                                     `11` = "7º año (sistema antiguo)",
                                                     `12` = "Otro - 11º año de escolarización",
                                                     `13` = "2º año de curso complementario de secundaria",
                                                     `14` = "10º año de escolarización",
                                                     `18` = "Curso general de comercio",
                                                     `19` = "Educación básica 3er ciclo (9º/10º/11º año) o equivalente",
                                                     `20` = "Curso complementario de secundaria",
                                                     `22` = "Curso técnico-profesional",
                                                     `25` = "Curso complementario de secundaria - no completado",
                                                     `26` = "7º año de escolarización",
                                                     `27` = "2º ciclo del bachillerato general",
                                                     `29` = "9º año de escolarización - No completado",
                                                     `30` = "8º año de escolarización",
                                                     `31` = "Curso general de administración y comercio",
                                                     `33` = "Curso complementario de contabilidad y administración",
                                                     `34` = "Desconocido",
                                                     `35` = "No sabe leer ni escribir",
                                                     `36` = "Sabe leer sin haber completado 4º año",
                                                     `37` = "Educación básica 1er ciclo (4º/5º año) o equivalente",
                                                     `38` = "Educación básica 2º ciclo (6º/7º/8º año) o equivalente",
                                                     `39` = "Curso de especialización tecnológica",
                                                     `40` = "Educación superior - Grado (1er ciclo)",
                                                     `41` = "Curso de estudios superiores especializados",
                                                     `42` = "Curso técnico superior profesional",
                                                     `43` = "Educación superior - Máster (2º ciclo)",
                                                     `44` = "Educación superior - Doctorado (3er ciclo)")

#Mothers occupation
datos_recodificados$Mother.s.occupation<-recode(datos_recodificados$Mother.s.occupation,
                                                `0` = "Estudiante",
                                                `1` = "Representantes del poder legislativo y ejecutivo, directores y gerentes",
                                                `2` = "Especialistas en actividades intelectuales y científicas",
                                                `3` = "Técnicos y profesiones de nivel intermedio",
                                                `4` = "Personal administrativo",
                                                `5` = "Trabajadores de servicios personales, seguridad y vendedores",
                                                `6` = "Agricultores y trabajadores cualificados en agricultura, pesca y silvicultura",
                                                `7` = "Trabajadores cualificados de la industria, construcción y artesanos",
                                                `8` = "Operadores de instalaciones y maquinaria y trabajadores de montaje",
                                                `9` = "Trabajadores no cualificados",
                                                `10` = "Profesiones de las fuerzas armadas",
                                                `90` = "Otra situación",
                                                `99` = "(en blanco)",
                                                `122` = "Profesionales de la salud",
                                                `123` = "Profesores",
                                                `125` = "Especialistas en tecnologías de la información y la comunicación (TIC)",
                                                `131` = "Técnicos y profesiones intermedias en ciencia e ingeniería",
                                                `132` = "Técnicos y profesionales de nivel intermedio en salud",
                                                `134` = "Técnicos intermedios en servicios jurídicos, sociales, deportivos, culturales y similares",
                                                `141` = "Empleados de oficina, secretarios y operadores de datos",
                                                `143` = "Operadores de datos, contabilidad, estadística, servicios financieros y registros",
                                                `144` = "Otro personal de apoyo administrativo",
                                                `151` = "Trabajadores de servicios personales",
                                                `152` = "Vendedores",
                                                `153` = "Trabajadores de cuidado personal y similares",
                                                `171` = "Trabajadores cualificados de la construcción (excepto electricistas)",
                                                `173` = "Trabajadores cualificados en impresión, instrumentos de precisión, joyería y artesanía",
                                                `175` = "Trabajadores en procesamiento de alimentos, madera, textil y otras industrias",
                                                `191` = "Trabajadores de limpieza",
                                                `192` = "Trabajadores no cualificados en agricultura, pesca y silvicultura",
                                                `193` = "Trabajadores no cualificados en industria extractiva, construcción, manufactura y transporte",
                                                `194` = "Ayudantes de preparación de comidas"
                                                )
  
#Father's occupation:
datos_recodificados$Father.s.occupation<-recode(datos_recodificados$Father.s.occupation,
                                                `0` = "Estudiante",
                                                `1` = "Representantes del poder legislativo y ejecutivo, directores y gerentes",
                                                `2` = "Especialistas en actividades intelectuales y científicas",
                                                `3` = "Técnicos y profesiones de nivel intermedio",
                                                `4` = "Personal administrativo",
                                                `5` = "Trabajadores de servicios personales, seguridad y vendedores",
                                                `6` = "Agricultores y trabajadores cualificados en agricultura, pesca y silvicultura",
                                                `7` = "Trabajadores cualificados de la industria, construcción y artesanía",
                                                `8` = "Operadores de instalaciones y maquinaria y trabajadores de montaje",
                                                `9` = "Trabajadores no cualificados",
                                                `10` = "Profesiones de las fuerzas armadas",
                                                `90` = "Otra situación",
                                                `99` = "(en blanco)",
                                                `101` = "Oficiales de las fuerzas armadas",
                                                `102` = "Sargentos de las fuerzas armadas",
                                                `103` = "Otro personal de las fuerzas armadas",
                                                `112` = "Directores de servicios administrativos y comerciales",
                                                `114` = "Directores de hostelería, comercio y otros servicios",
                                                `121` = "Especialistas en ciencias físicas, matemáticas, ingeniería y afines",
                                                `122` = "Profesionales de la salud",
                                                `123` = "Profesores",
                                                `124` = "Especialistas en finanzas, contabilidad, organización administrativa y relaciones públicas/comerciales",
                                                `131` = "Técnicos intermedios en ciencia e ingeniería",
                                                `132` = "Técnicos y profesionales intermedios de salud",
                                                `134` = "Técnicos intermedios en servicios jurídicos, sociales, deportivos y culturales",
                                                `135` = "Técnicos en tecnologías de la información y la comunicación",
                                                `141` = "Empleados de oficina, secretarios y operadores de datos",
                                                `143` = "Operadores de datos, contabilidad, estadística y servicios financieros",
                                                `144` = "Otro personal de apoyo administrativo",
                                                `151` = "Trabajadores de servicios personales",
                                                `152` = "Vendedores",
                                                `153` = "Trabajadores de cuidado personal y similares",
                                                `154` = "Personal de protección y seguridad",
                                                `161` = "Agricultores orientados al mercado y trabajadores agrícolas cualificados",
                                                `163` = "Agricultores de subsistencia, pescadores, cazadores y recolectores",
                                                `171` = "Trabajadores cualificados de la construcción (excepto electricistas)",
                                                `172` = "Trabajadores cualificados en metalurgia y trabajo del metal",
                                                `174` = "Trabajadores cualificados en electricidad y electrónica",
                                                `175` = "Trabajadores en alimentación, madera, textil y otras industrias",
                                                `181` = "Operadores de instalaciones y maquinaria fija",
                                                `182` = "Trabajadores de montaje",
                                                `183` = "Conductores de vehículos y operadores de maquinaria móvil",
                                                `192` = "Trabajadores no cualificados en agricultura, pesca y silvicultura",
                                                `193` = "Trabajadores no cualificados en industria, construcción y transporte",
                                                `194` = "Ayudantes de preparación de comidas",
                                                `195` = "Vendedores ambulantes (excepto alimentos) y servicios callejeros")

#Displaced

datos_recodificados$Displaced<-recode(datos_recodificados$Displaced,
                                      `1` = "Sí",
                                      `0` = "No")


#Education special needs:
datos_recodificados$Educational.special.needs<-recode(datos_recodificados$Educational.special.needs,
                                                      `1` = "Sí",
                                                      `0` = "No")

#Debtor:
datos_recodificados$Debtor<-recode(datos_recodificados$Debtor,
                                   `1` = "Sí",
                                   `0` = "No")

#Tution feets up:
datos_recodificados$Tuition.fees.up.to.date<-recode(datos_recodificados$Tuition.fees.up.to.date,
                                                    `1` = "Sí",
                                                    `0` = "No")

#International:
datos_recodificados$International<-recode(datos_recodificados$International,
                                          `1` = "Sí",
                                          `0` = "No")

#Gender:
datos_recodificados$Gender<-recode(datos_recodificados$Gender,
                                   `1` = "Masculino",
                                   `0` = "Femenino")

#Scholarship holder:
datos_recodificados$Scholarship.holder<-recode(datos_recodificados$Scholarship.holder,
                                               `1` = "Sí",
                                               `0` = "No")

#Marital Status:
datos_recodificados$Marital.status<-recode(datos_recodificados$Marital.status,
                                           `1` = "Soltero",
                                           `2` = "Casado",
                                           `3` = "Viudo",
                                           `4` = "Divorciado",
                                           `5` = "Con pareja",
                                           `6` = "Separado legalmente"
)


#Application mode:
datos_recodificados$Application.mode<-recode(datos_recodificados$Application.mode,
                                             `1` = "1ª fase - cupo general",
                                             `2` = "Ordenanza nº 612/93",
                                             `5` = "1ª fase - cupo especial (Islas Azores)",
                                             `7` = "Titulares de otros estudios superiores",
                                             `10` = "Ordenanza nº 854-B/99",
                                             `15` = "Estudiante internacional (grado)",
                                             `16` = "1ª fase - cupo especial (Isla de Madeira)",
                                             `17` = "2ª fase - cupo general",
                                             `18` = "3ª fase - cupo general",
                                             `26` = "Ordenanza nº 533-A/99, apartado b2 (plan diferente)",
                                             `27` = "Ordenanza nº 533-A/99, apartado b3 (otra institución)",
                                             `39` = "Mayores de 23 años",
                                             `42` = "Traslado",
                                             `43` = "Cambio de titulación",
                                             `44` = "Titulares de diploma de especialización tecnológica",
                                             `51` = "Cambio de institución/titulación",
                                             `53` = "Titulares de diploma de ciclo corto",
                                             `57` = "Cambio de institución/titulación (internacional)")


#Previous qualification:
datos_recodificados$Previous.qualification<-recode(datos_recodificados$Previous.qualification,
                                                   `1` = "Educación secundaria",
                                                   `2` = "Educación superior - grado (bachelor)",
                                                   `3` = "Educación superior - grado",
                                                   `4` = "Educación superior - máster",
                                                   `5` = "Educación superior - doctorado",
                                                   `6` = "Asistencia a educación superior",
                                                   `9` = "12º curso - no completado",
                                                   `10` = "11º curso - no completado",
                                                   `12` = "Otro - 11º curso",
                                                   `14` = "10º curso",
                                                   `15` = "10º curso - no completado",
                                                   `19` = "Educación básica 3er ciclo (9º/10º/11º) o equivalente",
                                                   `38` = "Educación básica 2º ciclo (6º/7º/8º) o equivalente",
                                                   `39` = "Curso de especialización tecnológica",
                                                   `40` = "Educación superior - grado (1er ciclo)",
                                                   `42` = "Curso técnico superior profesional",
                                                   `43` = "Educación superior - máster (2º ciclo)")


#Daytime evening attendance:
datos_recodificados$Daytime.evening.attendance.<-recode(datos_recodificados$Daytime.evening.attendance.,
                                                        `1` = "Mañana",
                                                        `0` = "Tarde")



descriptive(datos_recodificados)
colnames(datos_recodificados)
table(datos_recodificados$Previous.qualification..grade.)
ncol(datos_recodificados)
