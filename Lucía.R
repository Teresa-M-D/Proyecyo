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

descriptive(datos_moda_condicionada$Previous.qualification..grade.)
boxplot(datos_moda_condicionada$Previous.qualification..grade.)
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
datos_recodificados$Mother.s.occupation <- as.integer(datos_moda_condicionada$Mother.s.occupation)
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
                                                `194` = "Ayudantes de preparación de comidas")
  
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




colnames(datos_recodificados)
#Transformacion lineal notas sobre 200 a sobre 10:

#Previos qualification grade (nota de estudios previos):
datos_recodificados$Previous.qualification.grade_10 <- (datos_recodificados$Previous.qualification..grade. / 20)

#Admission grade (nota de adimisión):
datos_recodificados$Admission.grade_10 <- (datos_recodificados$Admission.grade / 20)

#Reagrupaciones:

#Reagrupamos Nacionalidades en categorías Portugal, Europa, África y América Latina

table(datos_recodificados$Nationality)


datos_recodificados$Nationality_group <- ifelse(
  datos_recodificados$Nationality == "Portugués", "Portugal",
  
  ifelse(datos_recodificados$Nationality %in% c("Español", "Alemán", "Italiano", "Neerlandés", "Inglés", "Lituano", "Rumano", "Ruso", "Turco", "Ucraniano", "Moldavo (República de Moldavia)"),
         "Europa",
         
         ifelse(datos_recodificados$Nationality %in% c("Brasileño", "Mexicano", "Cubano", "Colombiano"),
                "América Latina",
                
                ifelse(datos_recodificados$Nationality %in% c("Angoleño", "Caboverdiano", "Guineano", "Mozambiqueño", "Santotomense"),
                       "África",
                       NA
                )
         )
  )
)



descriptive(datos_recodificados)
table(datos_recodificados$Nationality_group)


#Previous qualification:
table(datos_recodificados$Previous.qualification)
datos_recodificados$Previous_education_level <- case_when(
  
  # BAJO
  datos_recodificados$Previous.qualification %in% c(
    "10º curso", 
    "10º curso - no completado",
    "11º curso - no completado",
    "12º curso - no completado",
    "Educación básica 2º ciclo (6º/7º/8º) o equivalente",
    "Educación básica 3er ciclo (9º/10º/11º) o equivalente",
    "Otro - 11º curso"
  ) ~ "Bajo",
  
  # MEDIO
  datos_recodificados$Previous.qualification %in% c(
    "Educación secundaria"
  ) ~ "Medio",
  
  # TÉCNICO
  datos_recodificados$Previous.qualification %in% c(
    "Curso de especialización tecnológica",
    "Curso técnico superior profesional"
  ) ~ "Técnico",
  
  # SUPERIOR
  datos_recodificados$Previous.qualification %in% c(
    "Educación superior - grado",
    "Educación superior - grado (1er ciclo)",
    "Educación superior - grado (bachelor)",
    "Educación superior - máster",
    "Educación superior - máster (2º ciclo)",
    "Educación superior - doctorado",
    "Asistencia a educación superior"
  ) ~ "Superior"
)

descriptive(datos_recodificados)
table(datos_recodificados$Previous_education_level)

#Mother qualification en nivel bajo, medio, técnico y superior
table(datos_recodificados$Mother.s.qualification)


datos_recodificados$Mother_education_level <- case_when(
  
  # BAJO
  datos_recodificados$Mother.s.qualification %in% c(
    "10º año de escolarización", 
    "11º año de escolarización - No completado",
    "12º año de escolarización - No completado",
    "7º año (sistema antiguo)",
    "7º año de escolarización",
    "8º año de escolarización",
    "9º año de escolarización - No completado",
    "Educación básica 1er ciclo (4º/5º año) o equivalente",
    "Educación básica 2º ciclo (6º/7º/8º año) o equivalente",
    "Educación básica 3er ciclo (9º/10º/11º año) o equivalente",
    "Otro - 11º año de escolarización",
    "Sabe leer sin haber completado 4º año",
    "No sabe leer ni escribir"
  ) ~ "Bajo",
  
  # MEDIO
  datos_recodificados$Mother.s.qualification %in% c(
    "Educación secundaria - 12º año o equivalente",
    "2º ciclo del bachillerato general"
  ) ~ "Medio",
  
  # TÉCNICO
  datos_recodificados$Mother.s.qualification %in% c(
    "Curso de especialización tecnológica",
    "Curso de estudios superiores especializados",
    "Curso técnico-profesional",
    "Curso técnico superior profesional",
    "Curso general de comercio"
  ) ~ "Técnico",
  
  # SUPERIOR
  datos_recodificados$Mother.s.qualification %in% c(
    "Educación superior - Doctorado",
    "Educación superior - Doctorado (3er ciclo)",
    "Educación superior - Grado",
    "Educación superior - Grado (Bachelor)",
    "Educación superior - Máster",
    "Educación superior - Máster (2º ciclo)",
    "Educación superior - Grado (1er ciclo)",
    "Asistencia a educación superior"
  ) ~ "Superior"
)

table(datos_recodificados$Mother_education_level)

#Father qualification en nivel bajo, medio, técnico y superior
table(datos_recodificados$Father.s.qualification)

datos_recodificados$Father_education_level <- case_when(
  # BAJO
  datos_recodificados$Father.s.qualification %in% c(
    "10º año de escolarización", 
    "11º año de escolarización - No completado",
    "12º año de escolarización - No completado",
    "7º año (sistema antiguo)",
    "7º año de escolarización",
    "8º año de escolarización",
    "9º año de escolarización - No completado",
    "Educación básica 1er ciclo (4º/5º año) o equivalente",
    "Educación básica 2º ciclo (6º/7º/8º año) o equivalente",
    "Educación básica 3er ciclo (9º/10º/11º año) o equivalente",
    "Otro - 11º año de escolarización",
    "Sabe leer sin haber completado 4º año",
    "No sabe leer ni escribir"
  ) ~ "Bajo",
  
  # MEDIO
  datos_recodificados$Father.s.qualification %in% c(
    "Educación secundaria - 12º año o equivalente",
    "2º ciclo del bachillerato general"
  ) ~ "Medio",
  
  # TÉCNICO
  datos_recodificados$Father.s.qualification %in% c(
    "Curso de especialización tecnológica",
    "Curso de estudios superiores especializados",
    "Curso técnico-profesional",
    "Curso técnico superior profesional",
    "Curso general de comercio",
    "Curso general de administración y comercio",
    "Curso complementario de contabilidad y administración",
    "Curso complementario de secundaria",
    "Curso complementario de secundaria - no completado",
    "2º año de curso complementario de secundaria"
  ) ~ "Técnico",
  
  # SUPERIOR
  datos_recodificados$Father.s.qualification %in% c(
    "Educación superior - Doctorado",
    "Educación superior - Doctorado (3er ciclo)",
    "Educación superior - Grado",
    "Educación superior - Grado (Bachelor)",
    "Educación superior - Máster",
    "Educación superior - Máster (2º ciclo)",
    "Educación superior - Grado (1er ciclo)",
    "Asistencia a educación superior"
  ) ~ "Superior"
)
table(datos_recodificados$Father_education_level)


#Mother occupation

table(datos_recodificados$Mother.s.occupation)

datos_recodificados$Mother_occupation_level <- case_when(
  
  # ALTA CUALIFICACIÓN
  datos_recodificados$Mother.s.occupation %in% c(
    "Representantes del poder legislativo y ejecutivo, directores y gerentes",
    "Especialistas en actividades intelectuales y científicas",
    "Especialistas en tecnologías de la información y la comunicación (TIC)",
    "Profesionales de la salud",
    "Profesores"
  ) ~ "Alta cualificación",
  
  # CUALIFICACIÓN MEDIA
  datos_recodificados$Mother.s.occupation %in% c(
    "Personal administrativo",
    "Empleados de oficina, secretarios y operadores de datos",
    "Operadores de datos, contabilidad, estadística, servicios financieros y registros",
    "Otro personal de apoyo administrativo",
    "Técnicos y profesiones de nivel intermedio",
    "Técnicos y profesiones intermedias en ciencia e ingeniería",
    "Técnicos y profesionales de nivel intermedio en salud",
    "Técnicos intermedios en servicios jurídicos, sociales, deportivos, culturales y similares"
  ) ~ "Cualificación media",
  
  # BAJA CUALIFICACIÓN
  datos_recodificados$Mother.s.occupation %in% c(
    "Trabajadores de servicios personales, seguridad y vendedores",
    "Trabajadores de servicios personales",
    "Vendedores",
    "Trabajadores de cuidado personal y similares",
    "Agricultores y trabajadores cualificados en agricultura, pesca y silvicultura",
    "Trabajadores cualificados de la industria, construcción y artesanos",
    "Trabajadores cualificados de la construcción (excepto electricistas)",
    "Trabajadores cualificados en impresión, instrumentos de precisión, joyería y artesanía",
    "Trabajadores en procesamiento de alimentos, madera, textil y otras industrias",
    "Operadores de instalaciones y maquinaria y trabajadores de montaje",
    "Ayudantes de preparación de comidas"
  ) ~ "Baja cualificación",
  
  # NO CUALIFICADOS
  datos_recodificados$Mother.s.occupation %in% c(
    "Trabajadores no cualificados",
    "Trabajadores no cualificados en agricultura, pesca y silvicultura",
    "Trabajadores no cualificados en industria extractiva, construcción, manufactura y transporte",
    "Trabajadores de limpieza"
  ) ~ "No cualificados",
  
  # OTROS
  datos_recodificados$Mother.s.occupation %in% c(
    "Estudiante",
    "Otra situación",
    "Profesiones de las fuerzas armadas"
  ) ~ "Otros"

)
sum(table(datos_recodificados$Mother_occupation_level))
table(datos_recodificados$Mother.s.occupation)

#Father occupation

table(datos_recodificados$Father.s.occupation)


datos_recodificados$Father_occupation_level <- case_when(
  
  # ALTA CUALIFICACIÓN
  datos_recodificados$Father.s.occupation %in% c(
    "Representantes del poder legislativo y ejecutivo, directores y gerentes",
    "Directores de servicios administrativos y comerciales",
    "Directores de hostelería, comercio y otros servicios",
    "Especialistas en actividades intelectuales y científicas",
    "Especialistas en ciencias físicas, matemáticas, ingeniería y afines",
    "Especialistas en finanzas, contabilidad, organización administrativa y relaciones públicas/comerciales",
    "Profesionales de la salud",
    "Profesores"
  ) ~ "Alta cualificación",
  
  # CUALIFICACIÓN MEDIA
  datos_recodificados$Father.s.occupation %in% c(
    "Personal administrativo",
    "Empleados de oficina, secretarios y operadores de datos",
    "Operadores de datos, contabilidad, estadística y servicios financieros",
    "Otro personal de apoyo administrativo",
    "Técnicos en tecnologías de la información y la comunicación",
    "Técnicos y profesiones de nivel intermedio",
    "Técnicos intermedios en ciencia e ingeniería",
    "Técnicos y profesionales intermedios de salud",
    "Técnicos intermedios en servicios jurídicos, sociales, deportivos y culturales"
  ) ~ "Cualificación media",
  
  # BAJA CUALIFICACIÓN
  datos_recodificados$Father.s.occupation %in% c(
    "Trabajadores de servicios personales, seguridad y vendedores",
    "Trabajadores de servicios personales",
    "Vendedores",
    "Vendedores ambulantes (excepto alimentos) y servicios callejeros",
    "Trabajadores de cuidado personal y similares",
    "Personal de protección y seguridad",
    "Agricultores y trabajadores cualificados en agricultura, pesca y silvicultura",
    "Agricultores orientados al mercado y trabajadores agrícolas cualificados",
    "Agricultores de subsistencia, pescadores, cazadores y recolectores",
    "Trabajadores cualificados de la industria, construcción y artesanía",
    "Trabajadores cualificados de la construcción (excepto electricistas)",
    "Trabajadores cualificados en metalurgia y trabajo del metal",
    "Trabajadores cualificados en electricidad y electrónica",
    "Trabajadores en alimentación, madera, textil y otras industrias",
    "Operadores de instalaciones y maquinaria y trabajadores de montaje",
    "Operadores de instalaciones y maquinaria fija",
    "Conductores de vehículos y operadores de maquinaria móvil",
    "Trabajadores de montaje",
    "Ayudantes de preparación de comidas"
  ) ~ "Baja cualificación",
  
  # NO CUALIFICADOS
  datos_recodificados$Father.s.occupation %in% c(
    "Trabajadores no cualificados",
    "Trabajadores no cualificados en agricultura, pesca y silvicultura",
    "Trabajadores no cualificados en industria, construcción y transporte"
  ) ~ "No cualificados",
  
  # OTROS
  datos_recodificados$Father.s.occupation %in% c(
    "Estudiante",
    "Otra situación"
    
  ) ~ "Otros",
  
  # MILITAR
  datos_recodificados$Father.s.occupation %in% c(
    "Profesiones de las fuerzas armadas",
    "Oficiales de las fuerzas armadas",
    "Sargentos de las fuerzas armadas",
    "Otro personal de las fuerzas armadas"
  ) ~ "Formación militar"
  
)

sum(table(datos_recodificados$Father_occupation_level))

#probando una cosa del pib:
sort(unique(datos_recodificados$GDP))
sort(unique(datos_recodificados$Unemployment.rate))


unique(datos_recodificados[, c("GDP", "Unemployment.rate")]) |>
  dplyr::arrange(GDP)


library(dplyr)

tabla_años <- data.frame(
  GDP = c(0.32, -3.12, 1.74, -1.70, -4.06, -0.92, 0.79, 1.79, 2.02, 3.51),
  year = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
)

datos_recodificados <- datos_recodificados %>%
  left_join(tabla_años, by = "GDP")

sum(table(datos_recodificados$year))

#Tranformación lineal de nota media de semestres sobre 20 a sobre 10
datos_recodificados$Curricular.units.1st.sem.grade_10<-(datos_recodificados$Curricular.units.1st.sem..grade./2)
datos_recodificados$Curricular.units.2nd.sem.grade_10<-(datos_recodificados$Curricular.units.2nd.sem..grade./2)



#Análisis univariante:

#Variables numéricas:
install.packages("psych")   
library(psych)             

colnames(datos_recodificados)
#Para describir todas las variables numéricas:
variables_numéricas<-c("Previous.qualification.grade_10",
                       "Age.at.enrollment",
                       "Admission.grade_10",
                       "Application.order",
                       "Curricular.units.1st.sem..credited.",
                       "Curricular.units.1st.sem..enrolled.",
                       "Curricular.units.1st.sem..evaluations.",
                       "Curricular.units.1st.sem..approved.",
                       "Curricular.units.1st.sem.grade_10",
                       "Curricular.units.1st.sem..without.evaluations.",
                       "Curricular.units.2nd.sem..credited.",
                       "Curricular.units.2nd.sem..enrolled.",
                       "Curricular.units.2nd.sem..evaluations.",
                       "Curricular.units.2nd.sem..approved.",
                       "Curricular.units.2nd.sem.grade_10",
                       "Curricular.units.2nd.sem..without.evaluations.",
                       "Unemployment.rate",
                       "Inflation.rate",
                       "GDP") 


descriptive(datos_recodificados[, variables_numéricas])


#Describe y descriptivos de cada una por separado
datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0]
comparacion_semestres_y_nota_entrada<-c("Previous.qualification.grade_10",
                                        "Admission.grade_10",
                                        "Curricular.units.1st.sem.grade_10",
                                        "Curricular.units.2nd.sem.grade_10"
)


describe(datos_recodificados[,"Curricular.units.1st.sem.grade_10"])
describe(datos_recodificados[,"Curricular.units.2nd.sem.grade_10"])
describe(datos_recodificados[,"Previous.qualification.grade_10"])
describe(datos_recodificados[,"Admission.grade_10"])
descriptive(datos_recodificados$Curricular.units.1st.sem.grade_10)
descriptive(datos_recodificados$Curricular.units.2nd.sem.grade_10)

descriptive(datos_recodificados$Previous.qualification.grade_10)
descriptive(datos_recodificados[,comparacion_semestres_y_nota_entrada])
descriptive(datos_recodificados$Admission.grade_10)
descriptive(datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0])
descriptive(datos_recodificados$Curricular.units.2nd.sem.grade_10[datos_recodificados$Curricular.units.2nd.sem.grade_10!=0])
descriptive(datos_recodificados$Age.at.enrollment)
descriptive(datos_recodificados$Curricular.units.1st.sem..without.evaluations.)

""
#Boxplots
boxplot(datos_recodificados$Previous.qualification.grade_10,
        yaxt = "n",
        ylab = "Nota",
        main = "Nota estudios previos")
axis(2, at = seq(4, 10, by = 0.5))
boxplot(datos_recodificados$Age.at.enrollment)
boxplot(datos_recodificados$Admission.grade_10, yaxt="n", main="Admission grade")
axis(2, at=seq(4.5,10, by=0.5))
boxplot(datos_recodificados$Application.order)
boxplot(datos_recodificados$Curricular.units.1st.sem..credited.)
boxplot(datos_recodificados$Curricular.units.1st.sem..enrolled., yaxt="n")
axis(2, at=seq(0,25, by=1))
boxplot(datos_recodificados$Curricular.units.1st.sem..evaluations., yaxt="n", main="Grades 1st Sem.")
axis(2, at=seq(0,45, by=3))
boxplot(datos_recodificados$Curricular.units.1st.sem..approved., yaxt="n")
axis(2, at=seq(0,25, by=1))
boxplot(datos_recodificados$Curricular.units.1st.sem.grade_10, yaxt="n")
axis(2, at=seq(0,10, by=1))
boxplot(datos_recodificados$Curricular.units.1st.sem..without.evaluations.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..credited.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..enrolled.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..evaluations.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..approved.)
boxplot(datos_recodificados$Curricular.units.2nd.sem.grade_10)
boxplot(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.)
boxplot(datos_recodificados$Unemployment.rate)
boxplot(datos_recodificados$Inflation.rate)
boxplot(datos_recodificados$PIB)

#ecdf
plot(ecdf(datos_recodificados$Admission.grade_10), las=1, yaxt="n")
axis(2, at=seq(0,1,by=0.1 ))
grid()
plot(ecdf(datos_recodificados$Previous.qualification.grade_10), las=1, yaxt="n")
axis(2, at=seq(0,1,by=0.1 ))
grid()
#papel probabiístico normal
#admission
qqnorm(
  datos_recodificados$Admission.grade_10,
  main = "Papel probabilístico normal Admission Grade",
)
qqline(datos_recodificados$Admission.grade_10)
grid()
#previous qual grade
qqnorm(
  datos_recodificados$Previous.qualification.grade_10,
  main = "Papel probabilístico normal Previous qualification Grade",
)
qqline(datos_recodificados$Previous.qualification.grade_10)
grid()

#age at enrollment

qqnorm(
  datos_recodificados$Age.at.enrollment,
  main = "Papel probabilístico normal Age at enrollment",
)
qqline(datos_recodificados$Age.at.enrollment)
grid()
#curricular 1st sem grades (con 0)
qqnorm(
  datos_recodificados$Curricular.units.1st.sem.grade_10,
  main = "Papel probabilístico normal 1st Sem. Grades",
)
qqline(datos_recodificados$Curricular.units.1st.sem.grade_10)
grid()
#curricular 1st sem grades (sin 0)
qqnorm(
  datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0],
  main = "Papel probabilístico normal 1st Sem. Grades",
)
qqline(datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0])
grid()
#curricular 2nd sem grades (con 0)
qqnorm(
  datos_recodificados$Curricular.units.2nd.sem.grade_10,
  main = "Papel probabilístico normal 2nd Sem. Grades",
)
qqline(datos_recodificados$Curricular.units.2nd.sem.grade_10)
grid()
#curricular 2st sem grades (sin 0)
qqnorm(
  datos_recodificados$Curricular.units.2nd.sem.grade_10[datos_recodificados$Curricular.units.2nd.sem.grade_10!=0],
  main = "Papel probabilístico normal 2nd Sem. Grades",
)
qqline(datos_recodificados$Curricular.units.2nd.sem.grade_10[datos_recodificados$Curricular.units.2nd.sem.grade_10!=0])
grid()
#papel probabiístico exponencial
x_exp <- datos_recodificados$Admission.grade_10 - 4.75 #le restamos 4.75 para q el mínimo sea 0
qqplot(
  qexp(ppoints(length(x_exp)), rate = 1/mean(x_exp)),
  sort(x_exp),
  main = "QQ plot exponencial",
  xlab = "Cuantiles teóricos",
  ylab = "Cuantiles observados"
)

abline(0,1)
#Estudio de las variables dew notas de 1 y 2 sem
#1SEM
#histograma
hist(datos_recodificados$Curricular.units.1st.sem.grade_10, breaks=seq(0, 10, by=0.5), xaxt="n")
axis(1, at=seq(0,10, by=0.5))
hist(datos_recodificados$Previous.qualification.grade_10, breaks=seq(0, 10, by=0.5), xaxt="n")
axis(1, at=seq(0,10, by=0.5))
hist(datos_recodificados$Admission.grade_10, breaks=seq(0, 10, by=0.5), xaxt="n")
axis(1, at=seq(0,10, by=0.5))
#media de solo los alumnos q se han presentado
median(datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0])
mean(datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0])

#2 SEM
hist(datos_recodificados$Curricular.units.2nd.sem.grade_10, breaks=seq(0, 10, by=0.5), xaxt="n")
axis(1, at=seq(0,10, by=0.5))

median(datos_recodificados$Curricular.units.2nd.sem.grade_10[datos_recodificados$Curricular.units.2nd.sem.grade_10!=0])
mean(datos_recodificados$Curricular.units.2nd.sem.grade_10[datos_recodificados$Curricular.units.2nd.sem.grade_10!=0])



sum(datos_recodificados$Curricular.units.1st.sem..without.evaluations.)



#***********************
#Aprobadas
#***********************
#1SEM
#tiene q ver las q apruebas con a cuantas te presentas?
datos_recodificados$Curricular.units.1st.sem..approved.
descriptive(datos_recodificados$Curricular.units.1st.sem..approved.)
boxplot(datos_recodificados$Curricular.units.1st.sem..approved.~ datos_recodificados$Course, las=2)
#otra vez salen los 0 raros de multimedia



#vamos a ver si dpendn del año (aunq me paree raro q puedan aprobar)
ggplot(datos_recodificados, 
       aes(x=factor(PIB), y=Curricular.units.1st.sem..approved.)
)+
  geom_boxplot()+
  facet_wrap(~Course)+
  labs(x="PIB", y="Unidades curriculares aprobadas", title="Unidades curriculares aprobadas en cada año según la carrera")


#Creo nueva variable
datos_recodificados$Porcentaje_aprobado_sem_1<-100*(datos_recodificados$Curricular.units.1st.sem..approved./datos_recodificados$Curricular.units.1st.sem..evaluations.)
descriptive(datos_recodificados$Porcentaje_aprobado_sem_1)
boxplot(datos_recodificados$Porcentaje_aprobado_sem_1~ datos_recodificados$Course, las=2, cex.axis=0.6) #hay q bajar tamaño letra
par(mar=c(13,4,4,2))
#aproxmamos a normal
qqnorm(
  datos_recodificados$Curricular.units.1st.sem..approved.,
  main = "Papel probabilístico normal Aprobados 1º sem.",
)
qqline(datos_recodificados$Curricular.units.1st.sem..approved.)
grid()

#2SEM
descriptive(datos_recodificados$Curricular.units.2nd.sem..approved.)
boxplot(datos_recodificados$Curricular.units.1st.sem..approved.~ datos_recodificados$Course, las=2, cex.axis=0.6)

#****************************
#Asignaturas matriculadas
#****************************
#1SEM
datos_recodificados$Curricular.units.1st.sem..enrolled.
descriptive(datos_recodificados$Curricular.units.1st.sem..enrolled.)
hist(datos_recodificados$Curricular.units.1st.sem..enrolled., col="pink", breaks=seq(0, 26, by=1), xaxt="n", yaxt="n")
axis(1, at=seq(0, 26, by=1),las=2)
axis(2, at=seq(0, 2000, by=100), las=2)
boxplot(datos_recodificados$Curricular.units.1st.sem..enrolled.~datos_recodificados$Course, las=2, cex.axis=0.7)

#ver si los q no se matriculan a nada en el semestre 1 tampoco lo hacen en el segundo
datos_recodificados$Curricular.units.2nd.sem..enrolled.[datos_recodificados$Curricular.units.1st.sem..enrolled.==0]
#probamos ppn x si acaso

table(
  datos_recodificados$Target[
    datos_recodificados$Course=="Diseño de Animación y Multimedia" &
      datos_recodificados$Curricular.units.1st.sem..enrolled. == 0
  ]
)
#2SEM
descriptive(datos_recodificados$Curricular.units.2nd.sem..enrolled.)
hist(datos_recodificados$Curricular.units.2nd.sem..enrolled., col="pink", breaks=seq(0, 26, by=1), xaxt="n", yaxt="n")
axis(1, at=seq(0, 26, by=1),las=2)
axis(2, at=seq(0, 2000, by=100), las=2)
boxplot(datos_recodificados$Curricular.units.2nd.sem..enrolled.~datos_recodificados$Course, las=2, cex.axis=0.7)

#**************************
#Creditadas(convalidadas)
#**************************
#1SEM
descriptive(datos_recodificados$Curricular.units.1st.sem..credited.)
#ver cual es la frecuencia de 0 
tabla_cred<-table(datos_recodificados$Curricular.units.1st.sem..credited.)
tabla_cred
barplot(tabla_cred)
#ppexp
x_exp <- datos_recodificados$Curricular.units.1st.sem..credited.
qqplot(
  qexp(ppoints(length(x_exp)), rate = 1/mean(x_exp)),
  sort(x_exp),
  main = "QQ plot exponencial",
  xlab = "Cuantiles teóricos",
  ylab = "Cuantiles observados"
)
abline(0,1)
grid()
#BXPLOT Por carreras
boxplot(datos_recodificados$Curricular.units.1st.sem..credited.~datos_recodificados$Course, las=2)

#2SEM
descriptive(datos_recodificados$Curricular.units.2nd.sem..credited.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..credited.~datos_recodificados$Course, las=2)
tabla_cred2<-table(datos_recodificados$Curricular.units.2nd.sem..credited.)
tabla_cred2
barplot(tabla_cred2)



#Variables categóricas:

#Nacionality_group
freq_nacionality_group <- table(datos_recodificados$Nationality_group)
freq_nacionality_group
bp2 <- barplot(
  freq_nacionality_group,
  col = c("lightblue", "lightgreen", "khaki", "gold"),
  las = 1,
  main = "Nacionalidad",
  xlab = "Frecuencias absolutas (n)",
  ylim = c(0, max(freq_nacionality_group) * 1.3)
)
text(bp2, freq_nacionality_group, labels = freq_nacionality_group, pos = 3)

  #tabla de frecuencias
table(datos_recodificados$Nationality_group, datos_recodificados$Target)
prop.table(table(datos_recodificados$Nationality_group, datos_recodificados$Target), margin = 1)

library(dplyr)

datos_recodificados %>%
  group_by(Nationality_group, Target) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

round(prop.table(table(datos_recodificados$Nationality_group, datos_recodificados$Target), 1), 3)


#tuitons fees up to date
freq_tuition <- table(datos_recodificados$Tuition.fees.up.to.date)
freq_tuition 
bp3 <- barplot(
  freq_tuition,
  col = c("indianred", "lightgreen"),
  las = 1,
  main = "Tasas de mátricula actualizadas",
  xlab = "Frecuencias absolutas (n)",
  ylim = c(0, max(freq_tuition) * 1.3)
)
text(bp3, freq_tuition, labels = freq_tuition, pos = 3)

#tabla de frecuencias
table(datos_recodificados$Tuition.fees.up.to.date, datos_recodificados$Target)
prop.table(table(datos_recodificados$Tuition.fees.up.to.date, datos_recodificados$Target), margin = 1)

library(dplyr)

datos_recodificados %>%
  group_by(Tuition.fees.up.to.date, Target) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

round(prop.table(table(datos_recodificados$Tuition.fees.up.to.date, datos_recodificados$Target), 1), 3)

#Course

freq_course <- table(datos_recodificados$Course)
freq_course
#como son muchas titulaciones, hacemos un diagrama de barras horizonatal
library(ggplot2)
library(dplyr)

datos_recodificados %>%
  count(Course) %>% 
  ggplot(aes(x = n, y = Course)) +
  geom_bar(stat = "identity", fill = "#4C72B0") +
  labs(
    title = "Frecuencia de estudiantes por curso",
    x = "Frecuencia",
    y = "Curso"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold")
  )


#tabla de frecuencias
table(datos_recodificados$Course, datos_recodificados$Target)
prop.table(table(datos_recodificados$Course, datos_recodificados$Target), margin = 1)

library(dplyr)

datos_recodificados %>%
  group_by(Course, Target) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

round(prop.table(table(datos_recodificados$Course, datos_recodificados$Target), 1), 3)


#t-test:
t.test(Curricular.units.1st.sem.grade_10 ~ Target_bin, data=datos_recodificados)
t.test(Curricular.units.2nd.sem.grade_10 ~ Target_bin, data=datos_recodificados)
t.test(GDP ~ Target_bin, data=datos_recodificados)
t.test(Unemployment.rate ~ Target_bin, data=datos_recodificados)
t.test(Inflation.rate ~ Target_bin, data=datos_recodificados)
t.test(Admission.grade_10  ~ Target_bin, data=datos_recodificados)
t.test(Previous.qualification.grade_10  ~ Target_bin, data=datos_recodificados)





#Boxplots de la variable target después de la reagrupación
boxplot(Admission.grade_10 ~ Target_bin, data=datos_recodificados, las=1)
boxplot(Previous.qualification.grade_10 ~ Target_bin, data=datos_recodificados, las=1)
boxplot(Curricular.units.1st.sem.grade_10 ~ Target_bin, data=datos_recodificados, las=1)
boxplot(Curricular.units.2nd.sem.grade_10 ~ Target_bin, data=datos_recodificados, las=1)
boxplot(GDP ~ Target_bin, data=datos_recodificados, las=1)
boxplot(Unemployment.rate ~ Target_bin, data=datos_recodificados, las=1)
boxplot(Inflation.rate ~ Target_bin, data=datos_recodificados, las=1)




#ANÁLISIS BIVARIANTE

#Categóricas vs Target:
library(clickR)
library(vcd)
library(lsr)
library(rcompanion)
library(dplyr)
library(ggplot2)
library(scales)



descriptive(datos_modelo)
datos_modelo <- datos_recodificados %>%
  filter(!(Curricular.units.1st.sem.grade_10 == 0 &
             Curricular.units.1st.sem..approved. == 0 &
             Curricular.units.1st.sem..evaluations. == 0 &
             Curricular.units.1st.sem..credited. ==0 &
             Curricular.units.1st.sem..enrolled. ==0 
  ))

nrow(datos_recodificados)
nrow(datos_modelo) #vemos que si que encaja, da 4244, 4244=4424-180
colnames(datos_modelo)



#ANÁLISIS BIVARIANTE

#CATEGÓRICA VS TARGET
names(datos_modelo)[sapply(datos_modelo, is.character)] #nombres de las variables categóricas


#Reagrupacion variable Target:
datos_modelo$Target_bin <- ifelse(datos_modelo$Target == "Dropout", "Abandono", "No Abandono")
datos_modelo$Target_bin <- as.factor(datos_modelo$Target_bin)

#Reagrupación de Marital_Status:
datos_modelo <- datos_modelo %>%
  mutate(Marital_group = case_when(
    Marital.status == "Soltero" ~ "Soltero",
    Marital.status %in% c("Casado", "Con pareja") ~ "Pareja",
    Marital.status %in% c("Divorciado", "Separado legalmente", "Viudo") ~ "Otros"
  ))
sum(table(datos_modelo$Marital_group))

#Proporciones:
table(datos_modelo$Marital_group, datos_modelo$Target_bin) 
prop.table(table(datos_modelo$Marital_group, datos_modelo$Target_bin), 1)
prop.table(table(datos_modelo$Marital_group, datos_modelo$Target_bin), 2)

#V de Cramer y Tau
cramersV(table(datos_modelo$Marital_group, datos_modelo$Target_bin))
GK_assoc(datos_modelo$Marital_group, datos_modelo$Target_bin) 
GK_assoc(datos_modelo$Target_bin, datos_modelo$Marital_group) 


#Chi-cuadrado con Marital_group:
tabla <- table(datos_modelo$Marital_group, datos_modelo$Target_bin)
chisq.test(tabla)
chisq.test(tabla)$expected

#Gráfico mosaico:
mosaic(~ Marital_group + Target_bin, data = datos_modelo, 
       shade = TRUE, legend = TRUE, cex.axis = 0.7)


#Daytime.evening.attendance:
sum(table(datos_modelo$Daytime.evening.attendance.))
unique(datos_modelo$Daytime.evening.attendance.)

#Proporciones:
table(datos_modelo$Daytime.evening.attendance., datos_modelo$Target_bin)
prop.table(table(datos_modelo$Daytime.evening.attendance., datos_modelo$Target_bin), 1)
prop.table(table(datos_modelo$Daytime.evening.attendance., datos_modelo$Target_bin), 2)

#Cramer y Tau:
cramersV(table(datos_modelo$Daytime.evening.attendance., datos_modelo$Target_bin))
GK_assoc(datos_modelo$Daytime.evening.attendance., datos_modelo$Target_bin) 
GK_assoc(datos_modelo$Target_bin, datos_modelo$Daytime.evening.attendance.) 

#Chi-cuadrado:
tabla_daytime_target <- table(datos_modelo$Daytime.evening.attendance., datos_modelo$Target_bin)
chisq.test(tabla_daytime_target, correct=FALSE)
chisq.test(tabla_daytime_target)$expected

#Gráfico:

tabla_plot <- datos_modelo %>%                  
  count(Daytime.evening.attendance., Target_bin) %>%
  group_by(Daytime.evening.attendance.) %>%
  mutate(prop = n / sum(n))

ggplot(tabla_plot, 
       aes(x = Daytime.evening.attendance., y = prop, fill = Target_bin)) +
  geom_col(position = "dodge") +
  labs(
    x = "Turno",
    y = "Proporción",
    fill = "Abandono"
  ) +
  theme_minimal()




#Displaced

sum(table(datos_modelo$Displaced))
unique(datos_modelo$Displaced)

#Proporciones:
table(datos_modelo$Displaced, datos_modelo$Target_bin)
prop.table(table(datos_modelo$Displaced, datos_modelo$Target_bin), 1)
prop.table(table(datos_modelo$Displaced, datos_modelo$Target_bin), 2)

#Cramer y Tau:
cramersV(table(datos_modelo$Displaced, datos_modelo$Target_bin))
GK_assoc(datos_modelo$Displaced, datos_modelo$Target_bin) 
GK_assoc(datos_modelo$Target_bin, datos_modelo$Displaced) 

#Chi-cuadrado:
tabla_displaced_target <- table(datos_modelo$Displaced, datos_modelo$Target_bin)
chisq.test(tabla_displaced_target, correct=FALSE) #Quitamos el criterio de correccion que aplica R automáticamente en las tablas 2x2
chisq.test(tabla_displaced_target)$expected

#Gráfico:
mosaic(~ Displaced + Target_bin, data = datos_modelo,  #he elegido esta
       shade = TRUE, legend = TRUE)





#Tuition fees up to date:

sum(table(datos_modelo$Tuition.fees.up.to.date))
unique(datos_modelo$Tuition.fees.up.to.date)

#Proporciones:
table(datos_modelo$Tuition.fees.up.to.date, datos_modelo$Target_bin)
prop.table(table(datos_modelo$Tuition.fees.up.to.date, datos_modelo$Target_bin), 1)
prop.table(table(datos_modelo$Tuition.fees.up.to.date, datos_modelo$Target_bin), 2)

#Cramer y Tau:
cramersV(table(datos_modelo$Tuition.fees.up.to.date, datos_modelo$Target_bin))
GK_assoc(datos_modelo$Tuition.fees.up.to.date, datos_modelo$Target_bin) 
GK_assoc(datos_modelo$Target_bin, datos_modelo$Tuition.fees.up.to.date) 

#Chi-cuadrado:
tabla_tution_target <- table(datos_modelo$Tuition.fees.up.to.date, datos_modelo$Target_bin)
chisq.test(tabla_tution_target, correct=FALSE) #Quitamos el criterio de correccion que aplica R automáticamente en las tablas 2x2
chisq.test(tabla_tution_target)$expected

#Gráficos:
mosaic(~ Tuition.fees.up.to.date + Target_bin, data = datos_modelo,  
       shade = TRUE, legend = TRUE)



tabla_plot <- datos_modelo %>%
  count(Tuition.fees.up.to.date, Target_bin) %>%
  group_by(Tuition.fees.up.to.date) %>%
  mutate(prop = n / sum(n),
         etiqueta = percent(prop, accuracy = 0.1))

ggplot(tabla_plot, aes(x = Tuition.fees.up.to.date, y = prop, fill = Target_bin)) +
  geom_col(position = "fill") +
  geom_text(aes(label = etiqueta),
            position = position_fill(vjust = 0.5),
            size = 4) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Turno",
    y = "Proporción",
    fill = "Abandono",
    title = "Relación entre Matrícula al día y Abandono"
  ) +
  theme_minimal()



#Debtor:

sum(table(datos_modelo$Debtor))
unique(datos_modelo$Debtor)
#Proporciones:
table(datos_modelo$Debtor, datos_modelo$Target_bin)
prop.table(table(datos_modelo$Debtor, datos_modelo$Target_bin), 1)
prop.table(table(datos_modelo$Debtor, datos_modelo$Target_bin), 2)

#Cramer y Tau:
cramersV(table(datos_modelo$Debtor, datos_modelo$Target_bin))
GK_assoc(datos_modelo$Debtor, datos_modelo$Target_bin) 
GK_assoc(datos_modelo$Target_bin, datos_modelo$Debtor) 

#Chi-cuadrado:
tabla_debtor_target <- table(datos_modelo$Debtor, datos_modelo$Target_bin)
chisq.test(tabla_debtor_target, correct=FALSE) #Quitamos el criterio de correccion que aplica R automáticamente en las tablas 2x2
chisq.test(tabla_debtor_target)$expected

#Gráficos:
mosaic(~ Debtor + Target_bin, data = datos_modelo,  
       shade = TRUE, legend = TRUE)


tabla_debtor_plot <- datos_modelo %>%
  count(Debtor, Target_bin) %>%
  group_by(Debtor) %>%
  mutate(
    prop = n / sum(n),
    etiqueta = percent(prop, accuracy = 0.1)
  )

ggplot(tabla_debtor_plot, aes(x = Debtor, y = prop, fill = Target_bin)) +
  geom_col(position = "fill") +
  geom_text(aes(label = etiqueta),
            position = position_fill(vjust = 0.5),
            size = 4) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Debe dinero",
    y = "Proporción",
    fill = "Abandono",
    title = "Relación entre Deudor y Abandono"
  ) +
  theme_minimal()


#Scholarship_holder:

sum(table(datos_modelo$Scholarship.holder))
unique(datos_modelo$Scholarship.holder)
#Proporciones:
table(datos_modelo$Scholarship.holder, datos_modelo$Target_bin)
prop.table(table(datos_modelo$Scholarship.holder, datos_modelo$Target_bin), 1)
prop.table(table(datos_modelo$Scholarship.holder, datos_modelo$Target_bin), 2)

#Cramer y Tau:
cramersV(table(datos_modelo$Scholarship.holder, datos_modelo$Target_bin))
GK_assoc(datos_modelo$Scholarship.holder, datos_modelo$Target_bin) 
GK_assoc(datos_modelo$Target_bin, datos_modelo$Scholarship.holder) 

#Chi-cuadrado:
tabla_beca_target <- table(datos_modelo$Scholarship.holder, datos_modelo$Target_bin)
chisq.test(tabla_beca_target, correct=FALSE) #Quitamos el criterio de correccion que aplica R automáticamente en las tablas 2x2
chisq.test(tabla_beca_target)$expected

#Gráficos:
mosaic(~ Scholarship.holder + Target_bin, data = datos_modelo,  
       shade = TRUE, legend = TRUE)





#Educational_special_needs:
sum(table(datos_modelo$Educational.special.needs))
unique(datos_modelo$Educational.special.needs)

#Proporciones:
table(datos_modelo$Educational.special.needs, datos_modelo$Target_bin)
prop.table(table(datos_modelo$Educational.special.needs, datos_modelo$Target_bin), 1)
prop.table(table(datos_modelo$Educational.special.needs, datos_modelo$Target_bin), 2)

#Cramer y Tau:
cramersV(table(datos_modelo$Educational.special.needs, datos_modelo$Target_bin))
GK_assoc(datos_modelo$Educational.special.needs, datos_modelo$Target_bin) 
GK_assoc(datos_modelo$Target_bin, datos_modelo$Educational.special.needs) 

#Chi-cuadrado:
tabla_necesidades_target <- table(datos_modelo$Educational.special.needs, datos_modelo$Target_bin)
chisq.test(tabla_necesidades_target, correct=FALSE) #Quitamos el criterio de correccion que aplica R automáticamente en las tablas 2x2
chisq.test(tabla_necesidades_target)$expected




#International:
sum(table(datos_modelo$International))
unique(datos_modelo$International)

#Proporciones:
tabla_internacional_target <- table(datos_modelo$International,
                                    datos_modelo$Target_bin)
tabla_internacional_target

prop.table(tabla_internacional_target, 1)
prop.table(tabla_internacional_target, 2)

#Cramer y Tau:
cramersV(tabla_internacional_target)
GK_assoc(datos_modelo$International, datos_modelo$Target_bin)
GK_assoc(datos_modelo$Target_bin, datos_modelo$International)

#Chi-cuadrado:
chisq.test(tabla_internacional_target, correct = FALSE)
chisq.test(tabla_internacional_target)$expected

#Gráfico:
table(datos_modelo$International)

tabla_internacional_plot <- datos_modelo %>%
  count(International, Target_bin) %>%
  group_by(International) %>%
  mutate(prop = n / sum(n),
         etiqueta = scales::percent(prop, accuracy = 0.1))

ggplot(tabla_internacional_plot, aes(x = International, y = prop, fill = Target_bin)) +
  geom_col(position = "fill") +
  geom_text(aes(label = etiqueta),
            position = position_fill(vjust = 0.5),
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Estudiante internacional",
    y = "Proporción",
    fill = "Abandono",
    title = "Relación entre Estudiantes internacionales y Abandono"
  ) +
  theme_minimal()





#Application_mode:


#Reagrupación application_mode:

table(datos_modelo$Application.mode)


datos_modelo$Application.mode_group <- case_when(
  
  # ACCESO NORMAL
  datos_modelo$Application.mode %in% c(
    "1ª fase - cupo general",
    "2ª fase - cupo general",
    "3ª fase - cupo general",
    "1ª fase - cupo especial (Isla de Madeira)",
    "1ª fase - cupo especial (Islas Azores)"
  ) ~ "Acceso normal",
  
  # MAYORES/ESPECIALES
  datos_modelo$Application.mode %in% c(
    "Mayores de 23 años",
    "Ordenanza nº 533-A/99, apartado b2 (plan diferente)",
    "Ordenanza nº 533-A/99, apartado b3 (otra institución)",
    "Ordenanza nº 612/93",
    "Ordenanza nº 854-B/99"
  ) ~ "Acceso mayores/especiales",
  
  # CAMBIO/TRASLADO
  datos_modelo$Application.mode %in% c(
    "Cambio de institución/titulación",
    "Cambio de titulación",
    "Traslado",
    "Cambio de institución/titulación (internacional)"
  ) ~ "Acceso por cambio/traslado",
  
  # FORMACIÓN PREVIA
  datos_modelo$Application.mode %in% c(
    "Titulares de diploma de ciclo corto",
    "Titulares de diploma de especialización tecnológica",
    "Titulares de otros estudios superiores"
  ) ~ "Acceso por formación previa",
  
  # INTERNACIONAL
  datos_modelo$Application.mode %in% c(
    "Estudiante internacional (grado)"
    
  ) ~ "Acceso internacional",
  
)





sum(table(datos_modelo$Application.mode_group))
tabla_modo_app_target <- table(datos_modelo$Application.mode_group,
                               datos_modelo$Target_bin)
tabla_modo_app_target

prop.table(tabla_modo_app_target, 1)
prop.table(tabla_modo_app_target, 2)

cramersV(tabla_modo_app_target)
GK_assoc(datos_modelo$Application.mode_group, datos_modelo$Target_bin)
GK_assoc(datos_modelo$Target_bin, datos_modelo$Application.mode_group)

chisq.test(tabla_modo_app_target, correct = FALSE)
chisq.test(tabla_modo_app_target)$expected



#Gráficos:
tabla_modo_app_plot <- datos_modelo %>%
  count(Application.mode_group, Target_bin) %>%
  group_by(Application.mode_group) %>%
  mutate(prop = n / sum(n),
         etiqueta = scales::percent(prop, accuracy = 0.1))

ggplot(tabla_modo_app_plot, aes(x = Application.mode_group, y = prop, fill = Target_bin)) +
  geom_col(position = "fill") +
  geom_text(aes(label = etiqueta),
            position = position_fill(vjust = 0.5),
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Tipo de acceso al grado",
    y = "Proporción",
    fill = "Abandono",
    title = "Relación entre Tipo de acceso al grado y Abandono"
  ) +
  theme_minimal()



#Género:
unique(datos_modelo$Gender)
tabla_genero_target <- table(datos_modelo$Gender,
                             datos_modelo$Target_bin)
tabla_genero_target

prop.table(tabla_genero_target, 1)
prop.table(tabla_genero_target, 2)

cramersV(tabla_genero_target)
GK_assoc(datos_modelo$Gender, datos_modelo$Target_bin)
GK_assoc(datos_modelo$Target_bin, datos_modelo$Gender)

chisq.test(tabla_genero_target, correct = FALSE)
chisq.test(tabla_genero_target)$expected
#Gráfico:
mosaic(~ Gender + Target_bin, data = datos_modelo,  
       shade = TRUE, legend = TRUE)



#Course
#Vamosa realizar una reagrupacion de las carreras para que el estudio se fiable:

datos_modelo$Course_group <- dplyr::case_when(
  
  # SALUD
  datos_modelo$Course_limpio %in% c(
    "Enfermería",
    "Enfermería Veterinaria",
    "Higiene Bucodental"
  ) ~ "Salud",
  
  # INGENIERÍA / TECNOLOGÍA
  datos_modelo$Course_limpio %in% c(
    "Ingeniería Informática",
    "Tecnologías de Producción de Biocombustibles",
    "Diseño de Animación y Multimedia"
  ) ~ "Ingeniería/Tech",
  
  # SOCIALES / EMPRESA
  datos_modelo$Course_limpio %in% c(
    "Gestión",
    "Gestión de Publicidad y Marketing",
    "Turismo"
  ) ~ "Empresa",
  
  # EDUCACIÓN / SOCIAL
  datos_modelo$Course_limpio %in% c(
    "Educación Básica",
    "Trabajo Social"
  ) ~ "Educación/Social",
  
  # COMUNICACIÓN / DISEÑO
  datos_modelo$Course_limpio %in% c(
    "Diseño de Comunicación",
    "Periodismo y Comunicación"
  ) ~ "Comunicación",
  
  # AGRO / ANIMAL
  datos_modelo$Course_limpio %in% c(
    "Agronomía",
    "Equinocultura"
  ) ~ "Agro/Animal",
  
  TRUE ~ NA_character_
)
table(datos_modelo$Course_limpio)

sum(table(datos_modelo$Course_group))
unique(datos_modelo$Course_limpio)
tabla_course_target <- table(datos_modelo$Course_group,
                             datos_modelo$Target_bin)
tabla_course_target

prop.table(tabla_course_target, 1)
prop.table(tabla_course_target, 2)

cramersV(tabla_course_target)
GK_assoc(datos_modelo$Course_group, datos_modelo$Target_bin)
GK_assoc(datos_modelo$Target_bin, datos_modelo$Course_group)

chisq.test(tabla_course_target, correct = FALSE)
chisq.test(tabla_course_target)$expected

#Simplemente cambiamos los nombres a más cortos para que en el gráfico se entienda
datos_modelo$Course_group_short <- dplyr::recode(
  datos_modelo$Course_group,
  "Ingeniería/Tech" = "Ing./Tech",
  "Educación/Social" = "Educ./Soc.",
  "Comunicación" = "Com.",
  "Agro/Animal" = "Agro/Anim.",
  "Empresa" = "Empresa",
  "Salud" = "Salud"
)

mosaic(~ Course_group_short + Target_bin, 
       data = datos_modelo,
       shade = TRUE,
       legend = TRUE,
       cex.axis = 0.8)  




tabla_course_plot <- datos_modelo %>%
  count(Course_group, Target_bin) %>%
  group_by(Course_group) %>%
  mutate(prop = n / sum(n),
         etiqueta = scales::percent(prop, accuracy = 0.1))

ggplot(tabla_course_plot, aes(x = Course_group, y = prop, fill = Target_bin)) +
  geom_col(position = "fill") +
  geom_text(aes(label = etiqueta),
            position = position_fill(vjust = 0.5),
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Tipo de carrera",
    y = "Proporción",
    fill = "Abandono",
    title = "Relación entre Tipo de carrera y Abandono"
  ) +
  theme_minimal()


#Previous education level
table(datos_modelo$Previous_education_level)
unique(datos_modelo$Previous_education_level)
tabla_prev_edu_target <- table(datos_modelo$Previous_education_level,
                               datos_modelo$Target_bin)
tabla_prev_edu_target

prop.table(tabla_prev_edu_target, 1)
prop.table(tabla_prev_edu_target, 2)

cramersV(tabla_prev_edu_target)
GK_assoc(datos_modelo$Previous_education_level, datos_modelo$Target_bin)
GK_assoc(datos_modelo$Target_bin, datos_modelo$Previous_education_level)

chisq.test(tabla_prev_edu_target, correct = FALSE)
chisq.test(tabla_prev_edu_target)$expected


#Cambiamos a nombres más cortos:
datos_modelo$Previous_education_level_group_short <- dplyr::recode(
  datos_modelo$Previous_education_level,
  "Bajo"= "Bajo",
  "Medio"= "Medio",
  "Superior"= "Sup.",
  "Técnico"= "Técn."
)
#Gráfico:
mosaic(~ Previous_education_level_group_short + Target_bin, data = datos_modelo,  
       shade = TRUE, legend = TRUE)


#Mother education level:
table(datos_modelo$Mother_education_level)
unique(datos_modelo$Mother_education_level)
tabla_mum_educ_target <- table(datos_modelo$Mother_education_level,
                               datos_modelo$Target_bin)
tabla_mum_educ_target

prop.table(tabla_mum_educ_target, 1)
prop.table(tabla_mum_educ_target, 2)

cramersV(tabla_mum_educ_target)
GK_assoc(datos_modelo$Mother_education_level, datos_modelo$Target_bin)
GK_assoc(datos_modelo$Target_bin, datos_modelo$Mother_education_level)

chisq.test(tabla_mum_educ_target, correct = FALSE)
chisq.test(tabla_mum_educ_target)$expected

#Cambiamos a nombres más cortos:
datos_modelo$Mother_education_level_group_short <- dplyr::recode(
  datos_modelo$Mother_education_level,
  "Bajo"= "Bajo",
  "Medio"= "Medio",
  "Superior"= "Sup.",
  "Técnico"= "Técn."
)
#Gráfico:
mosaic(~ Mother_education_level_group_short + Target_bin, data = datos_modelo,  
       shade = TRUE, legend = TRUE)


#Father education level:
table(datos_modelo$Father_education_level)
unique(datos_modelo$Father_education_level)
tabla_dad_educ_target <- table(datos_modelo$Father_education_level,
                               datos_modelo$Target_bin)
tabla_dad_educ_target

prop.table(tabla_dad_educ_target, 1)
prop.table(tabla_dad_educ_target, 2)

cramersV(tabla_dad_educ_target)
GK_assoc(datos_modelo$Father_education_level, datos_modelo$Target_bin)
GK_assoc(datos_modelo$Target_bin, datos_modelo$Father_education_level)

chisq.test(tabla_dad_educ_target, correct = FALSE)
chisq.test(tabla_dad_educ_target)$expected
#Cambiamos a nombres más cortos:
datos_modelo$Father_education_level_group_short <- dplyr::recode(
  datos_modelo$Father_education_level,
  "Bajo"= "Bajo",
  "Medio"= "Medio",
  "Superior"= "Sup.",
  "Técnico"= "Técn."
)
#Gráfico:
mosaic(~ Father_education_level_group_short + Target_bin, data = datos_modelo,  
       shade = TRUE, legend = TRUE)


#Mother occupation level:

sum(table(datos_modelo$Mother_occupation_level))
unique(datos_modelo$Mother_occupation_level)
tabla_mum_ocup_target <- table(datos_modelo$Mother_occupation_level,
                               datos_modelo$Target_bin)
tabla_mum_ocup_target

prop.table(tabla_mum_ocup_target, 1)
prop.table(tabla_mum_ocup_target, 2)

cramersV(tabla_mum_ocup_target)
GK_assoc(datos_modelo$Mother_occupation_level, datos_modelo$Target_bin)
GK_assoc(datos_modelo$Target_bin, datos_modelo$Mother_occupation_level)

chisq.test(tabla_mum_ocup_target, correct = FALSE)
chisq.test(tabla_mum_ocup_target)$expected
#Gráfico:
mosaic(~ Mother_occupation_level + Target_bin, data = datos_modelo,  
       shade = TRUE, legend = TRUE)


#Father occupation level:
table(datos_modelo$Father_occupation_level)
unique(datos_modelo$Father_occupation_level)
tabla_dad_ocup_target <- table(datos_modelo$Father_occupation_level,
                               datos_modelo$Target_bin)
tabla_dad_ocup_target

prop.table(tabla_dad_ocup_target, 1)
prop.table(tabla_dad_ocup_target, 2)

cramersV(tabla_dad_ocup_target)
GK_assoc(datos_modelo$Father_occupation_level, datos_modelo$Target_bin)
GK_assoc(datos_modelo$Target_bin, datos_modelo$Father_occupation_level)

chisq.test(tabla_dad_ocup_target, correct = FALSE)
chisq.test(tabla_dad_ocup_target)$expected

#Gráficos:
mosaic(~ Father_occupation_level + Target_bin, data = datos_modelo,  
       shade = TRUE, legend = TRUE)



tabla_course_plot <- datos_modelo %>%
  count(Course_group, Target_bin) %>%
  group_by(Course_group) %>%
  mutate(prop = n / sum(n),
         etiqueta = scales::percent(prop, accuracy = 0.1))

ggplot(tabla_course_plot, aes(x = Course_group, y = prop, fill = Target_bin)) +
  geom_col(position = "fill") +
  geom_text(aes(label = etiqueta),
            position = position_fill(vjust = 0.5),
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Tipo de carrera",
    y = "Proporción",
    fill = "Abandono",
    title = "Relación entre Tipo de carrera y Abandono"
  ) +
  theme_minimal()


#Pongo aquí el análisis de sensibilidad:

#Analizamos Mother_education_level sin los datos imputados para ver como cambia respecto a la imputación
datos_sensibilidad <- datos_sin_imputar %>%
  filter(!(Curricular.units.1st.sem..grade. == 0 &
             Curricular.units.1st.sem..approved. == 0 &
             Curricular.units.1st.sem..evaluations. == 0 &
             Curricular.units.1st.sem..credited. ==0 &
             Curricular.units.1st.sem..enrolled. ==0 
  ))
nrow(datos_sensibilidad)


# Recodificamos  la variable:



datos_sensibilidad$Mother.s.qualification <- recode(
  datos_sensibilidad$Mother.s.qualification,
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
  `44` = "Educación superior - Doctorado (3er ciclo)"
)

table(datos_sensibilidad$Mother.s.qualification)
datos_sensibilidad$Mother_education_level <- case_when(
  
  # BAJO
  datos_sensibilidad$Mother.s.qualification %in% c(
    "10º año de escolarización", 
    "11º año de escolarización - No completado",
    "12º año de escolarización - No completado",
    "7º año (sistema antiguo)",
    "7º año de escolarización",
    "8º año de escolarización",
    "9º año de escolarización - No completado",
    "Educación básica 1er ciclo (4º/5º año) o equivalente",
    "Educación básica 2º ciclo (6º/7º/8º año) o equivalente",
    "Educación básica 3er ciclo (9º/10º/11º año) o equivalente",
    "Otro - 11º año de escolarización",
    "Sabe leer sin haber completado 4º año",
    "No sabe leer ni escribir"
  ) ~ "Bajo",
  
  # MEDIO
  datos_sensibilidad$Mother.s.qualification %in% c(
    "Educación secundaria - 12º año o equivalente",
    "2º ciclo del bachillerato general"
  ) ~ "Medio",
  
  # TÉCNICO
  datos_sensibilidad$Mother.s.qualification %in% c(
    "Curso de especialización tecnológica",
    "Curso de estudios superiores especializados",
    "Curso técnico-profesional",
    "Curso técnico superior profesional",
    "Curso general de comercio"
  ) ~ "Técnico",
  
  # SUPERIOR
  datos_sensibilidad$Mother.s.qualification %in% c(
    "Educación superior - Doctorado",
    "Educación superior - Doctorado (3er ciclo)",
    "Educación superior - Grado",
    "Educación superior - Grado (Bachelor)",
    "Educación superior - Máster",
    "Educación superior - Máster (2º ciclo)",
    "Educación superior - Grado (1er ciclo)",
    "Asistencia a educación superior"
  ) ~ "Superior",
  
  TRUE ~ NA_character_
)


datos_sensibilidad$Target_bin <- ifelse(
  datos_sensibilidad$Target == "Dropout",
  "Abandono",
  "No Abandono"
)
datos_sensibilidad$Target_bin <- as.factor(datos_sensibilidad$Target_bin)

sum(table(datos_sensibilidad$Mother_education_level)) #nos da 4115, debería dar 4414 pero puede que uno de los multimedia eliminados fuera tambien faltante
#Lo comprobamos

problematicos <- datos_sin_imputar %>%
  filter(
    Curricular.units.1st.sem..grade. == 0 &
      Curricular.units.1st.sem..approved. == 0 &
      Curricular.units.1st.sem..evaluations. == 0 &
      Curricular.units.1st.sem..credited. == 0 &
      Curricular.units.1st.sem..enrolled. == 0
  )

sum(problematicos$Mother.s.qualification == "34", na.rm = TRUE) #en efecto, uno de los problemáticos (De multimedia), tenia NA
#aqui lo podemos ver:
View(
  datos_sin_imputar %>%
    filter(
      Mother.s.qualification == 34 &
        Course == 171
    )
)

nrow(datos_sensibilidad)
sum(table(datos_sensibilidad$Mother_education_level))
sum(is.na(datos_sensibilidad$Mother_education_level))


tabla_mum_educ_target_sin_imputar <- table(
  datos_sensibilidad$Mother_education_level,
  datos_sensibilidad$Target_bin
)
sum(tabla_mum_educ_target_sin_imputar)

prop.table(tabla_mum_educ_target_sin_imputar, 1)
prop.table(tabla_mum_educ_target_sin_imputar, 2)

cramersV(tabla_mum_educ_target_sin_imputar)
GK_assoc(datos_sensibilidad$Mother_education_level, datos_sensibilidad$Target_bin)
GK_assoc(datos_sensibilidad$Target_bin, datos_sensibilidad$Mother_education_level)

chisq.test(tabla_mum_educ_target_sin_imputar, correct = FALSE)
chisq.test(tabla_mum_educ_target_sin_imputar)$expected

# Cambiamos a nombres más cortos:
datos_sensibilidad$Mother_education_level_group_short <- dplyr::recode(
  datos_sensibilidad$Mother_education_level,
  "Bajo" = "Bajo",
  "Medio" = "Medio",
  "Superior" = "Sup.",
  "Técnico" = "Técn."
)

# Gráfico:
mosaic(~ Mother_education_level_group_short + Target_bin,
       data = datos_sensibilidad,
       shade = TRUE, legend = TRUE)


# Analizamos Father_education_level sin imputados

# Recodificamos primero la variable:
datos_sensibilidad$Father.s.qualification<- recode(
  datos_sensibilidad$Father.s.qualification,
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
  `44` = "Educación superior - Doctorado (3er ciclo)"
)

sum(table(datos_sensibilidad$Father.s.qualification))

datos_sensibilidad$Father_education_level <- case_when(
  
  # BAJO
  datos_sensibilidad$Father.s.qualification %in% c(
    "10º año de escolarización", 
    "11º año de escolarización - No completado",
    "12º año de escolarización - No completado",
    "7º año (sistema antiguo)",
    "7º año de escolarización",
    "8º año de escolarización",
    "9º año de escolarización - No completado",
    "Educación básica 1er ciclo (4º/5º año) o equivalente",
    "Educación básica 2º ciclo (6º/7º/8º año) o equivalente",
    "Educación básica 3er ciclo (9º/10º/11º año) o equivalente",
    "Otro - 11º año de escolarización",
    "Sabe leer sin haber completado 4º año",
    "No sabe leer ni escribir"
  ) ~ "Bajo",
  
  # MEDIO
  datos_sensibilidad$Father.s.qualification%in% c(
    "Educación secundaria - 12º año o equivalente",
    "2º ciclo del bachillerato general"
  ) ~ "Medio",
  
  # TÉCNICO
  datos_sensibilidad$Father.s.qualification %in% c(
    "Curso de especialización tecnológica",
    "Curso de estudios superiores especializados",
    "Curso técnico-profesional",
    "Curso técnico superior profesional",
    "Curso general de comercio",
    "Curso general de administración y comercio",
    "Curso complementario de contabilidad y administración",
    "Curso complementario de secundaria",
    "Curso complementario de secundaria - no completado",
    "2º año de curso complementario de secundaria"
  ) ~ "Técnico",
  
  # SUPERIOR
  datos_sensibilidad$Father.s.qualification %in% c(
    "Educación superior - Doctorado",
    "Educación superior - Doctorado (3er ciclo)",
    "Educación superior - Grado",
    "Educación superior - Grado (Bachelor)",
    "Educación superior - Máster",
    "Educación superior - Máster (2º ciclo)",
    "Educación superior - Grado (1er ciclo)",
    "Asistencia a educación superior"
  ) ~ "Superior",
  
  TRUE ~ NA_character_
)

sum(table(datos_sensibilidad$Father_education_level)) #deberia dar 4132, puede ser, que de nuevo, uno de los multimedia fuera faltante

#aqui lo podemos ver:
View(
  datos_sin_imputar %>%
    filter(
      Father.s.qualification == 34 &
        Course == 171
    )
)

nrow(datos_sensibilidad)
sum(table(datos_sensibilidad$Father_education_level))
sum(is.na(datos_sensibilidad$Father_education_level))

tabla_dad_educ_target_sin_imputar <- table(
  datos_sensibilidad$Father_education_level,
  datos_sensibilidad$Target_bin
)
sum(tabla_dad_educ_target_sin_imputar)

prop.table(tabla_dad_educ_target_sin_imputar, 1)
prop.table(tabla_dad_educ_target_sin_imputar, 2)

cramersV(tabla_dad_educ_target_sin_imputar)
GK_assoc(datos_sensibilidad$Father_education_level, datos_sensibilidad$Target_bin)
GK_assoc(datos_sensibilidad$Target_bin, datos_sensibilidad$Father_education_level)

chisq.test(tabla_dad_educ_target_sin_imputar, correct = FALSE)
chisq.test(tabla_dad_educ_target_sin_imputar)$expected

# Cambiamos a nombres más cortos:
datos_sensibilidad$Father_education_level_group_short <- dplyr::recode(
  datos_sensibilidad$Father_education_level,
  "Bajo" = "Bajo",
  "Medio" = "Medio",
  "Superior" = "Sup.",
  "Técnico" = "Técn."
)

# Gráfico:
mosaic(~ Father_education_level_group_short + Target_bin,
       data = datos_sensibilidad,
       shade = TRUE, legend = TRUE)


#Mother occupation sin imputados:

#Analizamos Mother occupation level sin los datos imputados para ver como cambia respecto a la imputación

# Recodificamos primero la variable:
datos_sensibilidad$Mother.s.occupation<-recode(datos_sensibilidad$Mother.s.occupation,
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
                                               `194` = "Ayudantes de preparación de comidas")

sum(table(datos_sensibilidad$Mother.s.occupation))



datos_sensibilidad$Mother_occupation_level <- case_when(
  
  # ALTA CUALIFICACIÓN
  datos_sensibilidad$Mother.s.occupation %in% c(
    "Representantes del poder legislativo y ejecutivo, directores y gerentes",
    "Especialistas en actividades intelectuales y científicas",
    "Especialistas en tecnologías de la información y la comunicación (TIC)",
    "Profesionales de la salud",
    "Profesores"
  ) ~ "Alta cualificación",
  
  # CUALIFICACIÓN MEDIA
  datos_sensibilidad$Mother.s.occupation %in% c(
    "Personal administrativo",
    "Empleados de oficina, secretarios y operadores de datos",
    "Operadores de datos, contabilidad, estadística, servicios financieros y registros",
    "Otro personal de apoyo administrativo",
    "Técnicos y profesiones de nivel intermedio",
    "Técnicos y profesiones intermedias en ciencia e ingeniería",
    "Técnicos y profesionales de nivel intermedio en salud",
    "Técnicos intermedios en servicios jurídicos, sociales, deportivos, culturales y similares"
  ) ~ "Cualificación media",
  
  # BAJA CUALIFICACIÓN
  datos_sensibilidad$Mother.s.occupation %in% c(
    "Trabajadores de servicios personales, seguridad y vendedores",
    "Trabajadores de servicios personales",
    "Vendedores",
    "Trabajadores de cuidado personal y similares",
    "Agricultores y trabajadores cualificados en agricultura, pesca y silvicultura",
    "Trabajadores cualificados de la industria, construcción y artesanos",
    "Trabajadores cualificados de la construcción (excepto electricistas)",
    "Trabajadores cualificados en impresión, instrumentos de precisión, joyería y artesanía",
    "Trabajadores en procesamiento de alimentos, madera, textil y otras industrias",
    "Operadores de instalaciones y maquinaria y trabajadores de montaje",
    "Ayudantes de preparación de comidas"
  ) ~ "Baja cualificación",
  
  # NO CUALIFICADOS
  datos_sensibilidad$Mother.s.occupation %in% c(
    "Trabajadores no cualificados",
    "Trabajadores no cualificados en agricultura, pesca y silvicultura",
    "Trabajadores no cualificados en industria extractiva, construcción, manufactura y transporte",
    "Trabajadores de limpieza"
  ) ~ "No cualificados",
  
  # OTROS
  datos_sensibilidad$Mother.s.occupation %in% c(
    "Estudiante",
    "Otra situación",
    "Profesiones de las fuerzas armadas"
  ) ~ "Otros",
  TRUE ~ NA_character_
  
)


nrow(datos_sensibilidad)
sum(table(datos_sensibilidad$Mother_occupation_level))
sum(is.na(datos_sensibilidad$Mother_occupation_level))


sum(table(datos_sensibilidad$Mother_occupation_level)) #coincide con 4244-17 faltantes
tabla_mum_ocup_target_sin_imputar <- table(datos_sensibilidad$Mother_occupation_level,
                                           datos_sensibilidad$Target_bin)
sum(tabla_mum_ocup_target_sin_imputar)

prop.table(tabla_mum_ocup_target_sin_imputar, 1)
prop.table(tabla_mum_ocup_target_sin_imputar, 2)

cramersV(tabla_mum_ocup_target_sin_imputar)
GK_assoc(datos_sensibilidad$Mother_occupation_level, datos_sensibilidad$Target_bin)
GK_assoc(datos_sensibilidad$Target_bin, datos_sensibilidad$Mother_occupation_level)

chisq.test(tabla_mum_ocup_target_sin_imputar, correct = FALSE)
chisq.test(tabla_mum_ocup_target_sin_imputar)$expected


#Gráfico:
mosaic(~ Mother_occupation_level + Target_bin, data = datos_sensibilidad,  
       shade = TRUE, legend = TRUE)


#Analizamos Father occupation level sin los datos imputados para ver como cambia respecto a la imputación

# Recodificamos primero la variable:
datos_sensibilidad$Father.s.occupation<-recode(datos_sensibilidad$Father.s.occupation,
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

sum(table(datos_sensibilidad$Father.s.occupation))


datos_sensibilidad$Father_occupation_level <- case_when(
  
  # ALTA CUALIFICACIÓN
  datos_sensibilidad$Father.s.occupation %in% c(
    "Representantes del poder legislativo y ejecutivo, directores y gerentes",
    "Directores de servicios administrativos y comerciales",
    "Directores de hostelería, comercio y otros servicios",
    "Especialistas en actividades intelectuales y científicas",
    "Especialistas en ciencias físicas, matemáticas, ingeniería y afines",
    "Especialistas en finanzas, contabilidad, organización administrativa y relaciones públicas/comerciales",
    "Profesionales de la salud",
    "Profesores"
  ) ~ "Alta cualificación",
  
  # CUALIFICACIÓN MEDIA
  datos_sensibilidad$Father.s.occupation %in% c(
    "Personal administrativo",
    "Empleados de oficina, secretarios y operadores de datos",
    "Operadores de datos, contabilidad, estadística y servicios financieros",
    "Otro personal de apoyo administrativo",
    "Técnicos en tecnologías de la información y la comunicación",
    "Técnicos y profesiones de nivel intermedio",
    "Técnicos intermedios en ciencia e ingeniería",
    "Técnicos y profesionales intermedios de salud",
    "Técnicos intermedios en servicios jurídicos, sociales, deportivos y culturales"
  ) ~ "Cualificación media",
  
  # BAJA CUALIFICACIÓN
  datos_sensibilidad$Father.s.occupation %in% c(
    "Trabajadores de servicios personales, seguridad y vendedores",
    "Trabajadores de servicios personales",
    "Vendedores",
    "Vendedores ambulantes (excepto alimentos) y servicios callejeros",
    "Trabajadores de cuidado personal y similares",
    "Personal de protección y seguridad",
    "Agricultores y trabajadores cualificados en agricultura, pesca y silvicultura",
    "Agricultores orientados al mercado y trabajadores agrícolas cualificados",
    "Agricultores de subsistencia, pescadores, cazadores y recolectores",
    "Trabajadores cualificados de la industria, construcción y artesanía",
    "Trabajadores cualificados de la construcción (excepto electricistas)",
    "Trabajadores cualificados en metalurgia y trabajo del metal",
    "Trabajadores cualificados en electricidad y electrónica",
    "Trabajadores en alimentación, madera, textil y otras industrias",
    "Operadores de instalaciones y maquinaria y trabajadores de montaje",
    "Operadores de instalaciones y maquinaria fija",
    "Conductores de vehículos y operadores de maquinaria móvil",
    "Trabajadores de montaje",
    "Ayudantes de preparación de comidas"
  ) ~ "Baja cualificación",
  
  # NO CUALIFICADOS
  datos_sensibilidad$Father.s.occupation %in% c(
    "Trabajadores no cualificados",
    "Trabajadores no cualificados en agricultura, pesca y silvicultura",
    "Trabajadores no cualificados en industria, construcción y transporte"
  ) ~ "No cualificados",
  
  # OTROS
  datos_sensibilidad$Father.s.occupation %in% c(
    "Estudiante",
    "Otra situación"
    
  ) ~ "Otros",
  
  # MILITAR
  datos_sensibilidad$Father.s.occupation %in% c(
    "Profesiones de las fuerzas armadas",
    "Oficiales de las fuerzas armadas",
    "Sargentos de las fuerzas armadas",
    "Otro personal de las fuerzas armadas"
  ) ~ "Formación militar",
  
  TRUE ~ NA_character_
)


nrow(datos_sensibilidad)
sum(table(datos_sensibilidad$Father_occupation_level))
sum(is.na(datos_sensibilidad$Father_occupation_level))


sum(table(datos_sensibilidad$Father_occupation_level)) #coincide con 4244-19 faltantes
tabla_dad_ocup_target_sin_imputar <- table(datos_sensibilidad$Father_occupation_level,
                                           datos_sensibilidad$Target_bin)
sum(tabla_dad_ocup_target_sin_imputar)

prop.table(tabla_dad_ocup_target_sin_imputar, 1)
prop.table(tabla_dad_ocup_target_sin_imputar, 2)

cramersV(tabla_dad_ocup_target_sin_imputar)
GK_assoc(datos_sensibilidad$Father_occupation_level, datos_sensibilidad$Target_bin)
GK_assoc(datos_sensibilidad$Target_bin, datos_sensibilidad$Father_occupation_level)

chisq.test(tabla_dad_ocup_target_sin_imputar, correct = FALSE)
chisq.test(tabla_dad_ocup_target_sin_imputar)$expected


#Gráfico:
mosaic(~ Father_occupation_level + Target_bin, data = datos_sensibilidad,  
       shade = TRUE, legend = TRUE)



#limpiando variable Course
table(datos_recodificados$Course_limpio)
datos_recodificados$Course_limpio <- gsub(" \\(turno de tarde\\)", "", 
                                          datos_recodificados$Course)
table(datos_recodificados$Course_limpio)


aggregate(Curricular.units.1st.sem..evaluations. ~ Course_limpio, 
          data = datos_recodificados, 
          mean)


aggregate(Curricular.units.1st.sem..evaluations. ~ Course_limpio,
          data = datos_recodificados,
          sd)

boxplot(Curricular.units.1st.sem..evaluations. ~ Course_limpio,
        data = datos_recodificados,
        las = 2,
        col = "lightblue",
        main = "Evaluaciones por carrera",
        ylab = "Número de evaluaciones")

#Entendiendo las matriculaciones :)
table(datos_recodificados$Curricular.units.1st.sem..enrolled.)

#Ver si los alumnos con más matriculaciones tienen tambien más convalidadas
View(datos_recodificados[
  order(-datos_recodificados$Curricular.units.1st.sem..enrolled.),
  c("Curricular.units.1st.sem..enrolled.",
    "Curricular.units.1st.sem..credited.")
])

#Creo nueva variable con la carga real de los estudiantes, es decir, asignaturas matriculadas-convaldiadas

datos_recodificados$Carga_academica_real <- 
  datos_recodificados$Curricular.units.1st.sem..enrolled. - 
  datos_recodificados$Curricular.units.1st.sem..credited.

table(datos_recodificados$Carga_academica_real)
descriptive(datos_recodificados$Carga_academica_real)


View(datos_recodificados[
  datos_recodificados$Carga_academica_real==0,
  c("Carga_academica_real",
    "Course_limpio",
    "Target",
    "Curricular.units.1st.sem.grade_10",
    "Curricular.units.1st.sem..evaluations.",
    "Curricular.units.1st.sem..without.evaluations.",
    "Curricular.units.1st.sem..enrolled.",
    "Application.mode_group"
  )
])

datos_recodificados$Curricular.units.1st.sem..enrolled.

datos_recodificados$Carga_academica_real_sem2 <- 
  datos_recodificados$Curricular.units.2nd.sem..enrolled. - 
  datos_recodificados$Curricular.units.2nd.sem..credited.

View(datos_recodificados[
  datos_recodificados$Carga_academica_real_sem2==0,
  c("Carga_academica_real_sem2",
    "Course_limpio",
    "Target",
    "Curricular.units.2nd.sem.grade_10",
    "Curricular.units.2nd.sem..evaluations.",
    "Curricular.units.2nd.sem..without.evaluations.",
    "Curricular.units.2nd.sem..enrolled.",
    "Application.mode_group"
  )
])

 
#vamos a ver que estudiantes tienen carga_academica_real==carga_academia_real_sem2

View(datos_recodificados[
  datos_recodificados$Carga_academica_real == 0 &
    datos_recodificados$Carga_academica_real_sem2 == 0,
  c("Carga_academica_real",
    "Carga_academica_real_sem2",
    "Course_limpio",
    "Target",
    "Curricular.units.2nd.sem..evaluations.")
])

#Viendo los alumnos multimedia:

View(datos_recodificados[
  datos_recodificados$Course=="Diseño de Animación y Multimedia",
  c("Course",
    "year",
    "Target",
    "Curricular.units.1st.sem..evaluations.",
    "Curricular.units.1st.sem..enrolled.",
    "Carga_academica_real",
    "Carga_academica_real_sem2",
    "Application.mode_group"
  )
])
datos_recodificados$Application.mode_group

#Análisis multivariante:
install.packages("pscl")
#Regresión logística binaria:
# Aseguramos que la variable objetivo esté en formato factor
datos_recodificados$Target_bin <- relevel(datos_recodificados$Target_bin, ref = "No Dropout")
datos_recodificados$Target_bin <- as.factor(datos_recodificados$Target_bin)

# Ajustamos el modelo de regresión logística binaria
modelo_logit <- glm(
  Target_bin ~ Curricular.units.1st.sem.grade_10 +
    Tuition.fees.up.to.date +
    Scholarship.holder +
    Debtor +
    Admission.grade_10 +
    Application.mode_group +
    Course_group,
  data = datos_recodificados,
  family = binomial
)
colnames(datos_recodificados)

summary(modelo_logit)
exp(coef(modelo_logit))
exp(confint(modelo_logit))
library(pscl)
pR2(modelo_logit)
datos_recodificados$Target_bin <- factor(
  datos_recodificados$Target_bin,
  levels = c("No Dropout", "Dropout")
)
levels(datos_recodificados$Target_bin)
rm(modelo_logit)
#no lo he acabado, no hacer caos


#Poner datos para análisis numérico (sin los estudiantes de Multimedia con todo 0 (pero si los que tienen notas)):

datos_recodificados$Curricular.units.1st.sem..evaluations.

library(dplyr)

datos_modelo <- datos_recodificados %>%
  filter(!(Curricular.units.1st.sem.grade_10 == 0 &
             Curricular.units.1st.sem..approved. == 0 &
             Curricular.units.1st.sem..evaluations. == 0 &
             Curricular.units.1st.sem..credited. ==0 &
             Curricular.units.1st.sem..enrolled. ==0 
             ))

nrow(datos_recodificados)
nrow(datos_modelo) #vemos que si que encaja, da 4244, 4244=4424-180
colnames(datos_modelo)

table(datos_modelo$Course_limpio)


