#imputación de datos faltantes por moda condicionada:

#he añadido todas las librerías que creo q hemos usado hasta ahora
library(gridExtra)
library(clickR)
library(DescTools)
library(plotly)
library(scatterplot3d)
library(vcd)
library(ggplot2)
library(dplyr)
library(GGally)
library(psych)

datos_sin_imputar <- read.csv("estudiantes.csv", header=TRUE, sep=";")

datos_moda_condicionada<- read.csv("estudiantes.csv", header = TRUE, sep = ";")
#Chicas añado el cambio del nombre de variable de Nacionality a Nationality
datos_moda_condicionada$Nationality<-datos_moda_condicionada$Nacionality
datos_moda_condicionada$Nacionality<-NULL


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



#Recodificación de variables:

install.packages("dplyr")
library(dplyr)
datos_recodificados<-datos_moda_condicionada

#Nacionality:

datos_recodificados$Nationality<-recode(datos_recodificados$Nationality, 
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



#Reagrupaciones y transformaciones lineales:

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

