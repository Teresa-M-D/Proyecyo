# Instalar paquetes solo si no están instalados
paquetes <- c(
  "dplyr", "ggplot2", "DescTools", "clickR", "corrplot", "gridExtra", "coin",
  "plotly", "scatterplot3d", "vcd", "lsr", "GGally", "psych", "scales", "sf", "rnaturalearth", "rnaturalearthdata", "car", "pROC", "rstatix", "caret"
)

#Librerías a usar:
library(corrplot)
library(gridExtra)
library(clickR)
library(DescTools)
library(plotly)
library(scatterplot3d)
library(vcd)
library(lsr)
library(ggplot2)
library(dplyr)
library(GGally)
library(psych)
library(scales)
library(readxl)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(car)
library(pROC)
library(caret)
library(rstatix)
library(coin)
#CARGA DE DATOS:

datos_sin_imputar <- read.csv("estudiantes.csv", header=TRUE, sep=";")

datos_moda_condicionada<- read.csv("estudiantes.csv", header = TRUE, sep = ";")

#Cambio nombre Nacionality a Nationality:
datos_moda_condicionada$Nationality<-datos_moda_condicionada$Nacionality
datos_moda_condicionada$Nacionality<-NULL
#Renombramos GDP como PIB:
datos_moda_condicionada["PIB"]=datos_moda_condicionada["GDP"]
datos_moda_condicionada$GDP<-NULL



#IMPUTACIÓN DE VALORES DESCONOCIDOS MEDIANTE MODA CONDICIONADA:

# Convertimos los códigos de desconocido a NA
datos_moda_condicionada$Mother.s.qualification[datos_moda_condicionada$Mother.s.qualification == 34] <- NA
datos_moda_condicionada$Father.s.qualification[datos_moda_condicionada$Father.s.qualification == 34] <- NA
datos_moda_condicionada$Mother.s.occupation[datos_moda_condicionada$Mother.s.occupation == 99] <- NA
datos_moda_condicionada$Father.s.occupation[datos_moda_condicionada$Father.s.occupation == 99] <- NA

#Función para calcular la moda
moda <- function(x) {
  ux <- na.omit(x)
  ux[which.max(tabulate(match(ux, ux)))]
}


#Función para imputar por moda condicionada al Target:

imputar_moda_por_target <- function(variable, target) {
  ave(
    variable,
    target,
    FUN = function(x) {
      valor_moda <- moda(x)
      x[is.na(x)] <- valor_moda
      return(x)
    }
  )
}


# Imputación por moda condicionada a Target
datos_moda_condicionada$Mother.s.occupation <- imputar_moda_por_target(
  datos_moda_condicionada$Mother.s.occupation,
  datos_moda_condicionada$Target
)

datos_moda_condicionada$Father.s.occupation <- imputar_moda_por_target(
  datos_moda_condicionada$Father.s.occupation,
  datos_moda_condicionada$Target
)

datos_moda_condicionada$Mother.s.qualification <- imputar_moda_por_target(
  datos_moda_condicionada$Mother.s.qualification,
  datos_moda_condicionada$Target
)

datos_moda_condicionada$Father.s.qualification <- imputar_moda_por_target(
  datos_moda_condicionada$Father.s.qualification,
  datos_moda_condicionada$Target
)

# Comprobación de valores faltantes tras la imputación
colSums(is.na(datos_moda_condicionada))


#RECODIFICACIÓN DE VARIABLES CATEGÓRICAS:

datos_recodificados<-datos_moda_condicionada

#Nacionality:

datos_recodificados$Nationality<-dplyr::recode(datos_recodificados$Nationality, 
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



#Course
datos_recodificados$Course<-dplyr::recode(datos_recodificados$Course, 
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


#Limpieza de Course:
datos_recodificados$Course_limpio <- gsub(
  " \\(turno de tarde\\)",
  "",
  datos_recodificados$Course
)


#Mother's Cualification
datos_recodificados$Mother.s.qualification<-dplyr::recode(datos_recodificados$Mother.s.qualification,
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
datos_recodificados$Father.s.qualification <- dplyr::recode(datos_recodificados$Father.s.qualification,
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
datos_recodificados$Mother.s.occupation<-dplyr::recode(datos_recodificados$Mother.s.occupation,
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
datos_recodificados$Father.s.occupation<-dplyr::recode(datos_recodificados$Father.s.occupation,
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

datos_recodificados$Displaced<-dplyr::recode(datos_recodificados$Displaced,
                                      `1` = "Sí",
                                      `0` = "No")


#Education special needs:
datos_recodificados$Educational.special.needs<-dplyr::recode(datos_recodificados$Educational.special.needs,
                                                      `1` = "Sí",
                                                      `0` = "No")

#Debtor:
datos_recodificados$Debtor<-dplyr::recode(datos_recodificados$Debtor,
                                   `1` = "Sí",
                                   `0` = "No")

#Tution feets up:
datos_recodificados$Tuition.fees.up.to.date<-dplyr::recode(datos_recodificados$Tuition.fees.up.to.date,
                                                    `1` = "Sí",
                                                    `0` = "No")

#International:
datos_recodificados$International<-dplyr::recode(datos_recodificados$International,
                                          `1` = "Sí",
                                          `0` = "No")

#Gender:
datos_recodificados$Gender<-dplyr::recode(datos_recodificados$Gender,
                                   `1` = "Masculino",
                                   `0` = "Femenino")

#Scholarship holder:
datos_recodificados$Scholarship.holder<-dplyr::recode(datos_recodificados$Scholarship.holder,
                                               `1` = "Sí",
                                               `0` = "No")

#Marital Status:
datos_recodificados$Marital.status<-dplyr::recode(datos_recodificados$Marital.status,
                                           `1` = "Soltero",
                                           `2` = "Casado",
                                           `3` = "Viudo",
                                           `4` = "Divorciado",
                                           `5` = "Con pareja",
                                           `6` = "Separado legalmente"
)


#Application mode:
datos_recodificados$Application.mode<-dplyr::recode(datos_recodificados$Application.mode,
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
datos_recodificados$Previous.qualification<-dplyr::recode(datos_recodificados$Previous.qualification,
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
datos_recodificados$Daytime.evening.attendance.<-dplyr::recode(datos_recodificados$Daytime.evening.attendance.,
                                                        `1` = "Mañana",
                                                        `0` = "Tarde")



#TRANSFORMACIONES LINEALES:

# Nota de estudios previos sobre 10
datos_recodificados$Previous.qualification.grade_10 <- 
  datos_recodificados$Previous.qualification..grade. / 20

# Nota de admisión sobre 10
datos_recodificados$Admission.grade_10 <- 
  datos_recodificados$Admission.grade / 20

# Nota media del primer semestre sobre 10
datos_recodificados$Curricular.units.1st.sem.grade_10 <- 
  datos_recodificados$Curricular.units.1st.sem..grade. / 2

# Nota media del segundo semestre sobre 10
datos_recodificados$Curricular.units.2nd.sem.grade_10 <- 
  datos_recodificados$Curricular.units.2nd.sem..grade. / 2



#REAGRUPACIÓN DE VARIABLES CATEGÓRICAS:

#Reagrupación nacionalidad:

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

sum(table(datos_recodificados$Nationality_group))

#Marital Status:
datos_recodificados <- datos_recodificados %>%
  mutate(Marital_group = case_when(
    Marital.status == "Soltero" ~ "Soltero",
    Marital.status %in% c("Casado", "Con pareja") ~ "En pareja",
    Marital.status %in% c("Divorciado", "Separado legalmente", "Viudo") ~ "Otros"
  ))
sum(table(datos_recodificados$Marital_group))

#Application mode:

datos_recodificados$Application.mode_group <- case_when(
  
  # ACCESO NORMAL
  datos_recodificados$Application.mode %in% c(
    "1ª fase - cupo general",
    "2ª fase - cupo general",
    "3ª fase - cupo general",
    "1ª fase - cupo especial (Isla de Madeira)",
    "1ª fase - cupo especial (Islas Azores)"
  ) ~ "Acceso normal",
  
  # MAYORES/ESPECIALES
  datos_recodificados$Application.mode %in% c(
    "Mayores de 23 años",
    "Ordenanza nº 533-A/99, apartado b2 (plan diferente)",
    "Ordenanza nº 533-A/99, apartado b3 (otra institución)",
    "Ordenanza nº 612/93",
    "Ordenanza nº 854-B/99"
  ) ~ "Acceso mayores/especiales",
  
  # CAMBIO/TRASLADO
  datos_recodificados$Application.mode %in% c(
    "Cambio de institución/titulación",
    "Cambio de titulación",
    "Traslado",
    "Cambio de institución/titulación (internacional)"
  ) ~ "Acceso por cambio/traslado",
  
  # FORMACIÓN PREVIA
  datos_recodificados$Application.mode %in% c(
    "Titulares de diploma de ciclo corto",
    "Titulares de diploma de especialización tecnológica",
    "Titulares de otros estudios superiores"
  ) ~ "Acceso por formación previa",
  
  # INTERNACIONAL
  datos_recodificados$Application.mode %in% c(
    "Estudiante internacional (grado)"
    
  ) ~ "Acceso internacional",
  
)

sum(table(datos_recodificados$Application.mode_group))

#Course:
datos_recodificados$Course_group <- dplyr::case_when(
  
  # SALUD
  datos_recodificados$Course_limpio %in% c(
    "Enfermería",
    "Enfermería Veterinaria",
    "Higiene Bucodental"
  ) ~ "Salud",
  
  # INGENIERÍA / TECNOLOGÍA
  datos_recodificados$Course_limpio %in% c(
    "Ingeniería Informática",
    "Tecnologías de Producción de Biocombustibles",
    "Diseño de Animación y Multimedia"
  ) ~ "Ingeniería/Tech",
  
  # SOCIALES / EMPRESA
  datos_recodificados$Course_limpio %in% c(
    "Gestión",
    "Gestión de Publicidad y Marketing",
    "Turismo"
  ) ~ "Empresa",
  
  # EDUCACIÓN / SOCIAL
  datos_recodificados$Course_limpio %in% c(
    "Educación Básica",
    "Trabajo Social"
  ) ~ "Educación/Social",
  
  # COMUNICACIÓN / DISEÑO
  datos_recodificados$Course_limpio %in% c(
    "Diseño de Comunicación",
    "Periodismo y Comunicación"
  ) ~ "Comunicación",
  
  # AGRO / ANIMAL
  datos_recodificados$Course_limpio %in% c(
    "Agronomía",
    "Equinocultura"
  ) ~ "Agro/Animal",
  
  TRUE ~ NA_character_
)

sum(table(datos_recodificados$Course_group))


#Previous qualification:
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

sum(table(datos_recodificados$Previous_education_level))

#Mother qualification
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

sum(table(datos_recodificados$Mother_education_level))

#Father qualification
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
sum(table(datos_recodificados$Father_education_level))


#Mother occupation
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

#Father occupation

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


#CREACIÓN DE NUEVAS VARIABLES:


#Porcentaje de evaluaciones aprobadas: 
datos_recodificados$Porcentaje_aprobado_sem_1<-100*(datos_recodificados$Curricular.units.1st.sem..approved./datos_recodificados$Curricular.units.1st.sem..evaluations.)
datos_recodificados$Porcentaje_aprobado_sem_2<-100*(datos_recodificados$Curricular.units.2nd.sem..approved./datos_recodificados$Curricular.units.2nd.sem..evaluations.)

#Carga académica real:
datos_recodificados$Carga_academica_real <- 
  datos_recodificados$Curricular.units.1st.sem..enrolled. - 
  datos_recodificados$Curricular.units.1st.sem..credited.

datos_recodificados$Carga_academica_real_sem_2 <- 
  datos_recodificados$Curricular.units.2nd.sem..enrolled. - 
  datos_recodificados$Curricular.units.2nd.sem..credited.


#AÑO ASOCIADO AL PIB:

tabla_años <- data.frame(
  PIB = c(0.32, -3.12, 1.74, -1.70, -4.06, -0.92, 0.79, 1.79, 2.02, 3.51),
  year = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
)

datos_recodificados$year <- tabla_años$year[match(datos_recodificados$PIB, tabla_años$PIB)]



#ELIMINACIÓN DE REGISTROS PROBLEMÁTICOS:



#Eliminación de observaciones del grado Multimedia sin actividad académica:


datos_modelo <- datos_recodificados %>%
  filter(!(Curricular.units.1st.sem.grade_10 == 0 &
             Curricular.units.1st.sem..approved. == 0 &
             Curricular.units.1st.sem..evaluations. == 0 &
             Curricular.units.1st.sem..credited. ==0 &
             Curricular.units.1st.sem..enrolled. ==0 
  ))

#Creación Target_bin:

datos_modelo$Target_bin <- ifelse(
  datos_modelo$Target == "Dropout",
  "Abandono",
  "No Abandono"
)

datos_modelo$Target_bin <- as.factor(datos_modelo$Target_bin)

table(datos_modelo$Target_bin)

# Comprobación de valores faltantes
colSums(is.na(datos_modelo))

# Comprobación de variables agrupadas
table(datos_modelo$Nationality_group, useNA = "ifany")
table(datos_modelo$Marital_group, useNA = "ifany")
table(datos_modelo$Application.mode_group, useNA = "ifany")
table(datos_modelo$Course_group, useNA = "ifany")
table(datos_modelo$Previous_education_level, useNA = "ifany")
table(datos_modelo$Mother_education_level, useNA = "ifany")
table(datos_modelo$Father_education_level, useNA = "ifany")
table(datos_modelo$Mother_occupation_level, useNA = "ifany")
table(datos_modelo$Father_occupation_level, useNA = "ifany")

# Comprobación de la variable objetivo
table(datos_modelo$Target, useNA = "ifany")

# Comprobación de titulaciones limpias
table(datos_modelo$Course_limpio, useNA = "ifany")

# Vista general final
str(datos_modelo)
summary(datos_modelo)

descriptive(datos_modelo)

#Estudiantes con actividad durante todo el año:

datos_modelo <- datos_modelo %>%
  mutate(
    tipo_actividad = case_when(
      # Sin actividad en todo el año
      Curricular.units.1st.sem..approved. == 0 &
        Curricular.units.1st.sem..evaluations. == 0 &
        Curricular.units.2nd.sem..approved. == 0 &
        Curricular.units.2nd.sem..evaluations. == 0 ~ "Sin actividad en todo el año",
      
      # Sin actividad solo en 1º semestre
      Curricular.units.1st.sem..approved. == 0 &
        Curricular.units.1st.sem..evaluations. == 0 ~ "Sin actividad en 1º semestre",
      
      # Sin actividad solo en 2º semestre
      Curricular.units.2nd.sem..approved. == 0 &
        Curricular.units.2nd.sem..evaluations. == 0 ~ "Sin actividad en 2º semestre",
      
      # Con actividad
      TRUE ~ "Con actividad"
    )
  )
con_actividad_total <- subset(datos_modelo, tipo_actividad == "Con actividad")
con_actividad_total <- subset(datos_modelo, tipo_actividad == "Con actividad")
sin_actividad_total <- subset(datos_modelo, tipo_actividad == "Sin actividad en todo el año")
no_presentados_1    <- subset(datos_modelo, tipo_actividad == "Sin actividad en 1º semestre")
no_presentados_2    <- subset(datos_modelo, tipo_actividad == "Sin actividad en 2º semestre")

table(con_actividad_total$Target_bin)
table(sin_actividad_total$Target_bin)
table(no_presentados_1$Target_bin)
table(no_presentados_2$Target_bin)




#Recodificación y reagrupación de order application (orden de solicitud):

datos_modelo <- datos_modelo %>%
  mutate(
    # Corregimos el valor 0:
    # como solo aparece una vez, lo agrupamos con el valor 1.
    Application.order_corr = if_else(
      Application.order == 0,
      1,
      Application.order
    ),
    
    # Creamos la variable categórica interpretando 1 como primera opción
    Application.order_cat = case_when(
      Application.order_corr == 1 ~ "1ª opción",
      Application.order_corr == 2 ~ "2ª opción",
      Application.order_corr == 3 ~ "3ª opción",
      Application.order_corr == 4 ~ "4ª opción",
      Application.order_corr == 5 ~ "5ª opción",
      Application.order_corr == 6 ~ "6ª opción",
      Application.order_corr == 9 ~ "Última opción",
      TRUE ~ NA_character_
    ),
    
    # Factor ordenado para que salga bien en gráficos
    Application.order_cat = factor(
      Application.order_cat,
      levels = c(
        "1ª opción",
        "2ª opción",
        "3ª opción",
        "4ª opción",
        "5ª opción",
        "6ª opción",
        "Última opción"
      )
    )
  )


datos_modelo <- datos_modelo %>%
  mutate(
    Application.order_group = case_when(
      Application.order_corr == 1 ~ "1ª opción",
      Application.order_corr == 2 ~ "2ª opción",
      Application.order_corr == 3 ~ "3ª opción",
      Application.order_corr %in% c(4, 5, 6, 9) ~ "Otras opciones",
      TRUE ~ NA_character_
    ),
    
    Application.order_group = factor(
      Application.order_group,
      levels = c(
        "1ª opción",
        "2ª opción",
        "3ª opción",
        "Otras opciones"
      )
    )
  )

# Comprobaciones
table(datos_modelo$Application.order)
table(datos_modelo$Application.order_corr)
table(datos_modelo$Application.order_group)
prop.table(table(datos_modelo$Application.order_group))


