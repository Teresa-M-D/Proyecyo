#ANÁLISIS BIVARIANTE SIN MULTIMEDIA:

#Categóricas vs Target:
library(clickR)
library(vcd)
install.packages("lsr")
library(lsr)
install.packages("rcompanion")
library(rcompanion)
library(dplyr)
library(ggplot2)
library(scales)


names(datos_sin_multi)[sapply(datos_sin_multi, is.character)] #nombres de las variables categóricas


#Reagrupacion variable Target:
datos_sin_multi$Target_bin <- ifelse(datos_sin_multi$Target == "Dropout", "Abandono", "No Abandno")
datos_sin_multi$Target_bin <- as.factor(datos_sin_multi$Target_bin)

#Reagrupación de Marital_Status:
datos_sin_multi <- datos_sin_multi %>%
  mutate(Marital_group = case_when(
    Marital.status == "Soltero" ~ "Soltero",
    Marital.status %in% c("Casado", "Con pareja") ~ "En pareja",
    Marital.status %in% c("Divorciado", "Separado legalmente", "Viudo") ~ "Otros"
  ))
sum(table(datos_sin_multi$Marital_group))

#Proporciones:
table(datos_sin_multi$Marital_group, datos_sin_multi$Target_bin) 
prop.table(table(datos_sin_multi$Marital_group, datos_sin_multi$Target_bin), 1)
prop.table(table(datos_sin_multi$Marital_group, datos_sin_multi$Target_bin), 2)

#V de Cramer y Tau
cramersV(table(datos_sin_multi$Marital_group, datos_sin_multi$Target_bin))
GK_assoc(datos_sin_multi$Marital_group, datos_sin_multi$Target_bin) 
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Marital_group) 


#Chi-cuadrado con Marital_group:
tabla <- table(datos_sin_multi$Marital_group, datos_sin_multi$Target_bin)
chisq.test(tabla)
chisq.test(tabla)$expected

#Gráfico mosaico:
mosaic(~ Marital_group + Target_bin, data = datos_sin_multi, 
       shade = TRUE, legend = TRUE)


#Daytime.evening.attendance:
sum(table(datos_sin_multi$Daytime.evening.attendance.))
unique(datos_sin_multi$Daytime.evening.attendance.)

#Proporciones:
table(datos_sin_multi$Daytime.evening.attendance., datos_sin_multi$Target_bin)
prop.table(table(datos_sin_multi$Daytime.evening.attendance., datos_sin_multi$Target_bin), 1)
prop.table(table(datos_sin_multi$Daytime.evening.attendance., datos_sin_multi$Target_bin), 2)

#Cramer y Tau:
cramersV(table(datos_sin_multi$Daytime.evening.attendance., datos_sin_multi$Target_bin))
GK_assoc(datos_sin_multi$Daytime.evening.attendance., datos_sin_multi$Target_bin) 
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Daytime.evening.attendance.) 

#Chi-cuadrado:
tabla_daytime_target <- table(datos_sin_multi$Daytime.evening.attendance., datos_sin_multi$Target_bin)
chisq.test(tabla_daytime_target, correct=FALSE)
chisq.test(tabla_daytime_target)$expected

#Gráfico:

tabla_plot <- datos_sin_multi %>%                  
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




# Displaced

sum(table(datos_sin_multi$Displaced))
unique(datos_sin_multi$Displaced)

# Proporciones:
table(datos_sin_multi$Displaced, datos_sin_multi$Target_bin)
prop.table(table(datos_sin_multi$Displaced, datos_sin_multi$Target_bin), 1)
prop.table(table(datos_sin_multi$Displaced, datos_sin_multi$Target_bin), 2)

# Cramer y Tau:
cramersV(table(datos_sin_multi$Displaced, datos_sin_multi$Target_bin))
GK_assoc(datos_sin_multi$Displaced, datos_sin_multi$Target_bin) 
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Displaced) 

# Chi-cuadrado:
tabla_displaced_target <- table(datos_sin_multi$Displaced, datos_sin_multi$Target_bin)
chisq.test(tabla_displaced_target, correct=FALSE)
chisq.test(tabla_displaced_target)$expected

# Gráfico:
mosaic(~ Displaced + Target_bin, data = datos_sin_multi,
       shade = TRUE, legend = TRUE)



# Tuition fees up to date:

sum(table(datos_sin_multi$Tuition.fees.up.to.date))
unique(datos_sin_multi$Tuition.fees.up.to.date)

# Proporciones:
table(datos_sin_multi$Tuition.fees.up.to.date, datos_sin_multi$Target_bin)
prop.table(table(datos_sin_multi$Tuition.fees.up.to.date, datos_sin_multi$Target_bin), 1)
prop.table(table(datos_sin_multi$Tuition.fees.up.to.date, datos_sin_multi$Target_bin), 2)

# Cramer y Tau:
cramersV(table(datos_sin_multi$Tuition.fees.up.to.date, datos_sin_multi$Target_bin))
GK_assoc(datos_sin_multi$Tuition.fees.up.to.date, datos_sin_multi$Target_bin) 
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Tuition.fees.up.to.date) 

# Chi-cuadrado:
tabla_tution_target <- table(datos_sin_multi$Tuition.fees.up.to.date, datos_sin_multi$Target_bin)
chisq.test(tabla_tution_target, correct=FALSE)
chisq.test(tabla_tution_target)$expected

# Gráficos:
mosaic(~ Tuition.fees.up.to.date + Target_bin, data = datos_sin_multi,  
       shade = TRUE, legend = TRUE)

tabla_plot <- datos_sin_multi %>%
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
    x = "Matrícula al día",
    y = "Proporción",
    fill = "Abandono",
    title = "Relación entre Matrícula al día y Abandono"
  ) +
  theme_minimal()


# Debtor:

sum(table(datos_sin_multi$Debtor))
unique(datos_sin_multi$Debtor)

# Proporciones:
table(datos_sin_multi$Debtor, datos_sin_multi$Target_bin)
prop.table(table(datos_sin_multi$Debtor, datos_sin_multi$Target_bin), 1)
prop.table(table(datos_sin_multi$Debtor, datos_sin_multi$Target_bin), 2)

# Cramer y Tau:
cramersV(table(datos_sin_multi$Debtor, datos_sin_multi$Target_bin))
GK_assoc(datos_sin_multi$Debtor, datos_sin_multi$Target_bin) 
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Debtor) 

# Chi-cuadrado:
tabla_debtor_target <- table(datos_sin_multi$Debtor, datos_sin_multi$Target_bin)
chisq.test(tabla_debtor_target, correct=FALSE)
chisq.test(tabla_debtor_target)$expected

# Gráficos:
mosaic(~ Debtor + Target_bin, data = datos_sin_multi,  
       shade = TRUE, legend = TRUE)

tabla_debtor_plot <- datos_sin_multi %>%
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
    x = "Deudor",
    y = "Proporción",
    fill = "Abandono",
    title = "Relación entre Deudor y Abandono"
  ) +
  theme_minimal()



# Scholarship_holder:

sum(table(datos_sin_multi$Scholarship.holder))
unique(datos_sin_multi$Scholarship.holder)

# Proporciones:
table(datos_sin_multi$Scholarship.holder, datos_sin_multi$Target_bin)
prop.table(table(datos_sin_multi$Scholarship.holder, datos_sin_multi$Target_bin), 1)
prop.table(table(datos_sin_multi$Scholarship.holder, datos_sin_multi$Target_bin), 2)

# Cramer y Tau:
cramersV(table(datos_sin_multi$Scholarship.holder, datos_sin_multi$Target_bin))
GK_assoc(datos_sin_multi$Scholarship.holder, datos_sin_multi$Target_bin) 
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Scholarship.holder) 

# Chi-cuadrado:
tabla_beca_target <- table(datos_sin_multi$Scholarship.holder, datos_sin_multi$Target_bin)
chisq.test(tabla_beca_target, correct=FALSE)
chisq.test(tabla_beca_target)$expected

# Gráficos:
mosaic(~ Scholarship.holder + Target_bin, data = datos_sin_multi,  
       shade = TRUE, legend = TRUE)



# Educational_special_needs:

sum(table(datos_sin_multi$Educational.special.needs))
unique(datos_sin_multi$Educational.special.needs)

# Proporciones:
table(datos_sin_multi$Educational.special.needs, datos_sin_multi$Target_bin)
prop.table(table(datos_sin_multi$Educational.special.needs, datos_sin_multi$Target_bin), 1)
prop.table(table(datos_sin_multi$Educational.special.needs, datos_sin_multi$Target_bin), 2)

# Cramer y Tau:
cramersV(table(datos_sin_multi$Educational.special.needs, datos_sin_multi$Target_bin))
GK_assoc(datos_sin_multi$Educational.special.needs, datos_sin_multi$Target_bin) 
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Educational.special.needs) 

# Chi-cuadrado:
tabla_necesidades_target <- table(datos_sin_multi$Educational.special.needs, datos_sin_multi$Target_bin)
chisq.test(tabla_necesidades_target, correct=FALSE)
chisq.test(tabla_necesidades_target)$expected



# International:

sum(table(datos_sin_multi$International))
unique(datos_sin_multi$International)

# Proporciones:
tabla_internacional_target <- table(datos_sin_multi$International,
                                    datos_sin_multi$Target_bin)
tabla_internacional_target

prop.table(tabla_internacional_target, 1)
prop.table(tabla_internacional_target, 2)

# Cramer y Tau:
cramersV(tabla_internacional_target)
GK_assoc(datos_sin_multi$International, datos_sin_multi$Target_bin)
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$International)

# Chi-cuadrado:
chisq.test(tabla_internacional_target, correct = FALSE)
chisq.test(tabla_internacional_target)$expected

# Gráfico:
table(datos_sin_multi$International)

tabla_internacional_plot <- datos_sin_multi %>%
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




# Application_mode:

# Reagrupación application_mode:

table(datos_sin_multi$Application.mode)

datos_sin_multi$Application.mode_group <- case_when(
  
  # ACCESO NORMAL
  datos_sin_multi$Application.mode %in% c(
    "1ª fase - cupo general",
    "2ª fase - cupo general",
    "3ª fase - cupo general",
    "1ª fase - cupo especial (Isla de Madeira)",
    "1ª fase - cupo especial (Islas Azores)"
  ) ~ "Acceso normal",
  
  # MAYORES/ESPECIALES
  datos_sin_multi$Application.mode %in% c(
    "Mayores de 23 años",
    "Ordenanza nº 533-A/99, apartado b2 (plan diferente)",
    "Ordenanza nº 533-A/99, apartado b3 (otra institución)",
    "Ordenanza nº 612/93",
    "Ordenanza nº 854-B/99"
  ) ~ "Acceso mayores/especiales",
  
  # CAMBIO/TRASLADO
  datos_sin_multi$Application.mode %in% c(
    "Cambio de institución/titulación",
    "Cambio de titulación",
    "Traslado",
    "Cambio de institución/titulación (internacional)"
  ) ~ "Acceso por cambio/traslado",
  
  # FORMACIÓN PREVIA
  datos_sin_multi$Application.mode %in% c(
    "Titulares de diploma de ciclo corto",
    "Titulares de diploma de especialización tecnológica",
    "Titulares de otros estudios superiores"
  ) ~ "Acceso por formación previa",
  
  # INTERNACIONAL
  datos_sin_multi$Application.mode %in% c(
    "Estudiante internacional (grado)"
  ) ~ "Acceso internacional"
  
)

unique(datos_sin_multi$Application.mode_group)

tabla_modo_app_target <- table(datos_sin_multi$Application.mode_group,
                               datos_sin_multi$Target_bin)
tabla_modo_app_target

prop.table(tabla_modo_app_target, 1)
prop.table(tabla_modo_app_target, 2)

cramersV(tabla_modo_app_target)
GK_assoc(datos_sin_multi$Application.mode_group, datos_sin_multi$Target_bin)
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Application.mode_group)

chisq.test(tabla_modo_app_target, correct = FALSE)
chisq.test(tabla_modo_app_target)$expected

# Gráficos:
tabla_modo_app_plot <- datos_sin_multi %>%
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


# Género:
unique(datos_sin_multi$Gender)

tabla_genero_target <- table(datos_sin_multi$Gender,
                             datos_sin_multi$Target_bin)
tabla_genero_target

prop.table(tabla_genero_target, 1)
prop.table(tabla_genero_target, 2)

cramersV(tabla_genero_target)
GK_assoc(datos_sin_multi$Gender, datos_sin_multi$Target_bin)
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Gender)

chisq.test(tabla_genero_target, correct = FALSE)
chisq.test(tabla_genero_target)$expected

# Gráfico:
mosaic(~ Gender + Target_bin, data = datos_sin_multi,  
       shade = TRUE, legend = TRUE)


# Course
# Vamos a realizar una reagrupación de las carreras para que el estudio sea fiable:

datos_sin_multi$Course_group <- dplyr::case_when(
  
  # SALUD
  datos_sin_multi$Course %in% c(
    "Enfermería",
    "Enfermería Veterinaria",
    "Higiene Bucodental"
  ) ~ "Salud",
  
  # INGENIERÍA / TECNOLOGÍA
  datos_sin_multi$Course %in% c(
    "Ingeniería Informática",
    "Tecnologías de Producción de Biocombustibles",
    "Diseño de Animación y Multimedia"
  ) ~ "Ingeniería/Tech",
  
  # SOCIALES / EMPRESA
  datos_sin_multi$Course %in% c(
    "Gestión",
    "Gestión (turno de tarde)",
    "Gestión de Publicidad y Marketing",
    "Turismo"
  ) ~ "Empresa",
  
  # EDUCACIÓN / SOCIAL
  datos_sin_multi$Course %in% c(
    "Educación Básica",
    "Trabajo Social",
    "Trabajo Social (turno de tarde)"
  ) ~ "Educación/Social",
  
  # COMUNICACIÓN / DISEÑO
  datos_sin_multi$Course %in% c(
    "Diseño de Comunicación",
    "Periodismo y Comunicación"
  ) ~ "Comunicación",
  
  # AGRO / ANIMAL
  datos_sin_multi$Course %in% c(
    "Agronomía",
    "Equinocultura"
  ) ~ "Agro/Animal",
  
  TRUE ~ NA_character_
)

sum(table(datos_sin_multi$Course_group))
unique(datos_sin_multi$Course)

tabla_course_target <- table(datos_sin_multi$Course_group,
                             datos_sin_multi$Target_bin)
tabla_course_target

prop.table(tabla_course_target, 1)
prop.table(tabla_course_target, 2)

cramersV(tabla_course_target)
GK_assoc(datos_sin_multi$Course_group, datos_sin_multi$Target_bin)
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Course_group)

chisq.test(tabla_course_target, correct = FALSE)
chisq.test(tabla_course_target)$expected

# Simplemente cambiamos los nombres a más cortos para que en el gráfico se entienda
datos_sin_multi$Course_group_short <- dplyr::recode(
  datos_sin_multi$Course_group,
  "Ingeniería/Tech" = "Ing./Tech",
  "Educación/Social" = "Educ./Soc.",
  "Comunicación" = "Com.",
  "Agro/Animal" = "Agro/Anim.",
  "Empresa" = "Empresa",
  "Salud" = "Salud"
)

mosaic(~ Course_group_short + Target_bin, 
       data = datos_sin_multi,
       shade = TRUE,
       legend = TRUE,
       cex.axis = 0.8)  

tabla_course_plot <- datos_sin_multi %>%
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

#analizamos sin Diseño Multimedia:

#empezamos creando un dataset sin multimedia:
datos_sin_multi <- datos_recodificados[
  datos_recodificados$Course_limpio != "Diseño de Animación y Multimedia",
]
sum(table(datos_sin_multi$Course_limpio))

datos_sin_multi$Course_group <- dplyr::case_when(
  
  # SALUD
  datos_sin_multi$Course_limpio %in% c(
    "Enfermería",
    "Enfermería Veterinaria",
    "Higiene Bucodental"
  ) ~ "Salud",
  
  # INGENIERÍA / TECNOLOGÍA
  datos_sin_multi$Course_limpio %in% c(
    "Ingeniería Informática",
    "Tecnologías de Producción de Biocombustibles"
  ) ~ "Ingeniería/Tech",
  
  # SOCIALES / EMPRESA
  datos_sin_multi$Course_limpio %in% c(
    "Gestión",
    "Gestión de Publicidad y Marketing",
    "Turismo"
  ) ~ "Empresa",
  
  # EDUCACIÓN / SOCIAL
  datos_sin_multi$Course_limpio %in% c(
    "Educación Básica",
    "Trabajo Social"
  ) ~ "Educación/Social",
  
  # COMUNICACIÓN / DISEÑO
  datos_sin_multi$Course_limpio %in% c(
    "Diseño de Comunicación",
    "Periodismo y Comunicación"
  ) ~ "Comunicación",
  
  # AGRO / ANIMAL
  datos_sin_multi$Course_limpio %in% c(
    "Agronomía",
    "Equinocultura"
  ) ~ "Agro/Animal",
  
  TRUE ~ NA_character_
)



#seguimos
sum(table(datos_sin_multi$Course_group))
table(datos_sin_multi$Course_group)
colnames(datos_sin_multi)
tabla_course_sin_multi_target <- table(datos_sin_multi$Course_group,
                                       datos_sin_multi$Target_bin)
tabla_course_sin_multi_target


prop.table(tabla_course_sin_multi_target, 1)
prop.table(tabla_course_sin_multi_target, 2)

cramersV(tabla_course_sin_multi_target)
GK_assoc(datos_sin_multi$Course_group, datos_sin_multi$Target_bin)
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Course_group)

chisq.test(tabla_course_sin_multi_target, correct = FALSE)
chisq.test(tabla_course_sin_multi_target)$expected

#Simplemente cambiamos los nombres a más cortos para que en el gráfico se entienda

datos_sin_multi$Course_group_short <- dplyr::recode(
  datos_sin_multi$Course_group,
  "Ingeniería/Tech" = "Ing./Tech",
  "Educación/Social" = "Educ./Soc.",
  "Comunicación" = "Com.",
  "Agro/Animal" = "Agro/Anim.",
  "Empresa" = "Empresa",
  "Salud" = "Salud"
)

# Gráficos:

mosaic(~ Course_group_short + Target_bin, 
       data = datos_sin_multi,
       shade = TRUE,
       legend = TRUE,
       cex.axis = 0.8)  

tabla_course_plot <- datos_sin_multi %>%
  count(Course_group_short, Target_bin) %>%
  group_by(Course_group_short) %>%
  mutate(prop = n / sum(n),
         etiqueta = scales::percent(prop, accuracy = 0.1))

ggplot(tabla_course_plot, aes(x = Course_group_short, y = prop, fill = Target_bin)) +
  geom_col() +
  geom_text(aes(label = etiqueta),
            position = position_stack(vjust = 0.5),
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "Dropout" = "red",
    "No Dropout" = "lightgreen"
  )) +
  labs(
    x = "Tipo de carrera",
    y = "Proporción",
    fill = "Abandono",
    title = "Relación entre tipo de carrera y abandono"
  ) +
  theme_minimal()


# Previous education level
table(datos_sin_multi$Previous_education_level)
unique(datos_sin_multi$Previous_education_level)

tabla_prev_edu_target <- table(datos_sin_multi$Previous_education_level,
                               datos_sin_multi$Target_bin)
tabla_prev_edu_target

prop.table(tabla_prev_edu_target, 1)
prop.table(tabla_prev_edu_target, 2)

cramersV(tabla_prev_edu_target)
GK_assoc(datos_sin_multi$Previous_education_level, datos_sin_multi$Target_bin)
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Previous_education_level)

chisq.test(tabla_prev_edu_target, correct = FALSE)
chisq.test(tabla_prev_edu_target)$expected

# Cambiamos a nombres más cortos:
datos_sin_multi$Previous_education_level_group_short <- dplyr::recode(
  datos_sin_multi$Previous_education_level,
  "Bajo" = "Bajo",
  "Medio" = "Medio",
  "Superior" = "Sup.",
  "Técnico" = "Técn."
)

# Gráfico:
mosaic(~ Previous_education_level_group_short + Target_bin, 
       data = datos_sin_multi,  
       shade = TRUE, legend = TRUE)


# Mother education level:
table(datos_sin_multi$Mother_education_level)
unique(datos_sin_multi$Mother_education_level)

tabla_mum_educ_target <- table(datos_sin_multi$Mother_education_level,
                               datos_sin_multi$Target_bin)
tabla_mum_educ_target

prop.table(tabla_mum_educ_target, 1)
prop.table(tabla_mum_educ_target, 2)

cramersV(tabla_mum_educ_target)
GK_assoc(datos_sin_multi$Mother_education_level, datos_sin_multi$Target_bin)
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Mother_education_level)

chisq.test(tabla_mum_educ_target, correct = FALSE)
chisq.test(tabla_mum_educ_target)$expected

# Cambiamos a nombres más cortos:
datos_sin_multi$Mother_education_level_group_short <- dplyr::recode(
  datos_sin_multi$Mother_education_level,
  "Bajo" = "Bajo",
  "Medio" = "Medio",
  "Superior" = "Sup.",
  "Técnico" = "Técn."
)

# Gráfico:
mosaic(~ Mother_education_level_group_short + Target_bin, data = datos_sin_multi,  
       shade = TRUE, legend = TRUE)

# Father education level:
table(datos_sin_multi$Father_education_level)
unique(datos_sin_multi$Father_education_level)

tabla_dad_educ_target <- table(datos_sin_multi$Father_education_level,
                               datos_sin_multi$Target_bin)
tabla_dad_educ_target

prop.table(tabla_dad_educ_target, 1)
prop.table(tabla_dad_educ_target, 2)

cramersV(tabla_dad_educ_target)
GK_assoc(datos_sin_multi$Father_education_level, datos_sin_multi$Target_bin)
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Father_education_level)

chisq.test(tabla_dad_educ_target, correct = FALSE)
chisq.test(tabla_dad_educ_target)$expected

# Cambiamos a nombres más cortos:
datos_sin_multi$Father_education_level_group_short <- dplyr::recode(
  datos_sin_multi$Father_education_level,
  "Bajo" = "Bajo",
  "Medio" = "Medio",
  "Superior" = "Sup.",
  "Técnico" = "Técn."
)

# Gráfico:
mosaic(~ Father_education_level_group_short + Target_bin, 
       data = datos_sin_multi,  
       shade = TRUE, legend = TRUE)


# Mother occupation level:

table(datos_sin_multi$Mother_occupation_level)
unique(datos_sin_multi$Mother_occupation_level)

tabla_mum_ocup_target <- table(datos_sin_multi$Mother_occupation_level,
                               datos_sin_multi$Target_bin)
tabla_mum_ocup_target

prop.table(tabla_mum_ocup_target, 1)
prop.table(tabla_mum_ocup_target, 2)

cramersV(tabla_mum_ocup_target)
GK_assoc(datos_sin_multi$Mother_occupation_level, datos_sin_multi$Target_bin)
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Mother_occupation_level)

chisq.test(tabla_mum_ocup_target, correct = FALSE)
chisq.test(tabla_mum_ocup_target)$expected

# Gráfico:
mosaic(~ Mother_occupation_level + Target_bin, data = datos_sin_multi,  
       shade = TRUE, legend = TRUE)

# Father occupation level:

table(datos_sin_multi$Father_occupation_level)
unique(datos_sin_multi$Father_occupation_level)

tabla_dad_ocup_target <- table(datos_sin_multi$Father_occupation_level,
                               datos_sin_multi$Target_bin)
tabla_dad_ocup_target

prop.table(tabla_dad_ocup_target, 1)
prop.table(tabla_dad_ocup_target, 2)

cramersV(tabla_dad_ocup_target)
GK_assoc(datos_sin_multi$Father_occupation_level, datos_sin_multi$Target_bin)
GK_assoc(datos_sin_multi$Target_bin, datos_sin_multi$Father_occupation_level)

chisq.test(tabla_dad_ocup_target, correct = FALSE)
chisq.test(tabla_dad_ocup_target)$expected

# Gráficos:
mosaic(~ Father_occupation_level + Target_bin, data = datos_sin_multi,  
       shade = TRUE, legend = TRUE)


tabla_course_plot <- datos_recodificados %>%
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

# Recodificamos primero la variable:
datos_sin_imputar$Mother.s.qualification_recodif<-recode(datos_sin_imputar$Mother.s.qualification,
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

sum(table(datos_sin_imputar$Mother.s.qualification))

sum(table(datos_sin_imputar$Mother.s.qualification_recodif))
datos_sin_imputar$Mother_education_level <- case_when(
  
  # BAJO
  datos_sin_imputar$Mother.s.qualification_recodif %in% c(
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
  datos_sin_imputar$Mother.s.qualification_recodif %in% c(
    "Educación secundaria - 12º año o equivalente",
    "2º ciclo del bachillerato general"
  ) ~ "Medio",
  
  # TÉCNICO
  datos_sin_imputar$Mother.s.qualification_recodif %in% c(
    "Curso de especialización tecnológica",
    "Curso de estudios superiores especializados",
    "Curso técnico-profesional",
    "Curso técnico superior profesional",
    "Curso general de comercio"
  ) ~ "Técnico",
  
  # SUPERIOR
  datos_sin_imputar$Mother.s.qualification_recodif %in% c(
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


datos_sin_imputar$Target_bin <- ifelse(datos_sin_imputar$Target == "Dropout", "Abandono", "No Abandono")
datos_sin_imputar$Target_bin <- as.factor(datos_sin_imputar$Target_bin)





sum(table(datos_sin_imputar$Mother_education_level)) #coincide con 4424 -130 faltantes
tabla_mum_educ_target_sin_imputar <- table(datos_sin_imputar$Mother_education_level,
                                           datos_sin_imputar$Target_bin)
sum(tabla_mum_educ_target_sin_imputar)

prop.table(tabla_mum_educ_target_sin_imputar, 1)
prop.table(tabla_mum_educ_target_sin_imputar, 2)

cramersV(tabla_mum_educ_target_sin_imputar)
GK_assoc(datos_sin_imputar$Mother_education_level, datos_sin_imputar$Target_bin)
GK_assoc(datos_sin_imputar$Target_bin, datos_sin_imputar$Mother_education_level)

chisq.test(tabla_mum_educ_target_sin_imputar, correct = FALSE)
chisq.test(tabla_mum_educ_target_sin_imputar)$expected

#Cambiamos a nombres más cortos:
datos_sin_imputar$Mother_education_level_group_short <- dplyr::recode(
  datos_sin_imputar$Mother_education_level,
  "Bajo"= "Bajo",
  "Medio"= "Medio",
  "Superior"= "Sup.",
  "Técnico"= "Técn."
)
#Gráfico:
mosaic(~ Mother_education_level_group_short + Target_bin, data = datos_sin_imputar,  
       shade = TRUE, legend = TRUE)



#Analizamos Father_education_level sin los datos imputados para ver como cambia respecto a la imputación

# Recodificamos primero la variable:
datos_sin_imputar$Father.s.qualification_recodif <- recode(datos_sin_imputar$Father.s.qualification,
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

sum(table(datos_sin_imputar$Father.s.qualification))

sum(table(datos_sin_imputar$Father.s.qualification_recodif))

datos_sin_imputar$Father_education_level <- case_when(
  # BAJO
  datos_sin_imputar$Father.s.qualification_recodif %in% c(
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
  datos_sin_imputar$Father.s.qualification_recodif %in% c(
    "Educación secundaria - 12º año o equivalente",
    "2º ciclo del bachillerato general"
  ) ~ "Medio",
  
  # TÉCNICO
  datos_sin_imputar$Father.s.qualification_recodif %in% c(
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
  datos_sin_imputar$Father.s.qualification_recodif%in% c(
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



sum(table(datos_sin_imputar$Father_education_level)) #coincide con 4424 -112 faltantes
tabla_dad_educ_target_sin_imputar <- table(datos_sin_imputar$Father_education_level,
                                           datos_sin_imputar$Target_bin)
sum(tabla_dad_educ_target_sin_imputar)

prop.table(tabla_dad_educ_target_sin_imputar, 1)
prop.table(tabla_dad_educ_target_sin_imputar, 2)

cramersV(tabla_dad_educ_target_sin_imputar)
GK_assoc(datos_sin_imputar$Father_education_level, datos_sin_imputar$Target_bin)
GK_assoc(datos_sin_imputar$Target_bin, datos_sin_imputar$Father_education_level)

chisq.test(tabla_dad_educ_target_sin_imputar, correct = FALSE)
chisq.test(tabla_dad_educ_target_sin_imputar)$expected

#Cambiamos a nombres más cortos:
datos_sin_imputar$Father_education_level_group_short <- dplyr::recode(
  datos_sin_imputar$Father_education_level,
  "Bajo"= "Bajo",
  "Medio"= "Medio",
  "Superior"= "Sup.",
  "Técnico"= "Técn."
)
#Gráfico:
mosaic(~ Father_education_level_group_short + Target_bin, data = datos_sin_imputar,  
       shade = TRUE, legend = TRUE)



#Analizamos Mother occupation level sin los datos imputados para ver como cambia respecto a la imputación

# Recodificamos primero la variable:
datos_sin_imputar$Mother.s.occupation_recodif<-recode(datos_sin_imputar$Mother.s.occupation,
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

sum(table(datos_sin_imputar$Mother.s.occupation))

sum(table(datos_sin_imputar$Mother.s.occupation_recodif))

datos_sin_imputar$Mother_occupation_level <- case_when(
  
  # ALTA CUALIFICACIÓN
  datos_sin_imputar$Mother.s.occupation_recodif %in% c(
    "Representantes del poder legislativo y ejecutivo, directores y gerentes",
    "Especialistas en actividades intelectuales y científicas",
    "Especialistas en tecnologías de la información y la comunicación (TIC)",
    "Profesionales de la salud",
    "Profesores"
  ) ~ "Alta cualificación",
  
  # CUALIFICACIÓN MEDIA
  datos_sin_imputar$Mother.s.occupation_recodif %in% c(
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
  datos_sin_imputar$Mother.s.occupation_recodif %in% c(
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
  datos_sin_imputar$Mother.s.occupation_recodif %in% c(
    "Trabajadores no cualificados",
    "Trabajadores no cualificados en agricultura, pesca y silvicultura",
    "Trabajadores no cualificados en industria extractiva, construcción, manufactura y transporte",
    "Trabajadores de limpieza"
  ) ~ "No cualificados",
  
  # OTROS
  datos_sin_imputar$Mother.s.occupation_recodif %in% c(
    "Estudiante",
    "Otra situación",
    "Profesiones de las fuerzas armadas"
  ) ~ "Otros",
  TRUE ~ NA_character_
  
)





sum(table(datos_sin_imputar$Mother_occupation_level)) #coincide con 4424 -17 faltantes
tabla_mum_ocup_target_sin_imputar <- table(datos_sin_imputar$Mother_occupation_level,
                                           datos_sin_imputar$Target_bin)
sum(tabla_mum_ocup_target_sin_imputar)

prop.table(tabla_mum_ocup_target_sin_imputar, 1)
prop.table(tabla_mum_ocup_target_sin_imputar, 2)

cramersV(tabla_mum_ocup_target_sin_imputar)
GK_assoc(datos_sin_imputar$Mother_occupation_level, datos_sin_imputar$Target_bin)
GK_assoc(datos_sin_imputar$Target_bin, datos_sin_imputar$Mother_occupation_level)

chisq.test(tabla_mum_ocup_target_sin_imputar, correct = FALSE)
chisq.test(tabla_mum_ocup_target_sin_imputar)$expected


#Gráfico:
mosaic(~ Mother_occupation_level + Target_bin, data = datos_sin_imputar,  
       shade = TRUE, legend = TRUE)


#Analizamos Father occupation level sin los datos imputados para ver como cambia respecto a la imputación

# Recodificamos primero la variable:
datos_sin_imputar$Father.s.occupation_recodif<-recode(datos_sin_imputar$Father.s.occupation,
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

sum(table(datos_sin_imputar$Father.s.occupation))

sum(table(datos_sin_imputar$Father.s.occupation_recodif))

datos_sin_imputar$Father_occupation_level <- case_when(
  
  # ALTA CUALIFICACIÓN
  datos_sin_imputar$Father.s.occupation_recodif %in% c(
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
  datos_sin_imputar$Father.s.occupation_recodif %in% c(
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
  datos_sin_imputar$Father.s.occupation_recodif %in% c(
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
  datos_sin_imputar$Father.s.occupation_recodif %in% c(
    "Trabajadores no cualificados",
    "Trabajadores no cualificados en agricultura, pesca y silvicultura",
    "Trabajadores no cualificados en industria, construcción y transporte"
  ) ~ "No cualificados",
  
  # OTROS
  datos_sin_imputar$Father.s.occupation_recodif %in% c(
    "Estudiante",
    "Otra situación"
    
  ) ~ "Otros",
  
  # MILITAR
  datos_sin_imputar$Father.s.occupation_recodif %in% c(
    "Profesiones de las fuerzas armadas",
    "Oficiales de las fuerzas armadas",
    "Sargentos de las fuerzas armadas",
    "Otro personal de las fuerzas armadas"
  ) ~ "Formación militar",
  
  TRUE ~ NA_character_
)





sum(table(datos_sin_imputar$Father_occupation_level)) #coincide con 4424 -19 faltantes
tabla_dad_ocup_target_sin_imputar <- table(datos_sin_imputar$Father_occupation_level,
                                           datos_sin_imputar$Target_bin)
sum(tabla_dad_ocup_target_sin_imputar)

prop.table(tabla_dad_ocup_target_sin_imputar, 1)
prop.table(tabla_dad_ocup_target_sin_imputar, 2)

cramersV(tabla_dad_ocup_target_sin_imputar)
GK_assoc(datos_sin_imputar$Father_occupation_level, datos_sin_imputar$Target_bin)
GK_assoc(datos_sin_imputar$Target_bin, datos_sin_imputar$Father_occupation_level)

chisq.test(tabla_dad_ocup_target_sin_imputar, correct = FALSE)
chisq.test(tabla_dad_ocup_target_sin_imputar)$expected


#Gráfico:
mosaic(~ Father_occupation_level + Target_bin, data = datos_sin_imputar,  
       shade = TRUE, legend = TRUE)