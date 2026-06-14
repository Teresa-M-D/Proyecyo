#En este script se encuentra todo el código no usado para gráficos de la memoria, pero si usado
#para gráficos de la presentación o anexos, o comprobaciones hechas.



#IMPUTACIÓN DE DATOS FALTANTES POR MODA SIMPLE:


datos_sin_imputar <- read.csv("estudiantes.csv", header = TRUE, sep = ";")
datos_imputados <- read.csv("estudiantes.csv", header = TRUE, sep = ";")

# Vista inicial de los datos
descriptive(datos_sin_imputar)
dim(datos_sin_imputar)
names(datos_sin_imputar)

#Converitr a NA:
datos_imputados$Mother.s.qualification[
  datos_imputados$Mother.s.qualification == 34
] <- NA

datos_imputados$Father.s.qualification[
  datos_imputados$Father.s.qualification == 34
] <- NA

datos_imputados$Mother.s.occupation[
  datos_imputados$Mother.s.occupation == 99
] <- NA

datos_imputados$Father.s.occupation[
  datos_imputados$Father.s.occupation == 99
] <- NA

#Comprobación de valores faltantes antes de imputación simple:
sum(is.na(datos_imputados$Mother.s.occupation))
sum(is.na(datos_imputados$Father.s.occupation))
sum(is.na(datos_imputados$Mother.s.qualification))
sum(is.na(datos_imputados$Father.s.qualification))

sum(is.na(datos_imputados))

descriptive(datos_imputados)

#Función para calcular la moda:
moda <- function(x) {
  ux <- na.omit(x)
  
  if (length(ux) == 0) {
    return(NA)
  }
  
  ux[which.max(tabulate(match(ux, ux)))]
}

#Imputación por moda simple:
# Calculamos la moda de cada variable
moda_mother_occupation <- moda(datos_imputados$Mother.s.occupation)
moda_father_occupation <- moda(datos_imputados$Father.s.occupation)
moda_mother_qualification <- moda(datos_imputados$Mother.s.qualification)
moda_father_qualification <- moda(datos_imputados$Father.s.qualification)

# Imputamos los NA usando la moda de cada variable
datos_imputados$Mother.s.occupation[
  is.na(datos_imputados$Mother.s.occupation)
] <- moda_mother_occupation

datos_imputados$Father.s.occupation[
  is.na(datos_imputados$Father.s.occupation)
] <- moda_father_occupation

datos_imputados$Mother.s.qualification[
  is.na(datos_imputados$Mother.s.qualification)
] <- moda_mother_qualification

datos_imputados$Father.s.qualification[
  is.na(datos_imputados$Father.s.qualification)
] <- moda_father_qualification

#Comprobación de valores faltantes después de imputar:
sum(is.na(datos_imputados$Mother.s.occupation))
sum(is.na(datos_imputados$Father.s.occupation))
sum(is.na(datos_imputados$Mother.s.qualification))
sum(is.na(datos_imputados$Father.s.qualification))

sum(is.na(datos_imputados))

descriptive(datos_imputados)

#Resumen de las modas utilizadas:
modas_utilizadas <- data.frame(
  Variable = c(
    "Mother.s.occupation",
    "Father.s.occupation",
    "Mother.s.qualification",
    "Father.s.qualification"
  ),
  Moda_utilizada = c(
    moda_mother_occupation,
    moda_father_occupation,
    moda_mother_qualification,
    moda_father_qualification
  )
)

modas_utilizadas


#Comparación antes y después de la imputación
datos_sin_imputar_comparacion <- datos_sin_imputar

datos_sin_imputar_comparacion$Mother.s.qualification[
  datos_sin_imputar_comparacion$Mother.s.qualification == 34
] <- NA

datos_sin_imputar_comparacion$Father.s.qualification[
  datos_sin_imputar_comparacion$Father.s.qualification == 34
] <- NA

datos_sin_imputar_comparacion$Mother.s.occupation[
  datos_sin_imputar_comparacion$Mother.s.occupation == 99
] <- NA

datos_sin_imputar_comparacion$Father.s.occupation[
  datos_sin_imputar_comparacion$Father.s.occupation == 99
] <- NA

#Ejemplo de comparación con mother qualification:
freq_mother_qualification_sin <- table(
  datos_sin_imputar_comparacion$Mother.s.qualification,
  useNA = "ifany"
)

freq_mother_qualification_imp <- table(
  datos_imputados$Mother.s.qualification,
  useNA = "ifany"
)

freq_mother_qualification_sin
freq_mother_qualification_imp

prop.table(freq_mother_qualification_sin)
prop.table(freq_mother_qualification_imp)

barplot(
  freq_mother_qualification_sin,
  main = "Mother's qualification antes de imputar",
  xlab = "Código de cualificación",
  ylab = "Frecuencia"
)

barplot(
  freq_mother_qualification_imp,
  main = "Mother's qualification después de imputar por moda",
  xlab = "Código de cualificación",
  ylab = "Frecuencia"
)


#Diferencia de proporciones antes y después de imputar:
niveles_comunes <- sort(unique(c(
  datos_sin_imputar_comparacion$Father.s.qualification,
  datos_imputados$Father.s.qualification
)))

tabla_sin <- prop.table(
  table(
    factor(
      datos_sin_imputar_comparacion$Father.s.qualification,
      levels = niveles_comunes
    )
  )
)

tabla_imp <- prop.table(
  table(
    factor(
      datos_imputados$Father.s.qualification,
      levels = niveles_comunes
    )
  )
)

diferencia_proporciones <- abs(tabla_sin - tabla_imp)

diferencia_proporciones

#Las diferencias de proporciones tanto antes como después de la imputación son reducidas,
#así que, la imputación por moda no modifica de forma relevante la distribución de la variable.




#Análisis univariante categórico: gráficos de titulación y titulación reagrupada que se encuentra en la presentación (los demás de la presentación se pueden encontrar en el script de Trabajo):


colores_course_group <- c(
  "Salud" = "#5DADE2",                  # azul claro
  "Empresa" = "#D4A017",                # dorado
  "Educación/Social" = "#F28E2B",       # naranja
  "Agro/Animal" = "#6AA84F",            # verde
  "Ingeniería/Tech" = "#34495E",  # azul grisáceo oscuro
  "Comunicación" = "#C77DFF"            # lila
)

# Tabla resumen de titulaciones limpias
resumen_course <- datos_modelo %>%
  count(Course_limpio, sort = TRUE) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = percent(porcentaje, accuracy = 0.1),
    Course_limpio = reorder(Course_limpio, n)
  )

# Ver tabla resumen
resumen_course

# Gráfico univariante de Course_limpio
resumen_course_limpio <- datos_modelo %>%
  count(Course_limpio, Course_group, sort = TRUE) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = percent(porcentaje, accuracy = 0.1),
    Course_limpio = reorder(Course_limpio, n)
  )

ggplot(
  resumen_course_limpio,
  aes(x = Course_limpio, y = porcentaje, fill = Course_group)
) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(
    aes(label = etiqueta),
    hjust = -0.1,
    size = 3.3
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, max(resumen_course_limpio$porcentaje) + 0.05)
  ) +
  scale_fill_manual(values = colores_course_group) +
  labs(
    title = "Distribución del alumnado según titulación",
    subtitle = "Color según área de estudio",
    x = "Titulación",
    y = "Porcentaje de estudiantes",
    fill = "Área de estudio"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.y = element_text(size = 8.5)
  )
#para Course_group
resumen_course_group <- datos_modelo %>%
  count(Course_group, sort = TRUE) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = percent(porcentaje, accuracy = 0.1),
    Course_group = reorder(Course_group, porcentaje)
  )

ggplot(
  resumen_course_group,
  aes(x = Course_group, y = porcentaje, fill = Course_group)
) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(
    aes(label = etiqueta),
    hjust = -0.1,
    size = 4
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, max(resumen_course_group$porcentaje) + 0.08)
  ) +
  scale_fill_manual(values = colores_course_group) +
  labs(
    title = "Distribución del alumnado por área de estudio",
    subtitle = "Agrupación de titulaciones por ramas de conocimiento",
    x = "Área de estudio",
    y = "Porcentaje de estudiantes"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )





#ANÁLISIS BIVARIANTE: VARIABLE CATEGÓRICA VS TARGET de las variables que no estaban en la memoria:

#MARITAL STATUS:

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

# Nombres más cortos para el gráfico
datos_modelo$Marital_group_short <- dplyr::recode(
  datos_modelo$Marital_group,
  "Soltero" = "Soltero",
  "En pareja" = "Pareja",
  "Otros" = "Otros"
)

#Gráfico:
datos_modelo$Marital_group_short <- factor(datos_modelo$Marital_group_short, levels=c("Otros", "Pareja", "Soltero"))

datos_modelo$Target_bin <- factor(
  datos_modelo$Target_bin,
  levels = c("Abandono", "No Abandono")
)


mosaic(
  ~ Marital_group_short + Target_bin,
  data = datos_modelo,
  shade = TRUE,
  legend = TRUE,
  cex.axis = 0.8,
  labeling_args = list(
    set_varnames = c(
      Marital_group_short = "Estado civil",
      Target_bin = "Abandono"
    )
  )
)


#DAYTIME EVENING ATTENDANCE:
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




#DISPLACED:

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


#INTERNATIONAL:
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
  mutate(
    Target_bin = factor(
      Target_bin,
      levels = c("No Abandono", "Abandono")
    )
  ) %>%
  count(International, Target_bin) %>%
  group_by(International) %>%
  mutate(
    prop = n / sum(n),
    etiqueta = scales::percent(prop, accuracy = 0.1)
  )

ggplot(tabla_internacional_plot, aes(x = International, y = prop, fill = Target_bin)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = etiqueta),
    position = position_fill(vjust = 0.5),
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "No Abandono" = "lightblue",
      "Abandono" = "indianred"
    ),
    breaks = c("No Abandono", "Abandono")
  ) +
  labs(
    x = "Estudiante internacional",
    y = "Proporción",
    fill = "Abandono",
    title = "Relación entre estudiantes internacionales y abandono"
  ) +
  theme_minimal()


#APPLICATION MODE:

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
  mutate(
    Target_bin = factor(
      Target_bin,
      levels = c("No Abandono", "Abandono")
    )
  ) %>%
  count(Application.mode_group, Target_bin) %>%
  group_by(Application.mode_group) %>%
  mutate(
    prop = n / sum(n),
    etiqueta = scales::percent(prop, accuracy = 0.1)
  )

ggplot(tabla_modo_app_plot, aes(x = Application.mode_group, y = prop, fill = Target_bin)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = etiqueta),
    position = position_fill(vjust = 0.5),
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "No Abandono" = "lightblue",
      "Abandono" = "indianred"
    ),
    breaks = c("No Abandono", "Abandono")
  ) +
  labs(
    x = "Tipo de acceso al grado",
    y = "Proporción",
    fill = "Abandono",
    title = "Relación entre tipo de acceso al grado y abandono"
  ) +
  theme_minimal()


#DEBTOR:

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

#Gráfico:
tabla_debtor_plot <- datos_modelo %>%
  mutate(
    Target_bin = factor(
      Target_bin,
      levels = c("No Abandono", "Abandono")
    )
  ) %>%
  count(Debtor, Target_bin) %>%
  group_by(Debtor) %>%
  mutate(
    prop = n / sum(n),
    etiqueta = scales::percent(prop, accuracy = 0.1)
  )

ggplot(tabla_debtor_plot, aes(x = Debtor, y = prop, fill = Target_bin)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = etiqueta),
    position = position_fill(vjust = 0.5),
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "No Abandono" = "lightblue",
      "Abandono" = "indianred"
    ),
    breaks = c("No Abandono", "Abandono")
  ) +
  labs(
    x = "Debe dinero",
    y = "Proporción",
    fill = "Abandono",
    title = "Relación entre deudor y abandono"
  ) +
  theme_minimal()

#SCHOLARSHIP_HOLDER:

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
datos_modelo$Scholarship.holder <- factor(datos_modelo$Scholarship.holder, levels=c("No", "Sí"))

datos_modelo$Target_bin <- factor(
  datos_modelo$Target_bin,
  levels = c("Abandono", "No Abandono")
)

#Gráficos:
mosaic(
  ~ Scholarship.holder + Target_bin,
  data = datos_modelo,
  shade = TRUE,
  legend = TRUE,
  cex.axis = 0.8,
  labeling_args = list(
    set_varnames = c(
      Scholarship.holder = "Becado",
      Target_bin = "Abandono"
    )
  )
)





#EDUCATIONAL SPECIAL NEEDS:
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




#PREVIOUS EDUCATION LEVEL:
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
datos_modelo$Previous_education_level_group_short <- factor(datos_modelo$Previous_education_level_group_short, levels=c("Bajo", "Medio", "Sup.", "Técn."))

datos_modelo$Target_bin <- factor(
  datos_modelo$Target_bin,
  levels = c("Abandono", "No Abandono")
)

mosaic(
  ~ Previous_education_level_group_short + Target_bin,
  data = datos_modelo,
  shade = TRUE,
  legend = TRUE,
  cex.axis = 0.8,
  labeling_args = list(
    set_varnames = c(
      Previous_education_level_group_short = "Nivel de titulación previa",
      Target_bin = "Abandono"
    )
  )
)

mosaic(~ Previous_education_level_group_short + Target_bin, data = datos_modelo,  
       shade = TRUE, legend = TRUE)


#MOTHER EDUCATION LEVEL.
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


#FATHER EDUCATION LEVEL:
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


#MOTHER OCCUPATION LEVEL:

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


#FATHER OCCUPATION LEVEL:


datos_modelo <- datos_modelo %>%
  mutate(
    Father_occupation_level = trimws(as.character(Father_occupation_level)),
    Father_occupation_short = case_when(
      Father_occupation_level == "Alta cualificación" ~ "Alta cualif.",
      Father_occupation_level == "Baja cualificación" ~ "Baja cualif.",
      Father_occupation_level == "Cualificación media" ~ "Media cualif.",
      Father_occupation_level == "Formación militar" ~ "FM",
      Father_occupation_level == "No cualificados" ~ "Sin cualif.",
      Father_occupation_level == "Otros" ~ "Otros",
      TRUE ~ Father_occupation_level
    )
  )


tabla_dad_ocup_target <- table(
  datos_modelo$Father_occupation_short,
  datos_modelo$Target_bin
)

tabla_dad_ocup_target

prop.table(tabla_dad_ocup_target, 1)
prop.table(tabla_dad_ocup_target, 2)

cramersV(tabla_dad_ocup_target)

chisq.test(tabla_dad_ocup_target, correct = FALSE)
chisq.test(tabla_dad_ocup_target)$expected


mosaic(
  ~ Father_occupation_short + Target_bin,
  data = datos_modelo,
  shade = TRUE,
  legend = TRUE,
  labeling_args = list(
    set_varnames = c(
      Father_occupation_short = "Ocupación del padre",
      Target_bin = "Abandono"
    )
  )
)

#ANÁLISIS BIVARIANTE: NÚMERICA VS TARGET

#digramas de cajas


#ANÁLISIS DE SENSIBILIDAD, comparamos datos imputados por moda condicionada con datos sin imputar:

# Base sin imputar con el mismo filtro que datos_modelo
datos_sensibilidad <- datos_sin_imputar %>%
  filter(
    !(Curricular.units.1st.sem..grade. == 0 &
        Curricular.units.1st.sem..approved. == 0 &
        Curricular.units.1st.sem..evaluations. == 0 &
        Curricular.units.1st.sem..credited. == 0 &
        Curricular.units.1st.sem..enrolled. == 0)
  )

# Variable objetivo
datos_sensibilidad$Target_bin <- ifelse(
  datos_sensibilidad$Target == "Dropout",
  "Abandono",
  "No Abandono"
)

datos_sensibilidad$Target_bin <- factor(
  datos_sensibilidad$Target_bin,
  levels = c("No Abandono", "Abandono")
)

datos_modelo$Target_bin <- factor(
  datos_modelo$Target_bin,
  levels = c("No Abandono", "Abandono")
)



# Reagrupación directa de las variables afectadas por imputación


datos_sensibilidad <- datos_sensibilidad %>%
  mutate(
    
    Mother_education_level = case_when(
      Mother.s.qualification %in% c(9, 10, 11, 12, 14, 19, 26, 29, 30, 35, 36, 37, 38) ~ "Bajo",
      Mother.s.qualification %in% c(1, 27) ~ "Medio",
      Mother.s.qualification %in% c(18, 22, 39, 41, 42) ~ "Técnico",
      Mother.s.qualification %in% c(2, 3, 4, 5, 6, 40, 43, 44) ~ "Superior",
      Mother.s.qualification == 34 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    Father_education_level = case_when(
      Father.s.qualification %in% c(9, 10, 11, 12, 14, 19, 26, 29, 30, 35, 36, 37, 38) ~ "Bajo",
      Father.s.qualification %in% c(1, 27) ~ "Medio",
      Father.s.qualification %in% c(13, 18, 20, 22, 25, 31, 33, 39, 41, 42) ~ "Técnico",
      Father.s.qualification %in% c(2, 3, 4, 5, 6, 40, 43, 44) ~ "Superior",
      Father.s.qualification == 34 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    Mother_occupation_level = case_when(
      Mother.s.occupation %in% c(1, 2, 122, 123, 125) ~ "Alta cualificación",
      Mother.s.occupation %in% c(3, 4, 131, 132, 134, 141, 143, 144) ~ "Cualificación media",
      Mother.s.occupation %in% c(5, 6, 7, 8, 151, 152, 153, 171, 173, 175, 194) ~ "Baja cualificación",
      Mother.s.occupation %in% c(9, 191, 192, 193) ~ "No cualificados",
      Mother.s.occupation %in% c(0, 10, 90) ~ "Otros",
      Mother.s.occupation == 99 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    Father_occupation_level = case_when(
      Father.s.occupation %in% c(1, 2, 112, 114, 121, 122, 123, 124) ~ "Alta cualificación",
      Father.s.occupation %in% c(3, 4, 131, 132, 134, 135, 141, 143, 144) ~ "Cualificación media",
      Father.s.occupation %in% c(5, 6, 7, 8, 151, 152, 153, 154, 161, 163, 171, 172, 174, 175, 181, 182, 183, 194, 195) ~ "Baja cualificación",
      Father.s.occupation %in% c(9, 192, 193) ~ "No cualificados",
      Father.s.occupation %in% c(0, 90) ~ "Otros",
      Father.s.occupation %in% c(10, 101, 102, 103) ~ "Formación militar",
      Father.s.occupation == 99 ~ NA_character_,
      TRUE ~ NA_character_
    )
  )


# Función para calcular métricas


calcular_metricas_sensibilidad <- function(datos, variable, nombre_base) {
  
  datos_aux <- datos %>%
    filter(!is.na(.data[[variable]]), !is.na(Target_bin))
  
  tabla <- table(datos_aux[[variable]], datos_aux$Target_bin)
  chi <- chisq.test(tabla, correct = FALSE)
  
  data.frame(
    Variable = variable,
    Base = nombre_base,
    N_usado = sum(tabla),
    N_perdidos = nrow(datos) - nrow(datos_aux),
    V_Cramer = as.numeric(cramersV(tabla)),
    p_valor_chi = chi$p.value
  )
}



# Tabla resumen final


resumen_sensibilidad <- bind_rows(
  calcular_metricas_sensibilidad(datos_modelo, "Mother_education_level", "Imputado"),
  calcular_metricas_sensibilidad(datos_sensibilidad, "Mother_education_level", "Sin imputar"),
  
  calcular_metricas_sensibilidad(datos_modelo, "Father_education_level", "Imputado"),
  calcular_metricas_sensibilidad(datos_sensibilidad, "Father_education_level", "Sin imputar"),
  
  calcular_metricas_sensibilidad(datos_modelo, "Mother_occupation_level", "Imputado"),
  calcular_metricas_sensibilidad(datos_sensibilidad, "Mother_occupation_level", "Sin imputar"),
  
  calcular_metricas_sensibilidad(datos_modelo, "Father_occupation_level", "Imputado"),
  calcular_metricas_sensibilidad(datos_sensibilidad, "Father_occupation_level", "Sin imputar")
) %>%
  mutate(
    V_Cramer = round(V_Cramer, 4),
    p_valor_chi = format.pval(p_valor_chi, digits = 4),
    Interpretacion = case_when(
      Variable == "Mother_education_level" & Base == "Sin imputar" ~ "No robusta",
      Variable %in% c("Mother_occupation_level", "Father_occupation_level") ~ "Robusta",
      Variable == "Father_education_level" ~ "Robusta débil",
      TRUE ~ ""
    )
  )

resumen_sensibilidad



#############################
#ESTUDIO UNIVARIANTE NUMÉRICO
#############################


#****************
#AGE AT ENROLLMENT
#****************
datos_recodificados$Age.at.enrollment
descriptive(datos_recodificados$Age.at.enrollment)
hist(datos_recodificados$Age.at.enrollment)
boxplot(datos_recodificados$Age.at.enrollment, yaxt="n") 
axis(2, at=seq(15,70, by=5))
#ppe
x_exp <- datos_recodificados$Age.at.enrollment - 17 #le restamos 17 para q el mínimo sea 0
qqplot(
  qexp(ppoints(length(x_exp)), rate = 1/mean(x_exp)),
  sort(x_exp),
  main = "Papel probabilístico exponencial Age At Enrollment",
  xlab = "Cuantiles teóricos",
  ylab = "Cuantiles observados"
  
)

abline(0,1)
#ppgamma
x_gamma <- datos_recodificados$Age.at.enrollment - 17
x_gamma <- x_gamma[x_gamma > 0]

library(MASS)

ajuste <- fitdistr(x_gamma, "gamma")
ajuste

qqplot(
  qgamma(
    ppoints(length(x_gamma)),
    shape = ajuste$estimate["shape"],
    rate = ajuste$estimate["rate"]
  ),
  sort(x_gamma),
  main = "Papel probabilístico gamma Edad",
  xlab = "Cuantiles teóricos",
  ylab = "Cuantiles observados"
)

abline(0,1)

#ppn
qqnorm(
  datos_recodificados$Age.at.enrollment,
  main = "Papel probabilístico normal Edad ",
)
qqline(datos_recodificados$Age.at.enrollment)
grid()

#*********************************
# C.U. WITHOUT EVALUATIONS
#*********************************

#1st SEM
descriptive(datos_recodificados$Curricular.units.1st.sem..without.evaluations.)
boxplot(datos_recodificados$Curricular.units.1st.sem..without.evaluations.)
hist(datos_recodificados$Curricular.units.1st.sem..without.evaluations., breaks=seq(0, 12, by=1), xaxt="n")
axis(1, at=seq(0,12, by=1))
#ver cuantos valores hay distintos de 0
descriptive(datos_recodificados$Curricular.units.1st.sem..without.evaluations.[datos_recodificados$Curricular.units.1st.sem..without.evaluations.!=0])
datos_recodificados$Curricular.units.1st.sem..without.evaluations.[datos_recodificados$Curricular.units.1st.sem..without.evaluations.!=0]
hist(datos_recodificados$Curricular.units.1st.sem..without.evaluations.[datos_recodificados$Curricular.units.1st.sem..without.evaluations.!=0]
)
axis(1, at=seq(0,12, by=1))
boxplot(datos_recodificados$Curricular.units.1st.sem..without.evaluations.[datos_recodificados$Curricular.units.1st.sem..without.evaluations.!=0])

#2nd SEM
descriptive(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.)
hist(datos_recodificados$Curricular.units.2nd.sem..without.evaluations., breaks=seq(0, 12, by=1), xaxt="n")
axis(1, at=seq(0,12, by=1))
#ver cuantos valores hay distintos de 0
descriptive(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.[datos_recodificados$Curricular.units.2nd.sem..without.evaluations.!=0])
datos_recodificados$Curricular.units.2nd.sem..without.evaluations.[datos_recodificados$Curricular.units.2nd.sem..without.evaluations.!=0]
hist(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.[datos_recodificados$Curricular.units.2nd.sem..without.evaluations.!=0]
)
axis(1, at=seq(0,12, by=1))
boxplot(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.[datos_recodificados$Curricular.units.2nd.sem..without.evaluations.!=0])

#comparación sem 1 y sem 2 para ver cuál tiene mas faltas en total
sum(datos_recodificados$Curricular.units.1st.sem..without.evaluations.)
sum(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.) #salen más faltas en sem2

#Ecdf
plot(ecdf(datos_recodificados$Curricular.units.1st.sem..without.evaluations.),do.points=FALSE, col="blue", verticals=TRUE)
plot(ecdf(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.), do.points=FALSE, col="red", add=TRUE, verticals=TRUE)
grid()
axis(1, at=seq(0, 12, by=1))


#***************
#C.U. EVALUATIONS
#***************

#1SEM
#descriptive general
descriptive(datos_recodificados$Curricular.units.2nd.sem..evaluations.)
#descriptivo por pib/año
tapply(datos_recodificados$Curricular.units.1st.sem..evaluations., datos_recodificados$PIB, descriptive) #sale lo http://127.0.0.1:44275/graphics/plot_zoom_png?width=1163&height=861del final pq descriptive es una funcion q cada vez q se eejcuta imprime aparte de return, entonces mientras se ejecuta me lo va enseñando, y al final me pone el vector del valor_PIB: resultado, para los tres casos: variable numerica(mi caso, evaluationes es numerica), categorica(no solo le hje dado una variable numerica asi q no sale nada) y fecha(lo mismo, null).
#boxplot por pib
boxplot(datos_recodificados$Curricular.units.1st.sem..evaluations.~datos_recodificados$PIB yaxt="n")
axis(2, at=seq(0, 45, by=5))
#curva densidad
plot(density(datos_recodificados$Curricular.units.1st.sem..evaluations.))
hist(datos_recodificados$Curricular.units.1st.sem..evaluations., breaks=seq(0, 46, by=1), xaxt="n")
axis(1, at=seq(2, 46, by=1))
describe(datos_recodificados$Curricular.units.1st.sem..evaluations.)

#cálculo moda (no existe una función concreta)
names(sort(table(datos_recodificados$Curricular.units.1st.sem..evaluations.), decreasing = TRUE))[1]
x<-prop.table(table(datos_recodificados$Curricular.units.1st.sem..evaluations.))
barplot(x)      

#boxplot por pib
boxplot(datos_recodificados$Curricular.units.1st.sem..evaluations.~datos_recodificados$PIB yaxt="n")
axis(2, at=seq(0, 45, by=5))

#curva densidad
plot(density(datos_recodificados$Curricular.units.1st.sem..evaluations.))
hist(datos_recodificados$Curricular.units.1st.sem..evaluations., breaks=seq(0, 46, by=1), xaxt="n")
axis(1, at=seq(2, 46, by=1))
describe(datos_recodificados$Curricular.units.1st.sem..evaluations.)

#Cálculo moda 
names(sort(table(datos_recodificados$Curricular.units.1st.sem..evaluations.), decreasing = TRUE))[1]
x<-prop.table(table(datos_recodificados$Curricular.units.1st.sem..evaluations.))
barplot(x)   

Aahora separación según las carreras
boxplot(datos_recodificados$Curricular.units.1st.sem..evaluations.~datos_recodificados$Course, las=2, cex.axis=0.6) #las=2 para poner verticales los nombres y así se ven, 
par(mar=c(13,4,4,2))  #para dejar más espacio abajo (el primer elemento del vector es el espacio de abajo)

#descriptivo  e histograma de evaluaciones en multimedia especificamente
descriptive(datos_recodificados$Curricular.units.2nd.sem..evaluations.[datos_recodificados$Course=="Diseño de Animación y Multimedia"])
unique(datos_recodificados$Course)

hist(datos_recodificados$Curricular.units.1st.sem..evaluations.[datos_recodificados$Course=="Diseño de Animación y Multimedia"])

#Comparación grado con dropout
tabla<-table(datos_recodificados$Course, datos_recodificados$Target)
mosaicplot(tabla, las=2, cex.axis = 0.7)#hay gente parecida en multimedia q en el resto, entonce no entiendo muy bien pq hay tan pocas evaluaciones

#Descriptivo por carrera:
tapply(datos_recodificados$Curricular.units.1st.sem..evaluations., datos_recodificados$Course, descriptive)

#2SEM
#descriptive general
descriptive(datos_recodificados$Curricular.units.2nd.sem..evaluations.)
#descriptivo por pib/año
tapply(datos_recodificados$Curricular.units.2nd.sem..evaluations., datos_recodificados$PIB, descriptive) #sale lo del final pq descriptive es una funcion q cada vez q se eejcuta imprime aparte de return, entonces mientras se ejecuta me lo va enseñando, y al final me pone el vector del valor_PIB: resultado, para los tres casos: variable numerica(mi caso, evaluationes es numerica), categorica(no solo le hje dado una variable numerica asi q no sale nada) y fecha(lo mismo, null).
boxplot(datos_recodificados$Curricular.units.2nd.sem..evaluations.~datos_recodificados$PIB)
#para carreras en cada año


ggplot(datos_recodificados,
       aes(x = factor(PIB),
           y = Curricular.units.2nd.sem..evaluations.)) +
  geom_boxplot() +
  facet_wrap(~ Course) +
  labs(
    x = "PIB",
    y = "Evaluaciones 2º semestre",
    title = "Distribución de evaluaciones por PIB en cada carrera"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#vemos si en Multimedia tiene que ver target con pib
tablamulti<-table(datos_recodificados$PIB[datos_recodificados$Course =="Diseño de Animación y Multimedia"], datos_recodificados$Target[datos_recodificados$Course =="Diseño de Animación y Multimedia"])
mosaicplot(tablamulti)

#***********************
#C.U. APPROVED
#***********************

#1SEM
#¿Tienen que ver los aprobados y a cuántas evaluaciones te presentas?
datos_recodificados$Curricular.units.1st.sem..approved.
descriptive(datos_recodificados$Curricular.units.1st.sem..approved.)
boxplot(datos_recodificados$Curricular.units.1st.sem..approved.~ datos_recodificados$Course, las=2)
#otra vez salen los 0 de multimedia

#Vemos si dependen del año
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

#Aproximamos a normal
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
#C. U. ENROLLED
#****************************
#1SEM
datos_recodificados$Curricular.units.1st.sem..enrolled.
descriptive(datos_recodificados$Curricular.units.1st.sem..enrolled.)
hist(datos_recodificados$Curricular.units.1st.sem..enrolled., col="pink", breaks=seq(0, 26, by=1), xaxt="n", yaxt="n")
axis(1, at=seq(0, 26, by=1),las=2)
axis(2, at=seq(0, 2000, by=100), las=2)
boxplot(datos_recodificados$Curricular.units.1st.sem..enrolled.~datos_recodificados$Course, las=2, cex.axis=0.7)

#Ver si quien no se matriculan a ninguna u.c. en el semestre 1 tampoco lo hacen en el semestre 2
datos_recodificados$Curricular.units.2nd.sem..enrolled.[datos_recodificados$Curricular.units.1st.sem..enrolled.==0]



#2SEM
descriptive(datos_recodificados$Curricular.units.2nd.sem..enrolled.)
hist(datos_recodificados$Curricular.units.2nd.sem..enrolled., col="pink", breaks=seq(0, 26, by=1), xaxt="n", yaxt="n")
axis(1, at=seq(0, 26, by=1),las=2)
axis(2, at=seq(0, 2000, by=100), las=2)
boxplot(datos_recodificados$Curricular.units.2nd.sem..enrolled.~datos_recodificados$Course, las=2, cex.axis=0.7)

#**************************
#C.U. CREDITED (RECONOCIDAS/CONVALIDADAS)
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
#boxplot por carreras
boxplot(datos_recodificados$Curricular.units.1st.sem..credited.~datos_recodificados$Course, las=2)

#2SEM
descriptive(datos_recodificados$Curricular.units.2nd.sem..credited.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..credited.~datos_recodificados$Course, las=2)
tabla_cred2<-table(datos_recodificados$Curricular.units.2nd.sem..credited.)
tabla_cred2
barplot(tabla_cred2)

#**************************
#COMPARACIÓN VARIABLES NOTAS
#***************************
#Describe y descriptivos de cada variable por separado
datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0]
comparacion_semestres_y_nota_entrada<-c("Previous.qualification.grade_10",
                                        "Admission.grade_10",
                                        "Curricular.units.1st.sem.grade_10",
                                        "Curricular.units.2nd.sem.grade_10", 
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
descriptive(datos_modelo$Curricular.units.2nd.sem.grade_10[datos_modelo$Curricular.units.2nd.sem.grade_10!=0])


#Boxplots
boxplot(datos_recodificados$Previous.qualification.grade_10,
        yaxt = "n",
        ylab = "Nota",
        main = "Nota estudios previos")
axis(2, at = seq(4, 10, by = 0.5))
boxplot(datos_recodificados$Age.at.enrollment)
boxplot(datos_recodificados$Admission.grade_10, yaxt="n", main="Nota admisión")
axis(2, at=seq(4.5,10, by=0.5))
boxplot(datos_recodificados$Application.order)
boxplot(datos_recodificados$Curricular.units.1st.sem..credited.)
boxplot(datos_recodificados$Curricular.units.1st.sem..enrolled., yaxt="n")
axis(2, at=seq(0,25, by=1))
boxplot(datos_recodificados$Curricular.units.1st.sem..evaluations., yaxt="n", main="Evaluations 1º Sem.")
axis(2, at=seq(0,45, by=3))
boxplot(datos_recodificados$Curricular.units.1st.sem..approved., yaxt="n")
axis(2, at=seq(0,25, by=1))
boxplot(datos_recodificados$Curricular.units.1st.sem.grade_10, yaxt="n", main="Notas 1º sem.")
axis(2, at=seq(0,10, by=1))
boxplot(datos_recodificados$Curricular.units.1st.sem..without.evaluations.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..credited.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..enrolled.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..evaluations.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..approved.)
boxplot(datos_recodificados$Curricular.units.2nd.sem.grade_10, main="Notas 2º sem.")
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
#Papel probabilístico normal
#datos$Admission.grade
qqnorm(
  datos_recodificados$Admission.grade_10,
  main = "Papel probabilístico normal Admission Grade",
)
qqline(datos_recodificados$Admission.grade_10)
grid()
#Previous.qualification.grade_10
qqnorm(
  datos_recodificados$Previous.qualification.grade_10,
  main = "Papel probabilístico normal Admission Grade",
)
qqline(datos_recodificados$Previous.qualification.grade_10)
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
#papel probabilístico exponencial
x_exp <- datos_recodificados$Admission.grade_10 - 4.75 #le restamos 4.75 para q el mínimo sea 0
qqplot(
  qexp(ppoints(length(x_exp)), rate = 1/mean(x_exp)),
  sort(x_exp),
  main = "QQ plot exponencial",
  xlab = "Cuantiles teóricos",
  ylab = "Cuantiles observados"
)

abline(0,1)
#Estudio de las variables de notas de 1 y 2 sem
#1SEM
#histograma
hist(datos_recodificados$Curricular.units.1st.sem.grade_10, breaks=seq(0, 10, by=0.5), xaxt="n",  main="Notas 1º sem.")
axis(1, at=seq(0,10, by=0.5))
hist(datos_recodificados$Previous.qualification.grade_10, breaks=seq(0, 10, by=0.5), xaxt="n",  main="Nota estudios previos
")
axis(1, at=seq(0,10, by=0.5))
hist(datos_recodificados$Admission.grade_10, breaks=seq(0, 10, by=0.5), xaxt="n",main="Nota admisión" )
axis(1, at=seq(0,10, by=0.5))
#media de solo los alumnos que han aprobado
median(datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0])
mean(datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0])

#2 SEM
hist(datos_recodificados$Curricular.units.2nd.sem.grade_10, breaks=seq(0, 10, by=0.5), xaxt="n", main="Notas 2º sem." )
axis(1, at=seq(0,10, by=0.5))

median(datos_recodificados$Curricular.units.2nd.sem.grade_10[datos_recodificados$Curricular.units.2nd.sem.grade_10!=0])
mean(datos_recodificados$Curricular.units.2nd.sem.grade_10[datos_recodificados$Curricular.units.2nd.sem.grade_10!=0])


#ver diferencia de ceros en nota cada semestre
table(datos_modelo$Curricular.units.1st.sem.grade_10[datos_modelo$Curricular.units.1st.sem.grade_10==0])
table(datos_modelo$Curricular.units.2nd.sem.grade_10[datos_modelo$Curricular.units.2nd.sem.grade_10==0])


#*********************
#Application order
#*********************
descriptive(datos_recodificados$Application.order)
tabla_orden<-table(datos_recodificados$Application.order)
barplot(tabla_orden,  yaxt="n")
axis(2, at=seq(0, 3500, by=100), las=2)


#***********
#Inflation rate
#***********
descriptive(datos_recodificados$Inflation.rate)
barplot(table(datos_recodificados$Inflation.rate))
table(datos_recodificados$Inflation.rate)
#***********
#PIB
#***********
descriptive(datos_recodificados$PIB)
barplot(table(datos_recodificados$PIB))
table(datos_recodificados$PIB)
#***********
#Unemployment
#***********
descriptive(datos_recodificados$Unemployment.rate)
barplot(table(datos_recodificados$Unemployment.rate))
table(datos_recodificados$Unemployment.rate)



