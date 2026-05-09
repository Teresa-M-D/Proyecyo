#Espacio de Yalda
library(clickR)
library(rio)
library(ggplot2)
library(plotly)
library(GGally)
datos <- read.csv("estudiantes.csv", sep = ";", header = TRUE)
descriptive(datos)
descriptive(datos_modelo)
#Análisis inicial de la variable Target

#Tabla de frecuencias absolutas
freq_target <- table(datos_modelo$Objetivo)
freq_target
#Frecuencias relativas
prop.table(freq_target)

#Porcentajes
porcentajes = prop.table(freq_target) * 100
porcentajes 
#Diagrama de barras
bp <- barplot(
  freq_target,
  col = c("indianred", "peachpuff", "palegreen1"),
  las = 1,
  main = "Target",
  xlab = "Frecuencias absolutas (n)",
  ylim = c(0, max(freq_target) * 1.1)   
)

text(bp, freq_target, labels = freq_target, pos = 3)

#Nacionality_group
freq_nacionality_group <- table(datos_modelo$Nationality_group)
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

#tuition fees up to date
freq_tuition <- table(datos_modelo$Tuition.fees.up.to.date)
freq_tuition 
bp3 <- barplot(
  freq_tuition,
  col = c("indianred", "lightgreen"),
  las = 1,
  main = "Tasas de matrícula actualizadas",
  xlab = "Frecuencias absolutas (n)",
  ylim = c(0, max(freq_tuition) * 1.3)
)
text(bp3, freq_tuition, labels = freq_tuition, pos = 3)

#Course
freq_course <- table(datos_modelo$Course_limpio)
freq_course
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
freq_courseG <- table(datos_modelo$Course_group)
freq_courseG
#como son muchas titulaciones, hacemos un diagrama de barras horizonatal
library(ggplot2)
library(dplyr)

datos_modelo %>%
  count(Course_group) %>% 
  ggplot(aes(x = n, y = Course_group)) +
  geom_bar(stat = "identity", fill = "#4C72B0") +
  labs(
    title = "Frecuencia de estudiantes por área",
    x = "Frecuencia",
    y = "Área"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold")
  )


#Valores atípicos en variables numéricas
boxplot(datos$Previous.qualification..grade., main = "Calificación de admisión (del 0 a 200)")
boxplot(datos$Unemployment.rate, main= "Tasa de desmpleo")
boxplot(datos$Inflation.rate, main= "Tasa de inflación")
boxplot(datos$GDP, main= "PIB")

#No tiene sentido hacer boxplot de variables numericas discretas!!!!


#Transformación lineal de la variable Calificación de admisión, en concreto, un cambio de escala (del 1 al 10)
datos$NotaAdmisión <- (datos$Previous.qualification..grade. / 200) * 10
descriptive(datos)
boxplot(datos$NotaAdmisión, main="Nota de Admisión (del 0 al 10)")


#Reagrupacion variable Target:
datos_modelo$Target_bin <- ifelse(datos_modelo$Target == "Dropout", "Abandono", "No Abandono")
datos_modelo$Target_bin <- as.factor(datos_modelo$Target_bin)

#Reagrupación variable Target (incluyendo a los matriculados)
datos_modelo$Objetivo <- ifelse(
  datos_modelo$Target == "Dropout", "Abandono",
  ifelse(datos_modelo$Target == "Enrolled", "Matriculados", "Graduados")
)

datos_modelo$Objetivo <- as.factor(datos_modelo$Objetivo)

#NUEVA VARIBLE: aprobados_reales_1sem y aprobados_reales_2sem
datos_modelo$aprobados_reales_1sem <- pmin(datos_modelo$Curricular.units.1st.sem..approved. , datos_modelo$Carga_academica_real)
datos_modelo$aprobados_reales_2sem <- pmin(datos_modelo$Curricular.units.2nd.sem..approved. , datos_modelo$Carga_academica_real_sem_2)
datos_modelo$aprobados_reales_2sem

sum(datos_modelo$aprobados_reales_1sem == 0)
sum(datos_modelo$Curricular.units.1st.sem..approved. == 0)
#ANÁLISIS BIVARIANTE

#Objetivo: comparar las medias de la notas del primer cuatrimestre de los estudiantes que abandonaron y los que no lo hicieron


# Calculamos la media del PIB para los estudiantes que abandonaron
mean_notas1_dropouts <- mean(Dropouts$Curricular.units.1st.sem.grade_10)

# Calculamos la media de las notas del primer cuatrimestre de los estudiantes que no abandonaron (graduados o siguen matriculados )
mean_notas1_noDropouts <- mean(noDropouts$Curricular.units.1st.sem.grade_10)

# Mostramos los resultados en pantalla

mean_notas1_dropouts #3.836201 (sobre 10)
mean_notas1_noDropouts #6.338498 (sobre 10)

#Vamos a hacer una comparación de medianas para comprobar si los resultados de las medias se ven afectadas por la presencia de valores átipicos

median_notas1_dropouts <- median(Dropouts$Curricular.units.1st.sem.grade_10)
median_notas1_noDropouts <- median(noDropouts$Curricular.units.1st.sem.grade_10)

median_notas1_dropouts #5.5 (sobre 10)
median_notas1_noDropouts # 6.4 (sobre 10)

#Como podemos ver la mediana de las notas del primer cuatrimestre del grupo Dropout es más mayor que la
#media debido a la presencia de valores extremos bajos que tiran de la media hacia abajo (como vimos en el diagrama de cajas multiple)


#Objetvo: comparar las medias de las notas del 2 cuatrimistre entre estudiantes que abandonaron y estudiantes que no


mean_notas2_dropouts <- mean(Dropouts$Curricular.units.2nd.sem.grade_10)
mean_notas2_noDropouts <- mean(noDropouts$Curricular.units.2nd.sem.grade_10)

# Mostramos los resultados en pantalla

mean_notas2_dropouts #3.118661 (sobre 10)
mean_notas2_noDropouts #6.35784 (sobre 10)

#Vamos a hacer la mediana ahora

median_notas2_dropouts <- median(Dropouts$Curricular.units.2nd.sem.grade_10)
median_notas2_noDropouts <- median(noDropouts$Curricular.units.2nd.sem.grade_10)

# Mostramos los resultados en pantalla

median_notas2_dropouts #5 (sobre 10)
median_notas2_noDropouts #6.4 (sobre 10)

#La mediana de las notas del segundo cuatrimestre es más baja que su media, lo que significa que hay valores extremos altos que tiran de la media hacia arriba.
#La mediana y la media de las notas de los que no abandonaron es muy similar

median_notaPrevia_dropouts <- median(Dropouts$Previous.qualification.grade_10)
median_notaPrevia_Nodropouts <- median(noDropouts$Previous.qualification.grade_10)

median_notaPrevia_dropouts
median_notaPrevia_Nodropouts

median_notaAdmision_dropouts <- median(Dropouts$Admission.grade_10)
median_notaAdmision_Nodropouts <- median(noDropouts$Admission.grade_10)

median_notaAdmision_dropouts
median_notaAdmision_Nodropouts

mean_notaPrevia_dropouts <- mean(Dropouts$Previous.qualification.grade_10)
mean_notaPrevia_Nodropouts <- mean(noDropouts$Previous.qualification.grade_10)

mean_notaPrevia_dropouts
mean_notaPrevia_Nodropouts

#T-TEST
t.test(Curricular.units.1st.sem.grade_10 ~ Target_bin, data=datos_modelo)
t.test(Curricular.units.2nd.sem.grade_10 ~ Target_bin, data=datos_modelo)
t.test(PIB ~ Target_bin, data=datos_modelo)
t.test(Unemployment.rate ~ Target_bin, data=datos_modelo)
t.test(Inflation.rate ~ Target_bin, data=datos_modelo)
t.test(Admission.grade_10  ~ Target_bin, data=datos_modelo)
t.test(Previous.qualification.grade_10  ~ Target_bin, data=datos_modelo)

#Comparación de medianas de las variables macroeconomicas respecto Target_bin

median_PIB_dropouts <- median(Dropouts$PIB)
median_PIB_noDropouts <- median(noDropouts$PIB)

median_PIB_dropouts #0.32
median_PIB_noDropouts #0.79

median_desempleo_dropouts <- median(Dropouts$Unemployment.rate)
median_desempleo_noDropouts <- median(noDropouts$Unemployment.rate)

median_desempleo_dropouts #11.1
median_desempleo_noDropouts #11.1

median_inflacion_dropouts <- median(Dropouts$Inflation.rate)
median_inflacion_noDropouts <- median(noDropouts$Inflation.rate)

median_inflacion_dropouts #1.4
median_inflacion_noDropouts #1.4

#Yuen's test
install.packages("WRS2")
library(WRS2)
yuen(Curricular.units.1st.sem.grade_10 ~ Target_bin, data = datos_modelo, tr = 0.2)
yuen(Curricular.units.2nd.sem.grade_10 ~ Target_bin, data = datos_modelo, tr = 0.2)

#Test Mann - Whitney
wilcox.test(Curricular.units.1st.sem.grade_10 ~ Target_bin, data = datos_modelo)
wilcox.test(Curricular.units.2nd.sem.grade_10 ~ Target_bin, data = datos_modelo)
wilcox.test(Porcentaje_aprobado_sem_1 ~ Target_bin, data = datos_modelo)
wilcox.test(Porcentaje_aprobado_sem_2  ~ Target_bin, data = datos_modelo)
wilcox.test(PIB ~ Target_bin, data=datos_modelo)
wilcox.test(Unemployment.rate ~ Target_bin, data=datos_modelo)
wilcox.test(Inflation.rate ~ Target_bin, data=datos_modelo)
wilcox.test(Admission.grade_10  ~ Target_bin, data=datos_modelo)
wilcox.test(Previous.qualification.grade_10  ~ Target_bin, data=datos_modelo)
wilcox.test(Carga_academica_real  ~ Target_bin, data=datos_modelo)
wilcox.test(Carga_academica_real_sem_2  ~ Target_bin, data=datos_modelo)
wilcox.test(Age.at.enrollment  ~ Target_bin, data=datos_modelo)
#Tamaño del efecto
install.packages("rstatix")
library(rstatix)

wilcox_effsize(data = datos_modelo , Curricular.units.1st.sem.grade_10 ~ Target_bin)
wilcox_effsize(Curricular.units.2nd.sem.grade_10 ~ Target_bin, data = datos_modelo)
wilcox_effsize(Porcentaje_aprobado_sem_1 ~ Target_bin, data = datos_modelo)
wilcox_effsize(Porcentaje_aprobado_sem_2 ~ Target_bin, data = datos_modelo)
wilcox_effsize(PIB ~ Target_bin, data=datos_modelo)
wilcox_effsize(Unemployment.rate ~ Target_bin, data=datos_modelo)
wilcox_effsize(Inflation.rate ~ Target_bin, data=datos_modelo)
wilcox_effsize(Admission.grade_10  ~ Target_bin, data=datos_modelo)
wilcox_effsize(Previous.qualification.grade_10  ~ Target_bin, data=datos_modelo)
wilcox_effsize(Carga_academica_real  ~ Target_bin, data=datos_modelo)
wilcox_effsize(Carga_academica_real_sem_2  ~ Target_bin, data=datos_modelo)
wilcox_effsize(Age.at.enrollment  ~ Target_bin, data=datos_modelo)
#GRAFICOS DE ASOCIACIÓN
install.packages(c("scatterplot3d", "vcd"))
library(vcd)
tabla1 <- xtabs(~ datos_modelo$Target_bin + datos_modelo$Gender)
tabla1
assoc(tabla1, shade = TRUE, col = c("lightblue", "lightcoral"))

tabla2 <- xtabs(~ datos_modelo$Target_bin + datos_modelo$Tuition.fees.up.to.date)
assoc(tabla2, shade = TRUE, col = c("lightblue", "lightcoral"))


tabla3 <- xtabs(~ datos_modelo$Target_bin + datos_modelo$Scholarship.holder)
assoc(tabla3, shade = TRUE, col = c("lightblue", "lightcoral"))

#Diagrama de barras bivariante
tabla4 <- xtabs(~ datos_modelo$Objetivo + datos_modelo$Course_limpio)
par(xpd = TRUE, mar = c(5, 18, 4, 8)) 
barplot(tabla4,  col=c("indianred2", "lightblue", "lightgreen"), horiz=TRUE, las = 1, cex.names = 0.8)
legend("topright", legend=c("Abandono","Matriculado","Graduado"),
       fill=c("indianred2","lightblue","lightgreen"), inset=c(-0.2,0))

#BOXPLOTS MÚLTIPLES
dev.off() # resetea la ventana de gráficos

#Boxplots con la variable target antes de la reagrupación
boxplot(Admission.grade ~ Target, data=datos_modelo, las=1)
boxplot(Admission.grade_10 ~ Target, data=datos_modelo, las=1)
boxplot(Previous.qualification.grade_10 ~ Target, data=datos_modelo, las=1)
boxplot(Curricular.units.1st.sem.grade_10 ~ Target, data=datos_modelo, las=1)
boxplot(Curricular.units.2nd.sem.grade_10 ~ Target, data=datos_modelo, las=1)


#Boxplots de la variable target después de la reagrupación
boxplot(Admission.grade_10 ~ Target_bin, data=datos_modelo, las=1, col = c("indianred2", "lightgreen"),  main= "Nota de admisión y abandono")
boxplot(Previous.qualification.grade_10 ~ Target_bin, data=datos_modelo, las=1, col = c("indianred2", "lightgreen"),  main= "Calificación previa y abandono" )
boxplot(Curricular.units.1st.sem.grade_10 ~ Target_bin, data=datos_modelo, las=1, col = c("indianred2", "lightgreen"),  main= "Notas del primer semestre y abandono")
boxplot(Curricular.units.2nd.sem.grade_10 ~ Target_bin, data=datos_modelo, las=1, col = c("indianred2", "lightgreen"),  main= "Notas del segundo semestre y abandono")
boxplot(Porcentaje_aprobado_sem_1  ~ Target_bin, data=datos_modelo, las=1, col = c("indianred2", "lightgreen"),  main= "Porcentaje evaluaciones aprobadas 1º sem y abandono")
boxplot(Porcentaje_aprobado_sem_2  ~ Target_bin, data=datos_modelo, las=1, col = c("indianred2", "lightgreen"),  main= "Porcentaje evaluaciones aprobadas 2º sem y abandono")
boxplot(PIB ~ Target_bin, data=datos_modelo, las=1, col = c("indianred2", "lightgreen"),  main= "PIB y abandono")
boxplot(Unemployment.rate ~ Target_bin, data=datos_modelo, las=1, col = c("indianred2", "lightgreen"),  main= "Tasa de desempleo y abandono")
boxplot(Age.at.enrollment  ~ Target_bin, data=datos_modelo, las=1, col = c("indianred2", "lightgreen"),  main= "Edad de matriculación y abandono")


install.packages("tidyverse")
library(tidyverse)

#NOTAS POR SEMESTRE SEGÚN ABANDONO

# Convertir a formato largo
df_long1 <- datos_modelo %>%
  pivot_longer(
    cols = c(Curricular.units.1st.sem.grade_10,
             Curricular.units.2nd.sem.grade_10),
    names_to = "Semestre",
    values_to = "Nota"
  ) %>%
  mutate(
    Semestre = recode(Semestre,
                      "Curricular.units.1st.sem.grade_10" = "1º Semestre",
                      "Curricular.units.2nd.sem.grade_10" = "2º Semestre")
  )

# Gráfico combinado
ggplot(df_long1, aes(x = Target_bin, y = Nota, fill = Semestre)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Abandono",
    y = "Nota",
    fill = "Semestre",
    title = "Notas por semestre según abandono"
  ) +
  theme_minimal(base_size = 14)

#PORCENTAJE EVALUACIONES APROBADAS POR SEMESTRE SEGUN ABANDONO

# Convertir a formato largo
df_long2 <- datos_modelo %>%
  pivot_longer(
    cols = c(Porcentaje_aprobado_sem_1,
             Porcentaje_aprobado_sem_2),
    names_to = "Semestre",
    values_to = "Porcentaje_evaluaciones_aprobadas"
  ) %>%
  mutate(
    Semestre = recode(Semestre,
                      "Porcentaje_aprobado_sem_1" = "1º Semestre",
                      "Porcentaje_aprobado_sem_2" = "2º Semestre")
  )


# línea para sustituir NA por 0
df_long2$Porcentaje_evaluaciones_aprobadas[
  is.na(df_long2$Porcentaje_evaluaciones_aprobadas)
] <- 0

# Gráfico combinado
ggplot(df_long2, aes(x = Target_bin, y = Porcentaje_evaluaciones_aprobadas, fill = Semestre)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Abandono",
    y = "Porcentaje evaluaciones aprobadas",
    fill = "Semestre",
    title = "Porcentaje evaluaciones aprobadas por semestre según abandono"
  ) +
  theme_minimal(base_size = 14)

summary(datos_modelo$Porcentaje_aprobado_sem_1) #Hay 169 Na´s en porcentaje aprobados 1º semestre
summary(datos_modelo$Porcentaje_aprobado_sem_2) #Hay 221 Na´s en porcentaje aprobados 2º semestre

#Los Na's se deben ha que hay valores 0/0 que R convierte en Na's. De hecho si vemos el número de estudiantes con 0 unidades curriculares aprobadas 
#y cero evaluaciones los números coinciden

sum(datos_modelo$Curricular.units.1st.sem..approved. == 0 &
      datos_modelo$Curricular.units.1st.sem..evaluations. == 0) #Hay 169 estudiantes con cero unidades curriculares aprobadas y cero evaluaciones

sum(datos_modelo$Curricular.units.2nd.sem..approved. == 0 &
      datos_modelo$Curricular.units.2nd.sem..evaluations. == 0) #Hay 221 estudiantes con cero unidades curriculares aprobadas y cero evaluaciones

#CARGA ACADEMICA REAL POR SEMESTRE SEGÚN ABANDONO

# Convertir a formato largo
df_long3 <- datos_modelo %>%
  pivot_longer(
    cols = c(Carga_academica_real,
             Carga_academica_real_sem_2),
    names_to = "Semestre",
    values_to = "Carga_academica_real"
  ) %>%
  mutate(
    Semestre = recode(Semestre,
                      "Carga_academica_real" = "1º Semestre",
                      "Carga_academica_real_sem_2" = "2º Semestre")
  )


# Gráfico combinado
ggplot(df_long3, aes(x = Target_bin, y = Carga_academica_real, fill = Semestre)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Abandono",
    y = "Carga académica real",
    fill = "Semestre",
    title = "Carga académica real por semestre según abandono"
  ) +
  theme_minimal(base_size = 14)


