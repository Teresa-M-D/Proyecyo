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
freq_target <- table(datos_modelo$Target)
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
  main = "Tasas de mátricula actualizadas",
  xlab = "Frecuencias absolutas (n)",
  ylim = c(0, max(freq_tuition) * 1.3)
)
text(bp3, freq_tuition, labels = freq_tuition, pos = 3)

#Course
freq_course <- table(datos_modelo$Course_limpio)
freq_course
#como son muchas titulaciones, hacemos un diagrama de barras horizonatal
library(ggplot2)
library(dplyr)

datos_modelo %>%
  count(Course_limpio) %>% 
  ggplot(aes(x = n, y = Course_limpio)) +
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
  ifelse(datos_modelo$Target == "Enrolled", "Matriculados", "No Abandono")
)

datos_modelo$Objetivo <- as.factor(datos_modelo$Objetivo)

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
wilcox.test(PIB ~ Target_bin, data=datos_modelo)
wilcox.test(Unemployment.rate ~ Target_bin, data=datos_modelo)
wilcox.test(Inflation.rate ~ Target_bin, data=datos_modelo)
wilcox.test(Admission.grade_10  ~ Target_bin, data=datos_modelo)
wilcox.test(Previous.qualification.grade_10  ~ Target_bin, data=datos_modelo)

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
boxplot(PIB ~ Target_bin, data=datos_modelo, las=1, col = c("indianred2", "lightgreen"),  main= "PIB y abandono")
boxplot(Unemployment.rate ~ Target_bin, data=datos_modelo, las=1, col = c("indianred2", "lightgreen"),  main= "Tasa de desempleo y abandono")
boxplot(Inflation.rate ~ Target_bin, data=datos_modelo, las=1, col = c("indianred2", "lightgreen"),  main= "Tasa de inflación y abandono")

