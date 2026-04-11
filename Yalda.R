#Espacio de Yalda
library(clickR)
library(rio)
library(ggplot2)
library(plotly)
library(GGally)
datos <- read.csv("estudiantes.csv", sep = ";", header = TRUE)
descriptive(datos)
#Análisis inicial de la variable Target

#Tabla de frecuencias absolutas
freq_target <- table(datos$Target)
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

#tuition fees up to date
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

#ANÁLISIS BIVARIANTE

#Objetivo: Comparar la media del PIB entre los estudiantes que abandonan y los que se gradúan.


# Filtramos los estudiantes cuyo estado final es "Dropout"
# Esto crea un subconjunto del dataset solo con los alumnos que abandonaron
dropouts <- datos[datos$Target == "Dropout", ]


# Filtramos los estudiantes cuyo estado final es "Graduate"
# Este subconjunto contiene únicamente a los alumnos que se graduaron  
graduates <- datos[datos$Target == "Graduate", ]

# Calculamos la media del PIB para los estudiantes que abandonaron
mean_pib_dropouts <- mean(dropouts$GDP)

# Calculamos la media del PIB para los estudiantes que se graduaron
mean_pib_graduates <- mean(graduates$GDP)

# Mostramos los resultados en pantalla

mean_pib_dropouts #-0.1508586
mean_pib_graduates #0.08183341

#Interpretación sencilla:

#Los estudiantes que abandonaron entraron en años económicamente peores
#Los que se graduaron entraron en años mejores.

#Objetivo: comparar la medias de tasa de desempleo entre los estudiantes que abandonan y los que se graduan

mean_desempleo_dropouts <- mean(dropouts$Unemployment.rate)
mean_desempleo_graduates <- mean(graduates$Unemployment.rate)

mean_desempleo_dropouts #11.6164
mean_desempleo_graduates #11.63934

#Interpretación: 
#Vemos que las medias de desempleo son muy parecidas, siendo un poco mayor la de los estudiantes que se graduaron
#lo que significa que entraron cuando la tasa de desempleo era un poco más alta


#Objetivo: comparar la medias de tasa de inflacio entre los estudiantes que abandonan y los que se graduan


mean_inflacion_dropouts <- mean(dropouts$Inflation.rate)
mean_inflacion_graduates <- mean(graduates$Inflation.rate)

mean_inflacion_dropouts #1.283955
mean_inflacion_graduates #1.197918

#Interpretación:
#Los estudiantes que abandonaron entraron en años donde la tasa de inflación era más alta
#Los que se graduaron entraron en años donde la tasa de inflación era un poco más baja 

#GRAFICOS DE ASOCIACIÓN
install.packages(c("scatterplot3d", "vcd"))
library(vcd)
tabla1 <- xtabs(~ datos_recodificados$Target + datos_recodificados$Gender)
tabla1
assoc(tabla1, shade = TRUE, col = c("lightblue", "lightcoral"))

tabla2 <- xtabs(~ datos_recodificados$Target + datos_recodificados$Tuition.fees.up.to.date)
assoc(tabla2, shade = TRUE, col = c("lightblue", "lightcoral"))


tabla3 <- xtabs(~ datos_recodificados$Target + datos_recodificados$Scholarship.holder)
assoc(tabla3, shade = TRUE, col = c("lightblue", "lightcoral"))

#Diagrama de barras bivariante
tabla4 <- xtabs(~ datos_recodificados$Target + datos_recodificados$Course)
par(mar = c(5, 18, 4, 2))   
barplot(tabla4, legend=TRUE, col=c("indianred2", "lightblue", "lightgreen"), horiz=TRUE, las = 1, cex.names = 0.8)


#BOXPLOTS MÚLTIPLES
dev.off() # resetea la ventana de gráficos
boxplot(Admission.grade ~ Target, data=datos_recodificados, las=1)
boxplot(Admission.grade_10 ~ Target, data=datos_recodificados, las=1)
boxplot(Previous.qualification.grade_10 ~ Target, data=datos_recodificados, las=1)
boxplot(Curricular.units.1st.sem.grade_10 ~ Target, data=datos_recodificados, las=1)
boxplot(Curricular.units.2nd.sem.grade_10 ~ Target, data=datos_recodificados, las=1)
