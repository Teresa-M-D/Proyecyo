#========================
# VARIABLES DERIVADAS
#========================

# Target binario
datos_recodificados$Target_bin <- ifelse(
  datos_recodificados$Target == "Dropout",
  "Abandono",
  "No abandono"
)

#========================
# AÑOS A PARTIR DEL PIB
#========================

tabla_años <- data.frame(
  GDP = c(0.32, -3.12, 1.74, -1.70, -4.06, -0.92, 0.79, 1.79, 2.02, 3.51),
  year = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
)

datos_recodificados$year <- tabla_años$year[
  match(
    round(datos_recodificados$PIB, 2),
    round(tabla_años$GDP, 2)
  )
]

#========================
# BOXPLOT MULTIMEDIA
#========================

boxplot(
  datos_recodificados$Curricular.units.1st.sem..enrolled.[
    datos_recodificados$Course=="Diseño de Animación y Multimedia"
  ] ~
    
    factor(
      datos_recodificados$year[
        datos_recodificados$Course=="Diseño de Animación y Multimedia"
      ],
      levels = 2008:2017
    ),
  
  xlab = "Año",
  ylab = "Asignaturas matriculadas",
  main = "Diseño de Animación y Multimedia",
  col = "lightblue"
)

#========================
# CASOS PROBLEMÁTICOS
#========================

datos_recodificados[
  datos_recodificados$year <= 2015 &
    datos_recodificados$Course == "Diseño de Animación y Multimedia" &
    datos_recodificados$Curricular.units.1st.sem..enrolled. == 0,
]

#========================
# HISTOGRAMA MEJORADO
#========================

hist(
  datos_modelo$Carga_academica_real,
  
  breaks = 15,
  col = "lightblue",
  border = "white",
  
  main = "Distribución de la carga académica real",
  xlab = "Carga académica real",
  ylab = "Frecuencia",
  
  cex.main = 1.3,
  cex.lab = 1.1
)

#========================
# HISTOGRAMA GGplot
#========================

library(ggplot2)

ggplot(datos_modelo,
       aes(
         x = Carga_academica_real,
         
         fill = after_stat(
           ifelse(
             x >= 4 & x <= 6,
             "central",
             "resto"
           )
         )
       )) +
  
  geom_histogram(
    bins = 15,
    color = "black"
  ) +
  
  scale_fill_manual(
    name = "Zona",
    
    values = c(
      "central" = "coral",
      "resto" = "grey90"
    ),
    
    labels = c(
      "Zona central",
      "Resto"
    )
  ) +
  
  annotate(
    "text",
    x = 5,
    y = 1700,
    label = "71% de los estudiantes",
    size = 6,
    fontface = "bold",
    color = "blue"
  ) +
  
  labs(
    x = "Carga académica real",
    y = "Frecuencia"
  ) +
  
  theme_minimal(base_size = 16) +
  
  theme(
    plot.title = element_text(size = 18),
    
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

#========================
# DENSIDADES DE NOTAS
#========================

library(dplyr)
library(tidyr)

datos_long <- datos_modelo %>%
  pivot_longer(
    cols = c(
      Curricular.units.1st.sem.grade_10,
      Curricular.units.2nd.sem.grade_10,
      Previous.qualification.grade_10,
      Admission.grade_10
    ),
    
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  
  filter(
    !(Variable == "Curricular.units.2nd.sem.grade_10" &
        Valor == 0)
  )

ggplot(datos_long,
       aes(
         x = Valor,
         color = Variable
       )) +
  
  geom_density(linewidth = 1.2) +
  
  scale_color_discrete(
    labels = c(
      "Nota estudios previos",
      "Nota media 2º semestre",
      "Nota media 1º semestre",
      "Nota de admisión"
    )
  ) +
  
  labs(
    color = "Variable",
    x = "Nota sobre 10",
    y = "Densidad"
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15)
  )

#========================
# WITHOUT EVALUATIONS
#========================

datos_long <- datos_modelo %>%
  pivot_longer(
    cols = c(
      Curricular.units.1st.sem..without.evaluations.,
      Curricular.units.2nd.sem..without.evaluations.
    ),
    
    names_to = "Variable",
    values_to = "Valor"
  )

ggplot(
  datos_long,
  
  aes(
    x = factor(Valor),
    fill = Variable
  )
) +
  
  geom_bar(position = "dodge") +
  
  scale_fill_discrete(
    labels = c(
      "Sin evaluación 1º semestre",
      "Sin evaluación 2º semestre"
    )
  ) +
  
  labs(
    fill = "Variable",
    x = "Número de asignaturas sin evaluación",
    y = "Frecuencia"
  ) +
  
  theme_minimal(base_size = 16)

#========================
# MATRIZ DE CORRELACIONES
#========================

cu <- c(
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
  
  "Carga_academica_real"
)

matriz_corr_pearson<-cor(datos_modelo[,cu], use="complete.obs", method="pearson")

round(matrixcu, 2)


#========================
# CORRPLOT
#========================

library(corrplot)

corrplot(
  matrixcu,
  
  method = "color",
  
  addCoef.col = "black",
  
  tl.cex = 0.7,
  number.cex = 0.6
)


