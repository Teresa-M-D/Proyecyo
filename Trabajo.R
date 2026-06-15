#·················
#MOTIVACIÓN:
#·················

#Diagrama de barras abandono de la educación en personas de 15 a 34 años en Europa (2024):

archivo <- "lfso_24eab01$defaultview_spreadsheet.xlsx"

datos <- read_excel(
  archivo,
  sheet = "Data",
  range = "A1058:D1092",
  col_names = FALSE
)

datos_limpios <- datos %>%
  select(
    pais = ...1,
    tasa_abandono = ...4
  ) %>%
  filter(
    !is.na(pais),
    !is.na(tasa_abandono)
  ) %>%
  mutate(
    tasa_abandono = as.character(tasa_abandono),
    tasa_abandono = trimws(tasa_abandono),
    tasa_abandono = na_if(tasa_abandono, ":"),
    tasa_abandono = gsub(",", ".", tasa_abandono),
    tasa_abandono = as.numeric(tasa_abandono)
  ) %>%
  filter(!is.na(tasa_abandono))

datos_ue_portugal <- datos_limpios %>%
  filter(
    pais %in% c(
      "European Union - 27 countries (from 2020)",
      "Portugal"
    )
  ) %>%
  mutate(
    pais = recode(
      pais,
      "European Union - 27 countries (from 2020)" = "Unión Europea"
    ),
    pais = factor(pais, levels = c("Portugal", "Unión Europea")),
    etiqueta = paste0(round(tasa_abandono, 1), "%")
  )

ggplot(datos_ue_portugal, aes(x = pais, y = tasa_abandono, fill = pais)) +
  geom_col(
    width = 0.35,
    color = "black",
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = etiqueta),
    vjust = -0.45,
    size = 8,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Portugal" = "#C8102E",
      "Unión Europea" = "#003399"
    )
  ) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(
    title = "Tasa de abandono de educación o formación formal",
    subtitle = "Personas de 15 a 34 años, 2024",
    x = NULL,
    y = "Porcentaje (%)",
    caption = "Fuente: Eurostat, lfso_24eab01"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(
      size = 25,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 19,
      hjust = 0.5,
      margin = margin(b = 22)
    ),
    axis.title.y = element_text(
      size = 20,
      face = "bold",
      margin = margin(r = 12)
    ),
    axis.text.x = element_text(
      size = 19,
      face = "bold",
      margin = margin(t = 8)
    ),
    axis.text.y = element_text(
      size = 17
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(
      color = "gray80",
      linewidth = 0.45
    ),
    plot.caption = element_text(
      size = 13,
      hjust = 1,
      margin = margin(t = 18)
    ),
    plot.margin = margin(20, 25, 20, 25)
  )

#Mapa de Europa interactivo sobre abandono de la educación en personas de 15 a 34 años en Europa (2024):

#Leer base de datos Eurostat
archivo <- "lfso_24eab01$defaultview_spreadsheet.xlsx"

datos <- read_excel(
  archivo,
  sheet = "Data",
  range = "A1058:D1092",
  col_names = FALSE
)

#Limpiar datos
datos_limpios <- datos %>%
  select(
    pais = ...1,
    tasa_abandono = ...4
  ) %>%
  filter(!is.na(pais), !is.na(tasa_abandono)) %>%
  mutate(
    tasa_abandono = suppressWarnings(as.numeric(tasa_abandono))
  ) %>%
  filter(!is.na(tasa_abandono)) %>%
  filter(!grepl("Euro area", pais))


#Tabla de equivalencias a ISO-3
#(necesario para plotly)

equivalencias_iso <- data.frame(
  pais = c(
    "European Union - 27 countries (from 2020)",
    "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany",
    "Estonia", "Ireland", "Greece", "Spain", "France",
    "Croatia", "Italy", "Cyprus", "Latvia", "Lithuania",
    "Luxembourg", "Hungary", "Malta", "Netherlands",
    "Austria", "Poland", "Portugal", "Romania", "Slovenia",
    "Slovakia", "Finland", "Sweden", "Norway", "North Macedonia"
  ),
  pais_es = c(
    "Unión Europea",
    "Bélgica", "Bulgaria", "Chequia", "Dinamarca", "Alemania",
    "Estonia", "Irlanda", "Grecia", "España", "Francia",
    "Croacia", "Italia", "Chipre", "Letonia", "Lituania",
    "Luxemburgo", "Hungría", "Malta", "Países Bajos",
    "Austria", "Polonia", "Portugal", "Rumanía", "Eslovenia",
    "Eslovaquia", "Finlandia", "Suecia", "Noruega", "Macedonia del Norte"
  ),
  iso3 = c(
    "EUU",
    "BEL", "BGR", "CZE", "DNK", "DEU",
    "EST", "IRL", "GRC", "ESP", "FRA",
    "HRV", "ITA", "CYP", "LVA", "LTU",
    "LUX", "HUN", "MLT", "NLD",
    "AUT", "POL", "PRT", "ROU", "SVN",
    "SVK", "FIN", "SWE", "NOR", "MKD"
  )
)


# Unir datos con ISO

datos_mapa <- datos_limpios %>%
  left_join(equivalencias_iso, by = "pais") %>%
  filter(!is.na(iso3)) %>%
  filter(iso3 != "EUU") %>%
  mutate(
    texto = paste0(
      "<b>", pais_es, "</b>",
      "<br>Tasa de abandono: ", tasa_abandono, "%"
    )
  )

# Crear texto para tooltip

datos_mapa <- datos_mapa %>%
  mutate(
    texto = paste0(
      "<b>", pais, "</b>",
      "<br>Tasa de abandono: ", tasa_abandono, "%"
    )
  )


#  Crear mapa interactivo

fig <- plotly::plot_ly(
  data = datos_mapa,
  type = "choropleth",
  locations = ~iso3,
  z = ~tasa_abandono,
  text = ~texto,
  hovertemplate = "%{text}<extra></extra>",
  colorscale = list(
    c(0.0, "#fee5d9"),
    c(0.2, "#fcbba1"),
    c(0.4, "#fc9272"),
    c(0.6, "#fb6a4a"),
    c(0.8, "#de2d26"),
    c(1.0, "#a50f15")
  ),
  marker = list(
    line = list(color = "white", width = 0.5)
  ),
  colorbar = list(
    title = "Tasa (%)"
  )
)


fig <- fig %>%
  layout(
    title = list(
      text = "Tasa de abandono de educación o formación formal en Europa<br><sup>Personas de 15 a 34 años, 2024</sup>"
    ),
    geo = list(
      scope = "europe",
      projection = list(type = "mercator"),
      showland = TRUE,
      landcolor = "rgb(245,245,245)",
      showcountries = TRUE,
      countrycolor = "white",
      showcoastlines = FALSE,
      showframe = FALSE,
      lataxis = list(range = c(34, 72)),
      lonaxis = list(range = c(-12, 35))
    )
  )

fig

#Diagrama de barras univariante variable objetivo Target:

datos_target <- datos_modelo %>%
  mutate(
    Target = recode(
      Target,
      "Dropout" = "Abandono",
      "Graduate" = "Graduado",
      "Enrolled" = "Matriculado"
    )
  ) %>%
  count(Target) %>%
  mutate(
    porcentaje = n / sum(n) * 100,
    etiqueta = paste0(round(porcentaje, 1), "%"),
    Target = reorder(Target, -n)
  )

#Gráfico de barras ordenado

ggplot(datos_target, aes(x = Target, y = n, fill = Target)) +
  geom_col(
    width = 0.65,
    color = "black",
    linewidth = 0.8
  ) +
  geom_text(
    aes(label = etiqueta),
    vjust = -0.4,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Abandono" = "#C0392B",
      "Graduado" = "#2E8B57",
      "Matriculado" = "#D9B56D"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Distribución de la variable Target",
    subtitle = "Porcentaje de estudiantes según su situación académica final",
    x = "Categoría Target",
    y = "Número de estudiantes"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      size = 22,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 13,
      hjust = 0.5,
      margin = margin(b = 15)
    ),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

#······················
#Perfil del estudiante:
#······················

#Orden de solicitud
datos_modelo$Application.order[datos_modelo$Application.order==0]=1
tabla<-prop.table(table(datos_modelo$Application.order))*100

df_orden<-data.frame(Orden = c("1ª opción",
            "2ª opción",
            "3ª opción",
            "4ª opción",
            "5ª opción",
            "6ª opción",
            "Última opción"),
  Porcentaje = as.numeric(tabla))

ggplot(df_orden,
       aes(x = Orden,
           y = Porcentaje)) +
  geom_col(fill = "#8B1E2D") +
  geom_text(aes(label = sprintf("%.2f%%", Porcentaje)),
            vjust = -0.3,
            size = 4) +
  labs(
    title = "Distribución del alumnado según orden de solicitud",
    subtitle = "Variable original tras agrupar el valor 0 con primera opción",
    x = "Orden de solicitud",
    y = "Porcentaje de estudiantes"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )


#Diagrama de cajas: edad de matriculación
ggplot(datos_modelo, aes(x = Age.at.enrollment)) +
  geom_boxplot(
    fill = "#8FA6C8",
    color = "#2F3A4A",
    width = 0.35,
    linewidth = 0.8,
    outlier.color = "#4A4A4A",
    outlier.fill = "#D9D9D9",
    outlier.shape = 21,
    outlier.size = 2.5,
    outlier.stroke = 0.7
  ) +
  scale_x_continuous(
    breaks = seq(15, 70, by = 5)
  ) +
  labs(
    title = "Distribución de la edad de matriculación",
    subtitle = "Edad de los estudiantes en el momento de su ingreso",
    x = "Edad al matricularse",
    y = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = 13,
      hjust = 0.5,
      color = "gray35",
      margin = margin(b = 18)
    ),
    axis.title.x = element_text(
      size = 14,
      face = "bold",
      margin = margin(t = 10)
    ),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      color = "gray85",
      linewidth = 0.4
    ),
    plot.margin = margin(15, 20, 15, 20),
    aspect.ratio=0.25
  )

#Boxplot edad de matriculación según abandono
boxplot(Age.at.enrollment ~ Target_bin,
        data = con_actividad_total,
        col = c("salmon", "lightgreen"),
        names = c("Abandono", "No Abandono"),
        main = "Edad de matriculación y abandono",
        xlab = "Target_bin",
        ylab = "Age.at.enrollment")




#Diagrama de barras género con proporciones:
datos_modelo$Gender <- factor(
  datos_modelo$Gender,
  levels = c("Femenino", "Masculino")
)

resumen_genero <- datos_modelo %>%
  count(Gender) %>%
  mutate(
    porcentaje = n / sum(n),
    porcentaje_label = scales::percent(porcentaje, accuracy = 0.1)
  )

resumen_genero

ggplot(resumen_genero, aes(x = Gender, y = porcentaje, fill = Gender)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_text(
    aes(label = porcentaje_label),
    vjust = -0.4,
    size = 4
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1)
  ) +
  scale_fill_manual(
    values = c(
      "Femenino" = "#E78AC3",
      "Masculino" = "#8DA0CB"
    )
  ) +
  labs(
    title = "Distribución del alumnado según género",
    x = "Género",
    y = "Porcentaje de estudiantes"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),legend.position = "none"
  )




# Crear tabla resumen con proporciones dentro de cada género
resumen_genero_target <- datos_modelo %>%
  count(Gender, Target_bin) %>%
  group_by(Gender) %>%
  mutate(
    prop = n / sum(n),
    etiqueta = percent(prop, accuracy = 0.1)
  ) %>%
  ungroup()

resumen_genero_target

# Gráfico de proporciones


resumen_genero_target <- datos_modelo %>%
  count(Gender, Target_bin) %>%
  group_by(Gender) %>%
  mutate(
    prop = n / sum(n),
    etiqueta = percent(prop, accuracy = 0.1)
  ) %>%
  ungroup()

resumen_genero_target


ggplot(
  resumen_genero_target,
  aes(x = Gender, y = prop, fill = Target_bin)
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = etiqueta),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "black"
  ) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c(
      "Abandono" = "indianred2",
      "No Abandono" = "darkseagreen3"
    )
  ) +
  labs(
    title = "Abandono según género",
    subtitle = "Proporción de abandono y no abandono dentro de cada género",
    x = "Género",
    y = "Proporción de estudiantes",
    fill = "Situación final"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )


#Nuevas variables:

#Carga académica real
ggplot(datos_modelo, aes(x = Carga_academica_real)) +
  geom_histogram(
    aes(fill = Carga_academica_real %in% c(5, 6)),
    binwidth = 1,
    color = "grey40"
  ) +
  scale_fill_manual(
    values = c("FALSE" = "#dfe8e6",
               "TRUE" = "#ff7f50"),
    guide = "none"
  ) +
  labs(
    x = "Carga académica real",
    y = "Frecuencia"
  ) +
  theme_bw()

prop.table(table(datos_modelo$Carga_academica_real))
prop.table(table(datos_modelo$Carga_academica_real_sem_2))
(0.2669651272+0.4429783223+0.2841658812+0.4415645617)/2

#Porcentaje de aprobados
#No incluímos los valores cero de multimedia. Nos fijamos en los NAs
descriptive(datos_recodificados$Porcentaje_aprobado_sem_1[datos_recodificados$Course!="Diseño de Animación y Multimedia"])
descriptive(datos_recodificados$Porcentaje_aprobado_sem_2[datos_recodificados$Course!="Diseño de Animación y Multimedia"])


#-------------------
#Abandono temprano
#-------------------
summary(datos_modelo$Porcentaje_aprobado_sem_1) #Hay 169 Na´s en porcentaje aprobados 1º semestre
summary(datos_modelo$Porcentaje_aprobado_sem_2) #Hay 221 Na´s en porcentaje aprobados 2º semestre

#Los Na's se deben ha que hay valores 0/0 que R convierte en Na's. De hecho si vemos el número de estudiantes con 0 unidades curriculares aprobadas 
#y cero evaluaciones los números coinciden

sum(datos_modelo$Curricular.units.1st.sem..approved. == 0 &
      datos_modelo$Curricular.units.1st.sem..evaluations. == 0) #Hay 169 estudiantes con cero unidades curriculares aprobadas y cero evaluaciones

sum(datos_modelo$Curricular.units.2nd.sem..approved. == 0 &
      datos_modelo$Curricular.units.2nd.sem..evaluations. == 0) #Hay 221 estudiantes con cero unidades curriculares aprobadas y cero evaluaciones

#Proporcion de abandono dentro del grupo que tiene cero unidades curriculares aprobadas y cero evaluaciones

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
sin_actividad_total <- subset(datos_modelo, tipo_actividad == "Sin actividad en todo el año")
no_presentados_1    <- subset(datos_modelo, tipo_actividad == "Sin actividad en 1º semestre")
no_presentados_2    <- subset(datos_modelo, tipo_actividad == "Sin actividad en 2º semestre")

table(con_actividad_total$Target_bin)
table(sin_actividad_total$Target_bin)
table(no_presentados_1$Target_bin)
table(no_presentados_2$Target_bin)

# Calcular proporciones
datos_prop <- datos_modelo %>%
  group_by(tipo_actividad, Target_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(tipo_actividad) %>%
  mutate(prop = n / sum(n),
         etiqueta = scales::percent(prop, accuracy = 0.1))

# Gráfico
ggplot(datos_prop,
       aes(x = tipo_actividad,
           y = prop,
           fill = Target_bin)) +
  geom_bar(stat = "identity") +
  
  # Etiquetas dentro de la barra
  geom_text(aes(label = etiqueta),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 4) +
  
  scale_fill_manual(values = c(
    "Abandono" = "indianred2",
    "No Abandono" = "lightgreen"
  )) +
  
  scale_y_continuous(labels = scales::percent) +
  
  labs(title = "Proporción de abandono según tipo de actividad académica",
       x = "Tipo de actividad",
       y = "Proporción") +
  
  theme_minimal()




#Análisis univariante numérico:



#Análisis bivariante: variables numéricas vs Target

#NOTAS POR SEMESTRE SEGÚN ABANDONO (voy a hacerlo excluyendo a las personas que no tienen actividad en alguno de 
#los dos semestres o en ambos)

# Convertir a formato largo
df_long1 <- datos_modelo %>%
  filter(tipo_actividad == "Con actividad") %>%
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

df_long2 <- datos_modelo %>%
  filter(tipo_actividad == "Con actividad") %>%   
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

#Test de Mann - Whitney
datos_activos <- datos_modelo %>% 
  filter(tipo_actividad == "Con actividad")

wilcox.test(Curricular.units.1st.sem.grade_10 ~ Target_bin, data = datos_activos)
wilcox.test(Curricular.units.2nd.sem.grade_10 ~ Target_bin, data = datos_activos)
wilcox.test(Porcentaje_aprobado_sem_1 ~ Target_bin, data = datos_activos)
wilcox.test(Porcentaje_aprobado_sem_2  ~ Target_bin, data = datos_activos)

#Tamaño del efecto
wilcox_effsize(Curricular.units.1st.sem.grade_10 ~ Target_bin, data = datos_activos)
wilcox_effsize(Curricular.units.2nd.sem.grade_10 ~ Target_bin, data = datos_activos)
wilcox_effsize(Porcentaje_aprobado_sem_1 ~ Target_bin, data = datos_activos)
wilcox_effsize(Porcentaje_aprobado_sem_2 ~ Target_bin, data = datos_activos)


#Análisis bivariante: variabels categóricas vs Target


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

datos_modelo <- datos_modelo %>%
  mutate(
    Gender_label = case_when(
      Gender == 0 ~ "Femenino",
      Gender == 1 ~ "Masculino",
      TRUE ~ NA_character_
    )
  )


#Tipo de carrera:

tabla_course_target <- table(
  datos_modelo$Course_group,
  datos_modelo$Target_bin
)

tabla_course_target

prop.table(tabla_course_target, 1)
prop.table(tabla_course_target, 2)

cramersV(tabla_course_target)
GK_assoc(datos_modelo$Course_group, datos_modelo$Target_bin)
GK_assoc(datos_modelo$Target_bin, datos_modelo$Course_group)

chisq.test(tabla_course_target, correct = FALSE)
chisq.test(tabla_course_target)$expected

# Nombres más cortos para el gráfico
datos_modelo$Course_group_short <- dplyr::recode(
  datos_modelo$Course_group,
  "Ingeniería/Tech" = "Ing/Tec",
  "Educación/Social" = "Edu/Soc",
  "Comunicación" = "Com.",
  "Agro/Animal" = "Agr/An",
  "Empresa" = "Emp",
  "Salud" = "Salud"
)

datos_modelo$Target_bin <- factor(
  datos_modelo$Target_bin,
  levels = c("No Abandono", "Abandono")
)
mosaic(
  ~ Course_group_short + Target_bin,
  data = datos_modelo,
  shade = TRUE,
  legend = TRUE,
  cex.axis = 0.8
)


#Orden de solicitud:
tabla_order_target <- table(
  datos_modelo$Application.order_group,
  datos_modelo$Target_bin
)

tabla_order_target
cramersV(tabla_order_target)
# Proporciones dentro de cada orden de solicitud
prop.table(tabla_order_target, margin = 1)

# Chi-cuadrado
chisq.test(tabla_order_target)

# Frecuencias esperadas
chisq.test(tabla_order_target)$expected


#grafico mosaico:
mosaic(~ Application.order_group + Target_bin, data = datos_modelo, 
       shade = TRUE, legend = TRUE, cex.axis = 0.7)


#Tasas al día:
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

datos_modelo$Tuition.fees.up.to.date <- factor(datos_modelo$Tuition.fees.up.to.date, levels=c("No", "Sí"))

datos_modelo$Target_bin <- factor(
  datos_modelo$Target_bin,
  levels = c("No Abandono", "Abandono")
)

#Gráficos:
mosaic(
  ~ Tuition.fees.up.to.date + Target_bin,
  data = datos_modelo,
  shade = TRUE,
  legend = TRUE,
  cex.axis = 0.8,
  labeling_args = list(
    set_varnames = c(
      Tuition.fees.up.to.date = "Matrícula al día",
      Target_bin = "Abandono"
    )
  )
)


#REGRESIÓN LOGÍSTICA BINARIA:

# Seleccionamos solo los estudiantes con actividad durante todo el curso
vars_modelo <- c(
  "Target_bin",
  "Carga_academica_real_sem_2",
  "Porcentaje_aprobado_sem_2",
  "Curricular.units.2nd.sem.grade_10",
  "Tuition.fees.up.to.date",
  "Scholarship.holder",
  "Age.at.enrollment",
  "Gender",
  "Course_group",
  "Application.mode_group",
  "tipo_actividad"
)

nrow(con_actividad_total) #comprobamos que son 4018
con_actividad_total <- datos_modelo %>%
  filter(tipo_actividad == "Con actividad") %>%
  select(all_of(vars_modelo)) %>%
  filter(complete.cases(.))

# Selección de categorías de referencia:
con_actividad_total$Target_bin <- factor(
  con_actividad_total$Target_bin,
  levels = c("No Abandono", "Abandono")
)

con_actividad_total$Tuition.fees.up.to.date <- relevel(
  as.factor(con_actividad_total$Tuition.fees.up.to.date),
  ref = "No"
)

con_actividad_total$Scholarship.holder <- relevel(
  as.factor(con_actividad_total$Scholarship.holder),
  ref = "No"
)

con_actividad_total$Gender <- relevel(
  as.factor(con_actividad_total$Gender),
  ref = "Femenino"
)

con_actividad_total$Application.mode_group <- relevel(
  as.factor(con_actividad_total$Application.mode_group),
  ref = "Acceso normal"
)

con_actividad_total$Course_group <- relevel(
  as.factor(con_actividad_total$Course_group),
  ref = "Empresa"
)



# MODELO COMPLETO
modelo_combinado <- glm(
  Target_bin ~ Carga_academica_real_sem_2 +
    Porcentaje_aprobado_sem_2 +
    Curricular.units.2nd.sem.grade_10 +
    Tuition.fees.up.to.date +
    Scholarship.holder +
    Age.at.enrollment +
    Gender +
    Course_group +
    Application.mode_group,
  data = con_actividad_total,
  family = binomial
)

summary(modelo_combinado)

# MULTICOLINEALIDAD
vif(modelo_combinado)


# ODDS RATIOS
OR <- exp(coef(modelo_combinado))
IC_OR <- exp(confint.default(modelo_combinado))

resultado_OR <- data.frame(
  Variable = names(OR),
  OR = as.numeric(OR),
  IC_inf = IC_OR[, 1],
  IC_sup = IC_OR[, 2]
)

resultado_OR

# GRÁFICO ODDS RATIOS
resultado_graf <- resultado_OR %>%
  filter(
    Variable != "(Intercept)",
    OR > 0,
    IC_inf > 0,
    IC_sup > 0
  )

ggplot(resultado_graf, aes(x = reorder(Variable, OR), y = OR)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "Odds Ratios del modelo logístico",
    subtitle = "Variable respuesta: Abandono",
    x = "Variable",
    y = "Odds Ratio, escala logarítmica"
  ) +
  theme_minimal()

# MODELO NULO
modelo_nulo <- glm(
  Target_bin ~ 1,
  data = con_actividad_total,
  family = binomial
)

anova(modelo_nulo, modelo_combinado, test = "Chisq")

# PSEUDO R CUADRADO
pseudo_R2 <- 1 - modelo_combinado$deviance / modelo_combinado$null.deviance
pseudo_R2


# PROBABILIDADES MODELO COMPLETO
con_actividad_total$prob_pred <- predict(
  modelo_combinado,
  newdata = con_actividad_total,
  type = "response"
)

summary(con_actividad_total$prob_pred)


# ROC MODELO COMPLETO
roc_modelo <- roc(
  response = con_actividad_total$Target_bin,
  predictor = con_actividad_total$prob_pred,
  levels = c("No Abandono", "Abandono"),
  direction = "<"
)

auc_modelo <- auc(roc_modelo)
auc_modelo

# CLASIFICACIÓN UMBRAL 0.5
con_actividad_total$pred_clase_05 <- ifelse(
  con_actividad_total$prob_pred >= 0.5,
  "Abandono",
  "No Abandono"
)

con_actividad_total$pred_clase_05 <- factor(
  con_actividad_total$pred_clase_05,
  levels = levels(con_actividad_total$Target_bin)
)

tabla_clasificacion_05 <- table(
  Real = con_actividad_total$Target_bin,
  Predicho = con_actividad_total$pred_clase_05
)

tabla_clasificacion_05

# MÉTRICAS UMBRAL 0.5
precision_global_05 <- mean(
  con_actividad_total$pred_clase_05 == con_actividad_total$Target_bin
)

sensibilidad_05 <- tabla_clasificacion_05["Abandono", "Abandono"] /
  sum(tabla_clasificacion_05["Abandono", ])

especificidad_05 <- tabla_clasificacion_05["No Abandono", "No Abandono"] /
  sum(tabla_clasificacion_05["No Abandono", ])

precision_global_05
sensibilidad_05
especificidad_05


# UMBRAL YOUDEN
punto_youden <- coords(
  roc_modelo,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity", "accuracy"),
  transpose = FALSE
)

punto_youden

umbral_youden <- as.numeric(punto_youden$threshold[1])
umbral_youden


# CLASIFICACIÓN MODELO COMPLETO
con_actividad_total$pred_clase_youden <- ifelse(
  con_actividad_total$prob_pred >= umbral_youden,
  "Abandono",
  "No Abandono"
)

con_actividad_total$pred_clase_youden <- factor(
  con_actividad_total$pred_clase_youden,
  levels = levels(con_actividad_total$Target_bin)
)

tabla_clasificacion_youden <- table(
  Real = con_actividad_total$Target_bin,
  Predicho = con_actividad_total$pred_clase_youden
)

tabla_clasificacion_youden


# MÉTRICAS MODELO COMPLETO
precision_global_youden <- mean(
  con_actividad_total$pred_clase_youden == con_actividad_total$Target_bin
)

sensibilidad_youden <- tabla_clasificacion_youden["Abandono", "Abandono"] /
  sum(tabla_clasificacion_youden["Abandono", ])

especificidad_youden <- tabla_clasificacion_youden["No Abandono", "No Abandono"] /
  sum(tabla_clasificacion_youden["No Abandono", ])

precision_global_youden
sensibilidad_youden
especificidad_youden


# GRÁFICO CLASIFICACIÓN MODELO COMPLETO
tabla_df_youden <- as.data.frame(tabla_clasificacion_youden)

ggplot(tabla_df_youden, aes(x = Real, y = Freq, fill = Predicho)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(
      "No Abandono" = "darkseagreen3",
      "Abandono" = "indianred2"
    )
  ) +
  labs(
    title = "Clasificación real vs predicha",
    subtitle = paste("Umbral de Youden =", round(umbral_youden, 3)),
    x = "Situación real",
    y = "Número de estudiantes",
    fill = "Clase predicha"
  ) +
  theme_minimal()


# GRÁFICO CLASIFICACIÓN MODELO COMPLETO EN PORCENTAJES
tabla_df_youden_pct <- tabla_df_youden %>%
  group_by(Real) %>%
  mutate(
    porcentaje = Freq / sum(Freq),
    etiqueta = percent(porcentaje, accuracy = 0.1)
  )

ggplot(tabla_df_youden_pct, aes(x = Real, y = Freq, fill = Predicho)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = etiqueta),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 4
  ) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c(
      "No Abandono" = "darkseagreen3",
      "Abandono" = "indianred2"
    )
  ) +
  labs(
    title = "Clasificación real vs predicha",
    subtitle = paste("Porcentajes dentro de cada clase real, umbral =", round(umbral_youden, 3)),
    x = "Situación real",
    y = "Porcentaje",
    fill = "Clase predicha"
  ) +
  theme_minimal()


# HISTOGRAMA PROBABILIDADES MODELO COMPLETO
ggplot(con_actividad_total, aes(x = prob_pred, fill = Target_bin)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  geom_vline(xintercept = umbral_youden, linetype = "dashed", linewidth = 1) +
  scale_fill_manual(
    values = c(
      "No Abandono" = "darkseagreen3",
      "Abandono" = "indianred2"
    )
  ) +
  labs(
    title = "Distribución de probabilidades predichas",
    subtitle = paste("Línea discontinua: umbral de Youden =", round(umbral_youden, 3)),
    x = "Probabilidad predicha de abandono",
    y = "Frecuencia",
    fill = "Situación real"
  ) +
  theme_minimal()


# CURVA ROC MODELO COMPLETO
roc_df <- coords(
  roc_modelo,
  x = "all",
  ret = c("specificity", "sensitivity"),
  transpose = FALSE
)

roc_df <- roc_df %>%
  mutate(fpr = 1 - specificity) %>%
  arrange(fpr)

ggplot(roc_df, aes(x = fpr, y = sensitivity)) +
  geom_line(linewidth = 1) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "gray50"
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = paste("Curva ROC del modelo logístico - AUC =", round(auc_modelo, 3)),
    x = "1 - Especificidad",
    y = "Sensibilidad"
  ) +
  theme_minimal()

# VALIDACIÓN CRUZADA 10 PARTICIONES


set.seed(123)

vars_modelo <- c(
  "Target_bin",
  "Carga_academica_real_sem_2",
  "Porcentaje_aprobado_sem_2",
  "Curricular.units.2nd.sem.grade_10",
  "Tuition.fees.up.to.date",
  "Scholarship.holder",
  "Age.at.enrollment",
  "Gender",
  "Course_group",
  "Application.mode_group"
)

datos_cv <- con_actividad_total[, vars_modelo]
datos_cv <- datos_cv[complete.cases(datos_cv), ]

# VARIABLE RESPUESTA PARA CARET
datos_cv$Target_bin <- factor(
  datos_cv$Target_bin,
  levels = c("Abandono", "No Abandono"),
  labels = c("Abandono", "No_Abandono")
)

levels(datos_cv$Target_bin)

# CONTROL DE VALIDACIÓN CRUZADA
control_cv <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = TRUE
)

# MODELO CON VALIDACIÓN CRUZADA
modelo_cv <- train(
  Target_bin ~ Carga_academica_real_sem_2 +
    Porcentaje_aprobado_sem_2 +
    Curricular.units.2nd.sem.grade_10 +
    Tuition.fees.up.to.date +
    Scholarship.holder +
    Age.at.enrollment +
    Gender +
    Course_group +
    Application.mode_group,
  data = datos_cv,
  method = "glm",
  family = binomial,
  metric = "ROC",
  trControl = control_cv
)

modelo_cv

# PREDICCIONES DE LA VALIDACIÓN CRUZADA
pred_cv <- modelo_cv$pred

head(pred_cv)

# CLASIFICACIÓN CON UMBRAL DE YOUDEN
pred_cv$pred_youden <- ifelse(
  pred_cv$Abandono >= umbral_youden,
  "Abandono",
  "No_Abandono"
)

pred_cv$pred_youden <- factor(
  pred_cv$pred_youden,
  levels = levels(pred_cv$obs)
)

# MATRIZ DE CLASIFICACIÓN
tabla_cv_youden <- table(
  Real = pred_cv$obs,
  Predicho = pred_cv$pred_youden
)

tabla_cv_youden

# MÉTRICAS
accuracy_cv_youden <- mean(
  pred_cv$pred_youden == pred_cv$obs
)

sensibilidad_cv_youden <- tabla_cv_youden["Abandono", "Abandono"] /
  sum(tabla_cv_youden["Abandono", ])

especificidad_cv_youden <- tabla_cv_youden["No_Abandono", "No_Abandono"] /
  sum(tabla_cv_youden["No_Abandono", ])

accuracy_cv_youden
sensibilidad_cv_youden
especificidad_cv_youden

