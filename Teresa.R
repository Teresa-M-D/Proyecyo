#espacio para mí:)
#COSAS BÁSICAS
head(datos)
descriptive(datos_recodificados)
#ver nombres variables
names(datos)
names(datos_recodificados)
datos_recodificados$Marital.status
matriculado=datos$Curricular.units.1st.sem..enrolled
max(matriculado)
boxplot(datos$Target)
datos$Curricular.units.1st.sem..approved.
datos$Curricular.units.1st.sem..without.evaluations.
datos$Curricular.units.1st.sem..evaluations.

#Veo si hay relación perfecta entre evaluations y without evaluations (si van a un examen deberia sumarse uno y no sumarse en la otra)
#
#falta hacerlo
#ESTUDIO UNIVARIANTE NUMERICAS

#Describe de todas
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
                       "Porcentaje_aprobado_sem_1",
                       "PIB",
                       "Carga_academica_real") #si no hago esto me convierte las categoricas en numericas poniendo un numero a cada opcion en orden alfabético

describe(datos_recodificados[,variables_numéricas])


#hacer matriz de correlaciones
matriz_corr_kendall<-cor(datos_modelo[,variables_numéricas], use="complete.obs", method="kendall") 
matriz_corr_pearson<-cor(datos_modelo[,variables_numéricas], use="complete.obs", method="pearson")
matriz_corr_spearman<-cor(datos_modelo[,variables_numéricas], use="complete.obs", method="spearman")
#para ponerlo gráficamente
corrplot(matriz_corr_kendall, method="color", type = "upper",
         tl.cex = 0.5,   addCoef.col = "black", number.cex=0.4, order = "hclust", main="Correlaciones de Kendall")
corrplot(matriz_corr_pearson, method="color", type = "upper",
         tl.cex = 0.5,   addCoef.col = "black", number.cex=0.4, order = "hclust", main="Correlaciones de Pearson")
corrplot(matriz_corr_spearman, method="color", type = "upper",
         tl.cex = 0.5,   addCoef.col = "black", number.cex=0.4, order = "hclust")


#*************************
#GRÁFICOS DE DISPERSIÓN
#*************************

#admission grade

plot(datos_modelo$Admission.grade_10~datos_modelo$Previous.qualification.grade_10, cex=1, xaxt="n")
axis(1, at=seq(0, 10, by=0.1))
datos_modelo$Previous.qualification.grade_10[datos_modelo$Previous.qualification.grade_10>=9]
  
#gráfico 3d
plot_ly(data=datos_modelo,
       x=~Admission.grade_10,
       y=~Previous.qualification.grade_10,
       z=~Age.at.enrollment,
       type="scatter3d", 
       mode="markers",
       marker=list(size=2))


plot_ly(
  data = datos_modelo,
  x = ~Admission.grade_10,
  y = ~carga_academica_real,
  color = ~Age.at.enrollment,
  type = "scatter",
  mode = "markers",
  marker = list(
    size = 4,
    symbol = "circle-open"
  )
)
cor(datos_modelo$Age.at.enrollment, datos_modelo$Application.order, method="spearman")
library(ggplot2)

# 1. Admission vs Previous
ggplot(datos_modelo, aes(x = Previous.qualification.grade_10,
                         y = Admission.grade_10)) +
  geom_point(alpha = 0.2, color = "blue")+
scale_x_continuous(breaks = seq(0, 9, 1))

# 2. Age vs Application order
ggplot(datos_modelo, aes(x = Application.order,
                         y = Age.at.enrollment)) +
  geom_point(alpha = 0.2, color = "darkgreen")+
scale_x_continuous(breaks = seq(0, 9, 1))

# 3. Age vs Carga académica real
ggplot(datos_modelo, aes(x = Carga_academica_real,
                         y = Age.at.enrollment)) +
  geom_point(alpha = 0.2, color = "purple")


# 4. Carga vs Application order
ggplot(datos_modelo, aes(x = Application.order,
                         y = Carga_academica_real)) +
  geom_point(alpha = 0.2, color = "orange2")+
scale_x_continuous(breaks = seq(0, 9, 1))+
scale_y_continuous(breaks = seq(0, 20, 2))

# 5. Carga vs Nota
ggplot(datos_modelo, aes(x = Carga_academica_real,
                         y = Curricular.units.1st.sem.grade_10)) +
  geom_point(alpha = 0.2, color = "blue")


# 6. Carga vs % aprobados
datos_modelo$porcentaje <- datos_modelo$Curricular.units.1st.sem..approved. /
  datos_modelo$Curricular.units.1st.sem..enrolled.

ggplot(datos_modelo, aes(x = Carga_academica_real,
                         y = porcentaje)) +
  geom_point(alpha = 0.2, color = "red")


# 7. Application order vs Credited
ggplot(datos_modelo, aes(x = Application.order,
                         y = Curricular.units.1st.sem..credited.)) +
  geom_point(alpha = 0.2)+
scale_x_continuous(breaks = seq(0, 9, 1))
  

# 8. Credited vs Evaluations
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem..credited.,
                         y = Curricular.units.1st.sem..evaluations.)) +
  geom_point(alpha = 0.2)


# 9. Enrolled vs Evaluations
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem..enrolled.,
                         y = Curricular.units.1st.sem..evaluations.)) +
  geom_point(alpha = 0.2, color="brown4")

  #es porque no puede haber más asignaturas que evaluaciones?
datos_modelo$Target[datos_modelo$Curricular.units.1st.sem..enrolled.>datos_modelo$Curricular.units.1st.sem..evaluations.]
#10 Convalidadas y caega de trabajo
ggplot(datos_modelo, aes(x=Curricular.units.1st.sem..credited.,
                         y=Carga_academica_real))+
  geom_point(alpha=0.2)


# 11. Aprobadas vs Credited
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem..approved.,
                         y = Curricular.units.1st.sem..credited.)) +
  geom_point(alpha = 0.2, color="hotpink4")


# 12. Aprobadas vs Enrolled
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem..approved.,
                         y = Curricular.units.1st.sem..enrolled.)) +
  geom_point(alpha = 0.2)

#vamos a ver entonces si credited está relacionado con target
table(datos_modelo$Curricular.units.1st.sem..credited.,datos_modelo$Target)
kruskal.test(
  Curricular.units.1st.sem..credited. ~ Target,
  data = datos_modelo
)
# 13. Aprobadas vs Evaluations
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem..approved.,
                         y = Curricular.units.1st.sem..evaluations.)) +
  geom_point(alpha = 0.2, color="deepskyblue4")


# 14. Nota vs Evaluations
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem.grade_10,
                         y = Curricular.units.1st.sem..evaluations.)) +
  geom_point(alpha = 0.2)


# 15. Nota vs Credited
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem.grade_10,
                         y = Curricular.units.1st.sem..credited.)) +
  geom_point(alpha = 0.2)


# 16. Nota vs Enrolled
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem.grade_10,
                         y = Curricular.units.1st.sem..enrolled.)) +
  geom_point(alpha = 0.2)


# 17. Comparación semestres
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem.grade_10,
                         y = Curricular.units.2nd.sem.grade_10)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "red")


#COMPARACIÓN SEMESTRES
library(ggplot2)

# 1. Nota 1º vs Nota 2º
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem.grade_10,
                         y = Curricular.units.2nd.sem.grade_10)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Nota 1º semestre",
       y = "Nota 2º semestre",
       title = "Comparación de notas")


# 2. Aprobadas 1º vs Aprobadas 2º
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem..approved.,
                         y = Curricular.units.2nd.sem..approved.)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Aprobadas 1º semestre",
       y = "Aprobadas 2º semestre",
       title = "Comparación de asignaturas aprobadas")


# 3. Matriculadas 1º vs Matriculadas 2º
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem..enrolled.,
                         y = Curricular.units.2nd.sem..enrolled.)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Matriculadas 1º semestre",
       y = "Matriculadas 2º semestre",
       title = "Comparación de asignaturas matriculadas")


# 4. Evaluaciones 1º vs Evaluaciones 2º
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem..evaluations.,
                         y = Curricular.units.2nd.sem..evaluations.)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Evaluaciones 1º semestre",
       y = "Evaluaciones 2º semestre",
       title = "Comparación de evaluaciones")


# 5. Convalidadas 1º vs Convalidadas 2º
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem..credited.,
                         y = Curricular.units.2nd.sem..credited.)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Convalidadas 1º semestre",
       y = "Convalidadas 2º semestre",
       title = "Comparación de créditos convalidados")


# 6. Sin evaluar 1º vs Sin evaluar 2º
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem..without.evaluations.,
                         y = Curricular.units.2nd.sem..without.evaluations.)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Sin evaluar 1º semestre",
       y = "Sin evaluar 2º semestre",
       title = "Comparación de asignaturas sin evaluación")

# 7. Sin evaluar 1º vs evaluar
ggplot(datos_modelo, aes(x = Curricular.units.1st.sem..without.evaluations.,
                         y = Curricular.units.1st.sem..evaluations.)) +
  geom_point(alpha = 0.25) 
