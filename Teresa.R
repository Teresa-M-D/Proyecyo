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
                       "PIB") #si no hago esto me convierte las categoricas en numericas poniendo un numero a cada opcion en orden alfabético

describe(datos_recodificados[,variables_numéricas])


#***********************
#Aprobadas
#***********************
#1SEM
#tiene q ver las q apruebas con a cuantas te presentas?
datos_recodificados$Curricular.units.1st.sem..approved.
descriptive(datos_recodificados$Curricular.units.1st.sem..approved.)
boxplot(datos_recodificados$Curricular.units.1st.sem..approved.~ datos_recodificados$Course, las=2)
#otra vez salen los 0 raros de multimedia
#vamos a ver si dpendn del año (aunq me paree raro q puedan aprobar)
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
#aproxmamos a normal
qqnorm(
  datos_recodificados$Curricular.units.1st.sem..approved.,
  main = "Papel probabilístico normal Aprobados 1º sem.",
)
qqline(datos_recodificados$Curricular.units.1st.sem..approved.)
grid()

#2SEM
descriptive(datos_recodificados$Curricular.units.2nd.sem..approved.)
boxplot(datos_recodificados$Curricular.units.1st.sem..approved.~ datos_recodificados$Course, las=2, cex.axis=0.6)

