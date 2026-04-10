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

#falta hacerlo
#ESTUDIO UNIVARIANTE NUMERICAS
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
datos_recodificados$Curricular.units.1st.sem..grade.
#Describe de cada una por separado
comparacion_semestres_y_nota_entrada<-c("Previous.qualification.grade_10",
                                        "Admission.grade_10",
                                        "Curricular.units.1st.sem.grade_10",
                                        "Curricular.units.2nd.sem.grade_10")
describe(datos_recodificados[,"Curricular.units.1st.sem.grade_10"])
describe(datos_recodificados[,"Curricular.units.2nd.sem.grade_10"])
describe(datos_recodificados[,"Previous.qualification.grade_10"])
describe(datos_recodificados[,"Admission.grade_10"])
descriptive(datos_recodificados$Previous.qualification.grade_10)
descriptive(datos_recodificados[,comparacion_semestres_y_nota_entrada])
""
#Boxplots
boxplot(datos_recodificados$Previous.qualification.grade_10,
        yaxt = "n",
        ylab = "Nota",
        main = "Nota estudios previos")
axis(2, at = seq(4, 10, by = 0.5))
boxplot(datos_recodificados$Age.at.enrollment)
boxplot(datos_recodificados$Admission.grade_10)
boxplot(datos_recodificados$Application.order)
boxplot(datos_recodificados$Curricular.units.1st.sem..credited.)
boxplot(datos_recodificados$Curricular.units.1st.sem..enrolled., yaxt="n")
axis(2, at=seq(0,25, by=1))
boxplot(datos_recodificados$Curricular.units.1st.sem..evaluations., yaxt="n")
axis(2, at=seq(0,45, by=3))
boxplot(datos_recodificados$Curricular.units.1st.sem..approved., yaxt="n")
axis(2, at=seq(0,25, by=1))
boxplot(datos_recodificados$Curricular.units.1st.sem.grade_10, yaxt="n")
axis(2, at=seq(0,10, by=1))


boxplot(datos_recodificados$Curricular.units.1st.sem..without.evaluations.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..credited.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..enrolled.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..evaluations.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..approved.)
boxplot(datos_recodificados$Curricular.units.2nd.sem.grade_10)
boxplot(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.)
boxplot(datos_recodificados$Unemployment.rate)
boxplot(datos_recodificados$Inflation.rate)
boxplot(datos_recodificados$PIB)


#Estudio de las variables dew notas de 1 y 2 sem
#1SEM
#histograma
hist(datos_recodificados$Curricular.units.1st.sem.grade_10, breaks=seq(0, 10, by=0.5), xaxt="n")
axis(1, at=seq(0,10, by=0.5))

#media de solo los alumnos q se han presentado
median(datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0])
mean(datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0])

#2 SEM
hist(datos_recodificados$Curricular.units.2nd.sem.grade_10, breaks=seq(0, 10, by=0.5), xaxt="n")
axis(1, at=seq(0,10, by=0.5))

median(datos_recodificados$Curricular.units.2nd.sem.grade_10[datos_recodificados$Curricular.units.2nd.sem.grade_10!=0])
mean(datos_recodificados$Curricular.units.2nd.sem.grade_10[datos_recodificados$Curricular.units.2nd.sem.grade_10!=0])
