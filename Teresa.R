#espacio para mí:)

descriptive(datos_modelo$Application.order)
datos_moda_condicionada<- read.csv("estudiantes.csv", header = TRUE, sep = ";")
#Chicas añado el cambio del nombre de variable de Nacionality a Nationality
datos_moda_condicionada$Nationality<-datos_moda_condicionada$Nacionality
datos_moda_condicionada$Nacionality<-NULL
datos_moda_condicionada["PIB"]=datos_moda_condicionada["GDP"]
datos_moda_condicionada$GDP<-NULL

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
descriptive(datos_modelo[,variables_numéricas])
