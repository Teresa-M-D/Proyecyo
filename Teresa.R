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
matriz_corr_kendall<-cor(datos_recodificados[,variables_numéricas], use="complete.obs", method="kendall") 
matriz_corr_pearson<-cor(datos_recodificados[,variables_numéricas], use="complete.obs", method="pearson")
matriz_corr_spearman<-cor(datos_recodificados[,variables_numéricas], use="complete.obs", method="spearman")
#para ponerlo gráficamente
corrplot(matriz_corr_kendall, method="color", type = "upper",
         tl.cex = 0.5,   addCoef.col = "black", number.cex=0.4, order = "hclust")
corrplot(matriz_corr_pearson, method="color", type = "upper",
         tl.cex = 0.5,   addCoef.col = "black", number.cex=0.4, order = "hclust")
corrplot(matriz_corr_spearman, method="color", type = "upper",
         tl.cex = 0.5,   addCoef.col = "black", number.cex=0.4, order = "hclust")
#ver relación entre age y application order


#miramos si en multimedia hay alguno con without evaluationes ==0 en los años q no hay carrera
boxplot(datos_recodificados$Curricular.units.1st.sem..without.evaluations.[datos_recodificados$Curricular.units.1st.sem..without.evaluations.==0 and ]~datos_recodificados$PIB)
