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


#EVALUCIACIONES
#1SEM
#descriptive general
descriptive(datos_recodificados$Curricular.units.2nd.sem..evaluations.)
#descriptivo por pib/año
tapply(datos_recodificados$Curricular.units.1st.sem..evaluations., datos_recodificados$PIB, descriptive) #sale lo http://127.0.0.1:44275/graphics/plot_zoom_png?width=1163&height=861del final pq descriptive es una funcion q cada vez q se eejcuta imprime aparte de return, entonces mientras se ejecuta me lo va enseñando, y al final me pone el vector del valor_PIB: resultado, para los tres casos: variable numerica(mi caso, evaluationes es numerica), categorica(no solo le hje dado una variable numerica asi q no sale nada) y fecha(lo mismo, null).
#boxplot por pib
boxplot(datos_recodificados$Curricular.units.1st.sem..evaluations.~datos_recodificados$PIB yaxt="n")
axis(2, at=seq(0, 45, by=5))
#curva densidad
plot(density(datos_recodificados$Curricular.units.1st.sem..evaluations.))
hist(datos_recodificados$Curricular.units.1st.sem..evaluations., breaks=seq(0, 46, by=1), xaxt="n")
axis(1, at=seq(2, 46, by=1))
describe(datos_recodificados$Curricular.units.1st.sem..evaluations.)

#calculo moda (no hay una fucnion)
names(sort(table(datos_recodificados$Curricular.units.1st.sem..evaluations.), decreasing = TRUE))[1]
x<-prop.table(table(datos_recodificados$Curricular.units.1st.sem..evaluations.))
barplot(x)      
#2SEM
#descriptive general
descriptive(datos_recodificados$Curricular.units.1st.sem..evaluations.)
#descriptivo por pib/año
tapply(datos_recodificados$Curricular.units.2nd.sem..evaluations., datos_recodificados$PIB, descriptive) #sale lo del final pq descriptive es una funcion q cada vez q se eejcuta imprime aparte de return, entonces mientras se ejecuta me lo va enseñando, y al final me pone el vector del valor_PIB: resultado, para los tres casos: variable numerica(mi caso, evaluationes es numerica), categorica(no solo le hje dado una variable numerica asi q no sale nada) y fecha(lo mismo, null).
#boxplot por pib
boxplot(datos_recodificados$Curricular.units.1st.sem..evaluations.~datos_recodificados$PIB yaxt="n")
axis(2, at=seq(0, 45, by=5))
#curva densidad
plot(density(datos_recodificados$Curricular.units.1st.sem..evaluations.))
hist(datos_recodificados$Curricular.units.1st.sem..evaluations., breaks=seq(0, 46, by=1), xaxt="n")
axis(1, at=seq(2, 46, by=1))
describe(datos_recodificados$Curricular.units.1st.sem..evaluations.)

#calculo moda (no hay una fucnion)
names(sort(table(datos_recodificados$Curricular.units.1st.sem..evaluations.), decreasing = TRUE))[1]
x<-prop.table(table(datos_recodificados$Curricular.units.1st.sem..evaluations.))
barplot(x)   

#ahora separo según las carreras
boxplot(datos_recodificados$Curricular.units.1st.sem..evaluations.~datos_recodificados$Course, las=2, cex.axis=0.6) #las=2 para poner verticales los nombres y así se ven, 
par(mar=c(13,4,4,2))  #para dejar más espacio abajo (el primer elemento del vector es el espacio de abajo)
#descriptivo  e histograma de evaluaciones en multimedia especificamente

descriptive(datos_recodificados$Curricular.units.2nd.sem..evaluations.[datos_recodificados$Course=="Diseño de Animación y Multimedia"])
unique(datos_recodificados$Course)

hist(datos_recodificados$Curricular.units.1st.sem..evaluations.[datos_recodificados$Course=="Diseño de Animación y Multimedia"])
#comparo carrera con dropout
tabla<-table(datos_recodificados$Course, datos_recodificados$Target)
mosaicplot(tabla, las=2, cex.axis = 0.7)#hay gente parecida en multimedia q en el resto, entonce no entiendo muy bien pq hay tan pocas evaluaciones

#descriptivo por carrera:
tapply(datos_recodificados$Curricular.units.1st.sem..evaluations., datos_recodificados$Course, descriptive)

