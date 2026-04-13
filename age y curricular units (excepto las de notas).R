#****************
#AGE AT ENROLLMENT
#****************
datos_recodificados$Age.at.enrollment
descriptive(datos_recodificados$Age.at.enrollment)
hist(datos_recodificados$Age.at.enrollment)
boxplot(datos_recodificados$Age.at.enrollment, yaxt="n") 
axis(2, at=seq(15,70, by=5))
#ppe
x_exp <- datos_recodificados$Age.at.enrollment - 17 #le restamos 17 para q el mínimo sea 0
qqplot(
  qexp(ppoints(length(x_exp)), rate = 1/mean(x_exp)),
  sort(x_exp),
  main = "Papel probabilístico exponencial Age At Enrollment",
  xlab = "Cuantiles teóricos",
  ylab = "Cuantiles observados"
  
)

abline(0,1)
#ppgamma
x_gamma <- datos_recodificados$Age.at.enrollment - 17
x_gamma <- x_gamma[x_gamma > 0]

library(MASS)

ajuste <- fitdistr(x_gamma, "gamma")
ajuste

qqplot(
  qgamma(
    ppoints(length(x_gamma)),
    shape = ajuste$estimate["shape"],
    rate = ajuste$estimate["rate"]
  ),
  sort(x_gamma),
  main = "Papel probabilístico gamma Edad",
  xlab = "Cuantiles teóricos",
  ylab = "Cuantiles observados"
)

abline(0,1)

#ppn
qqnorm(
  datos_recodificados$Age.at.enrollment,
  main = "Papel probabilístico normal Edad ",
)
qqline(datos_recodificados$Age.at.enrollment)
grid()

#*********************************
# WITHOUT EVALUATIONS 1st SEM
#*********************************

descriptive(datos_recodificados$Curricular.units.1st.sem..without.evaluations.)
boxplot(datos_recodificados$Curricular.units.1st.sem..without.evaluations.)
hist(datos_recodificados$Curricular.units.1st.sem..without.evaluations., breaks=seq(0, 12, by=1), xaxt="n")
axis(1, at=seq(0,12, by=1))
#ver cuantos valores hay distintos de 0
descriptive(datos_recodificados$Curricular.units.1st.sem..without.evaluations.[datos_recodificados$Curricular.units.1st.sem..without.evaluations.!=0])
datos_recodificados$Curricular.units.1st.sem..without.evaluations.[datos_recodificados$Curricular.units.1st.sem..without.evaluations.!=0]
hist(datos_recodificados$Curricular.units.1st.sem..without.evaluations.[datos_recodificados$Curricular.units.1st.sem..without.evaluations.!=0]
)
axis(1, at=seq(0,12, by=1))
boxplot(datos_recodificados$Curricular.units.1st.sem..without.evaluations.[datos_recodificados$Curricular.units.1st.sem..without.evaluations.!=0])

# WITHOUT EVALUATIONS 2nd SEM
descriptive(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.)
hist(datos_recodificados$Curricular.units.2nd.sem..without.evaluations., breaks=seq(0, 12, by=1), xaxt="n")
axis(1, at=seq(0,12, by=1))
#ver cuantos valores hay distintos de 0
descriptive(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.[datos_recodificados$Curricular.units.2nd.sem..without.evaluations.!=0])
datos_recodificados$Curricular.units.2nd.sem..without.evaluations.[datos_recodificados$Curricular.units.2nd.sem..without.evaluations.!=0]
hist(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.[datos_recodificados$Curricular.units.2nd.sem..without.evaluations.!=0]
)
axis(1, at=seq(0,12, by=1))
boxplot(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.[datos_recodificados$Curricular.units.2nd.sem..without.evaluations.!=0])

#comparo sem 1 y sem 2 para ver cual tieen mas falts en total
sum(datos_recodificados$Curricular.units.1st.sem..without.evaluations.)
sum(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.) #sale más faltas en sem2

#ahora con un ecdf
plot(ecdf(datos_recodificados$Curricular.units.1st.sem..without.evaluations.),do.points=FALSE, col="blue", verticals=TRUE)
plot(ecdf(datos_recodificados$Curricular.units.2nd.sem..without.evaluations.), do.points=FALSE, col="red", add=TRUE, verticals=TRUE)
grid()
axis(1, at=seq(0, 12, by=1))


#***************
#EVALUCIACIONES
#***************

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

#2SEM
#descriptive general
descriptive(datos_recodificados$Curricular.units.2nd.sem..evaluations.)
#descriptivo por pib/año
tapply(datos_recodificados$Curricular.units.2nd.sem..evaluations., datos_recodificados$PIB, descriptive) #sale lo del final pq descriptive es una funcion q cada vez q se eejcuta imprime aparte de return, entonces mientras se ejecuta me lo va enseñando, y al final me pone el vector del valor_PIB: resultado, para los tres casos: variable numerica(mi caso, evaluationes es numerica), categorica(no solo le hje dado una variable numerica asi q no sale nada) y fecha(lo mismo, null).
boxplot(datos_recodificados$Curricular.units.2nd.sem..evaluations.~datos_recodificados$PIB)
#para carreras en cada año


ggplot(datos_recodificados,
       aes(x = factor(PIB),
           y = Curricular.units.2nd.sem..evaluations.)) +
  geom_boxplot() +
  facet_wrap(~ Course) +
  labs(
    x = "PIB",
    y = "Evaluaciones 2º semestre",
    title = "Distribución de evaluaciones por PIB en cada carrera"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#vemos si en Multimedia tiene que ver target con pib
tablamulti<-table(datos_recodificados$PIB[datos_recodificados$Course =="Diseño de Animación y Multimedia"], datos_recodificados$Target[datos_recodificados$Course =="Diseño de Animación y Multimedia"])
mosaicplot(tablamulti)

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

#****************************
#Asignaturas matriculadas
#****************************
#1SEM
datos_recodificados$Curricular.units.1st.sem..enrolled.
descriptive(datos_recodificados$Curricular.units.1st.sem..enrolled.)
hist(datos_recodificados$Curricular.units.1st.sem..enrolled., col="pink", breaks=seq(0, 26, by=1), xaxt="n", yaxt="n")
axis(1, at=seq(0, 26, by=1),las=2)
axis(2, at=seq(0, 2000, by=100), las=2)
boxplot(datos_recodificados$Curricular.units.1st.sem..enrolled.~datos_recodificados$Course, las=2, cex.axis=0.7)

#ver si los q no se matriculan a nada en el semestre 1 tampoco lo hacen en el segundo
datos_recodificados$Curricular.units.2nd.sem..enrolled.[datos_recodificados$Curricular.units.1st.sem..enrolled.==0]
#probamos ppn x si acaso


#2SEM
descriptive(datos_recodificados$Curricular.units.2nd.sem..enrolled.)
hist(datos_recodificados$Curricular.units.2nd.sem..enrolled., col="pink", breaks=seq(0, 26, by=1), xaxt="n", yaxt="n")
axis(1, at=seq(0, 26, by=1),las=2)
axis(2, at=seq(0, 2000, by=100), las=2)
boxplot(datos_recodificados$Curricular.units.2nd.sem..enrolled.~datos_recodificados$Course, las=2, cex.axis=0.7)

#**************************
#Creditadas(convalidadas)
#**************************
#1SEM
descriptive(datos_recodificados$Curricular.units.1st.sem..credited.)
#ver cual es la frecuencia de 0 
tabla_cred<-table(datos_recodificados$Curricular.units.1st.sem..credited.)
tabla_cred
barplot(tabla_cred)
#ppexp
x_exp <- datos_recodificados$Curricular.units.1st.sem..credited.
qqplot(
  qexp(ppoints(length(x_exp)), rate = 1/mean(x_exp)),
  sort(x_exp),
  main = "QQ plot exponencial",
  xlab = "Cuantiles teóricos",
  ylab = "Cuantiles observados"
)
abline(0,1)
grid()
#BXPLOT Por carreras
boxplot(datos_recodificados$Curricular.units.1st.sem..credited.~datos_recodificados$Course, las=2)

#2SEM
descriptive(datos_recodificados$Curricular.units.2nd.sem..credited.)
boxplot(datos_recodificados$Curricular.units.2nd.sem..credited.~datos_recodificados$Course, las=2)
tabla_cred2<-table(datos_recodificados$Curricular.units.2nd.sem..credited.)
tabla_cred2
barplot(tabla_cred2)

