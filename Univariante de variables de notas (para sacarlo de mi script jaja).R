#Describe y descriptivos de cada una por separado
datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0]
comparacion_semestres_y_nota_entrada<-c("Previous.qualification.grade_10",
                                        "Admission.grade_10",
                                        "Curricular.units.1st.sem.grade_10",
                                        "Curricular.units.2nd.sem.grade_10", 
)

describe(datos_recodificados[,"Curricular.units.1st.sem.grade_10"])
describe(datos_recodificados[,"Curricular.units.2nd.sem.grade_10"])
describe(datos_recodificados[,"Previous.qualification.grade_10"])
describe(datos_recodificados[,"Admission.grade_10"])
descriptive(datos_recodificados$Curricular.units.1st.sem.grade_10)
descriptive(datos_recodificados$Curricular.units.2nd.sem.grade_10)

descriptive(datos_recodificados$Previous.qualification.grade_10)
descriptive(datos_recodificados[,comparacion_semestres_y_nota_entrada])
descriptive(datos_recodificados$Admission.grade_10)
descriptive(datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0])
descriptive(datos_recodificados$Curricular.units.2nd.sem.grade_10[datos_recodificados$Curricular.units.2nd.sem.grade_10!=0])

""
#Boxplots
boxplot(datos_recodificados$Previous.qualification.grade_10,
        yaxt = "n",
        ylab = "Nota",
        main = "Nota estudios previos")
axis(2, at = seq(4, 10, by = 0.5))
boxplot(datos_recodificados$Age.at.enrollment)
boxplot(datos_recodificados$Admission.grade_10, yaxt="n", main="Admission grade")
axis(2, at=seq(4.5,10, by=0.5))
boxplot(datos_recodificados$Application.order)
boxplot(datos_recodificados$Curricular.units.1st.sem..credited.)
boxplot(datos_recodificados$Curricular.units.1st.sem..enrolled., yaxt="n")
axis(2, at=seq(0,25, by=1))
boxplot(datos_recodificados$Curricular.units.1st.sem..evaluations., yaxt="n", main="Grades 1st Sem.")
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

#ecdf
plot(ecdf(datos_recodificados$Admission.grade_10), las=1, yaxt="n")
axis(2, at=seq(0,1,by=0.1 ))
grid()
plot(ecdf(datos_recodificados$Previous.qualification.grade_10), las=1, yaxt="n")
axis(2, at=seq(0,1,by=0.1 ))
grid()
#papel probabiístico normal
#admission
qqnorm(
  datos_recodificados$Admission.grade_10,
  main = "Papel probabilístico normal Admission Grade",
)
qqline(datos_recodificados$Admission.grade_10)
grid()
#previous qual grade
qqnorm(
  datos_recodificados$Previous.qualification.grade_10,
  main = "Papel probabilístico normal Admission Grade",
)
qqline(datos_recodificados$Previous.qualification.grade_10)
grid()
#curricular 1st sem grades (con 0)
qqnorm(
  datos_recodificados$Curricular.units.1st.sem.grade_10,
  main = "Papel probabilístico normal 1st Sem. Grades",
)
qqline(datos_recodificados$Curricular.units.1st.sem.grade_10)
grid()
#curricular 1st sem grades (sin 0)
qqnorm(
  datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0],
  main = "Papel probabilístico normal 1st Sem. Grades",
)
qqline(datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0])
grid()
#curricular 2nd sem grades (con 0)
qqnorm(
  datos_recodificados$Curricular.units.2nd.sem.grade_10,
  main = "Papel probabilístico normal 2nd Sem. Grades",
)
qqline(datos_recodificados$Curricular.units.2nd.sem.grade_10)
grid()
#curricular 2st sem grades (sin 0)
qqnorm(
  datos_recodificados$Curricular.units.2nd.sem.grade_10[datos_recodificados$Curricular.units.2nd.sem.grade_10!=0],
  main = "Papel probabilístico normal 2nd Sem. Grades",
)
qqline(datos_recodificados$Curricular.units.2nd.sem.grade_10[datos_recodificados$Curricular.units.2nd.sem.grade_10!=0])
grid()
#papel probabiístico exponencial
x_exp <- datos_recodificados$Admission.grade_10 - 4.75 #le restamos 4.75 para q el mínimo sea 0
qqplot(
  qexp(ppoints(length(x_exp)), rate = 1/mean(x_exp)),
  sort(x_exp),
  main = "QQ plot exponencial",
  xlab = "Cuantiles teóricos",
  ylab = "Cuantiles observados"
)

abline(0,1)
#Estudio de las variables dew notas de 1 y 2 sem
#1SEM
#histograma
hist(datos_recodificados$Curricular.units.1st.sem.grade_10, breaks=seq(0, 10, by=0.5), xaxt="n")
axis(1, at=seq(0,10, by=0.5))
hist(datos_recodificados$Previous.qualification.grade_10, breaks=seq(0, 10, by=0.5), xaxt="n")
axis(1, at=seq(0,10, by=0.5))
hist(datos_recodificados$Admission.grade_10, breaks=seq(0, 10, by=0.5), xaxt="n")
axis(1, at=seq(0,10, by=0.5))
#media de solo los alumnos q se han presentado
median(datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0])
mean(datos_recodificados$Curricular.units.1st.sem.grade_10[datos_recodificados$Curricular.units.1st.sem.grade_10!=0])

#2 SEM
hist(datos_recodificados$Curricular.units.2nd.sem.grade_10, breaks=seq(0, 10, by=0.5), xaxt="n")
axis(1, at=seq(0,10, by=0.5))

median(datos_recodificados$Curricular.units.2nd.sem.grade_10[datos_recodificados$Curricular.units.2nd.sem.grade_10!=0])
mean(datos_recodificados$Curricular.units.2nd.sem.grade_10[datos_recodificados$Curricular.units.2nd.sem.grade_10!=0])