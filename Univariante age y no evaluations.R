#AGE AT ENROLLMENT
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

# WITHOUT EVALUATIONS 1st SEM
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
