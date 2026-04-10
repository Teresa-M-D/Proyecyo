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
datos_recodificados$Curricular.units.1st.sem..without.evaluations.[datos_recodificados$Curricular.units.1st.sem..without.evaluations.!=0]
hist(datos_recodificados$Curricular.units.1st.sem..without.evaluations.[datos_recodificados$Curricular.units.1st.sem..without.evaluations.!=0]
)
axis(1, at=seq(0,12, by=1))
boxplot(datos_recodificados$Curricular.units.1st.sem..without.evaluations.[datos_recodificados$Curricular.units.1st.sem..without.evaluations.!=0])


