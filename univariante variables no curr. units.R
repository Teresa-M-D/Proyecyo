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




#*********************
#Application order
#*********************
descriptive(datos_recodificados$Application.order)
tabla_orden<-table(datos_recodificados$Application.order)
barplot(tabla_orden,  yaxt="n")
axis(2, at=seq(0, 3500, by=100), las=2)
#para ver si la opcion 0 es porque la forma de aplicqacion es distita, voy a ver si todos los de 0 no vienen del metodo normal
tabla_aplicacion<-table(datos_recodificados$Application.order, datos_recodificados$Application.mode)
tabla_aplicacion
unique(datos_recodificados$Application.mode)
chisq.test(tabla_aplicacion, simulate.p.value = TRUE)
sum(datos_recodificados$Application.order[datos_recodificados$Application.mode!="Titulares de otros estudios superiores"]==0)
mosaicplot(tabla_aplicacion, las=2)
#ppexp
#como es discreta, no se puede ver si cuadra con exponenecial con ppexp, entonces vemos otro metodo
plot(
  as.numeric(names(table(datos_recodificados$Application.order))),
  as.numeric((table(datos_recodificados$Application.order))),
  pch = 19,
  xlab = "Application order",
  ylab = "Frecuencia"
)
# en general el dibujo se parece a la exponenecial, entonces:
x <- as.numeric(names((table(datos_recodificados$Application.order))))
y <- log(as.numeric((table(datos_recodificados$Application.order))))

modelo_exp <- lm(y ~ x)
summary(modelo_exp)
#no es exponencial

#***********
#Inflation rate
#***********
descriptive(datos_recodificados$Inflation.rate)
barplot(table(datos_recodificados$Inflation.rate))
table(datos_recodificados$Inflation.rate)
#***********
#PIB
#***********
descriptive(datos_recodificados$PIB)
barplot(table(datos_recodificados$PIB))
table(datos_recodificados$PIB)
#***********
#Unemployment
#***********
descriptive(datos_recodificados$Unemployment.rate)
barplot(table(datos_recodificados$Unemployment.rate))
table(datos_recodificados$Unemployment.rate)

