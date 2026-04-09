datos<-read.csv("estudiantes.csv", sep=";")
head(datos)
install.packages(c("clickR", "DescTools","plotly", "scatterplot3d", "vcd","ggplot2","gridExtra"))
install.packages("rio", dependencies = TRUE)
library(gridExtra)
library(clickR)
library(DescTools)
library(plotly)
library(scatterplot3d)
library(vcd)
library(ggplot2)
library(dplyr)

#corrección nombre variable 
datos["Nationality"]=datos["Nacionality"]
datos$Nacionality<-NULL

#Llamo a GDP PIB
datos["PIB"]=datos["GDP"]
datos$GDP<-NULL

