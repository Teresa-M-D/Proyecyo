df<-read.csv("data.csv", sep=";")
head(df)
install.packages(c("clickR", "DescTools","plotly", "scatterplot3d", "vcd","ggplot2","gridExtra"))
install.packages("rio", dependencies = TRUE)
library(gridExtra)
library(clickR)
library(DescTools)
library(plotly)
library(scatterplot3d)
library(vcd)
library(ggplot2)

#corrección nombre variable 
df["Nationality"]=df["Nacionality"]
df$Nacionality<-NULL

#Llamo a GDP PIB
df["PIB"]=df["GDP"]
df$GDP<-NULL
