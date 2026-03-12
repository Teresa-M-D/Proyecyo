#Espacio de Yalda
#Análisis inicial de la variable Target

#Frecuencias absolutas
freq_target <- table(datos$Target)
freq_target
#Frecuencias relativas
prop.table(freq_target)

#Porcentajes
porcentajes = prop.table(freq_target) * 100
porcentajes