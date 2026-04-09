#espacio para mí:)
#COSAS BÁSICAS
head(datos)
descriptive(datos)
#ver nombres variables
names(datos)
matriculado=datos$Curricular.units.1st.sem..enrolled
max(matriculado)
boxplot(datos$Target)


