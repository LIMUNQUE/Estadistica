#Proyecto
#Leemos el csv
data <- read.csv("Base2DatosEstudiantes.csv")
View(data)

#Se obtiene el promedio para analizar
promedio <- data$Promedio
hist(promedio)
boxplot(promedio)
mean(promedio)
median(promedio)
sd(promedio)
