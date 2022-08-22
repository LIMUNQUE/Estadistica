#Proyecto
#Leemos el csv
data <- read.csv("Base2DatosEstudiantes.csv")
View(data)

#Obtenemos variables a analizar
#Cualitativas (4)
sexo <- data$Sexo
cantMaterias <- data$Materias_Promedio_x_Termino
ejercicio <- data$Realiza_ejercicio
comidasDiarias <- data$Numero_comidas_diarias


#Cuantitativa (5)
#Promedio
promedio <- data$Promedio
hist(promedio)
boxplot(promedio)
mean(promedio)
median(promedio)
sd(promedio)
quantile(promedio,0.25)
quantile(promedio,0.75)
tb1=data.frame(mean(promedio),median(promedio),median(promedio),sd(promedio),quantile(promedio,0.25),quantile(promedio,0.75))
tb1 =(tb1)
View(tb1)
barplot(tb1$mean.promedio.,tb1$median.promedio.,)


#Carbohidratos por Comida
carbohidratosPorComida <- data$Porcentaje_promedio_carbohidrato_por_comida
hist(carbohidratosPorComida)
tb2=data.frame(mean(carbohidratosPorComida),median(carbohidratosPorComida),median(carbohidratosPorComida),sd(carbohidratosPorComida),quantile(carbohidratosPorComida,0.25),quantile(carbohidratosPorComida,0.75))
barplot(tb2$mean.carbohidratosPorComida.,tb1$median.carbohidratosPorComida.)

#Estatura en cm
estatura <- data$Estatura
hist(estatura)
tb3=data.frame(mean(estatura),median(estatura),median(estatura),sd(estatura),quantile(estatura,0.25),quantile(estatura,0.75))
barplot(tb3$mean.estatura.,tb3$median.estatura.)

#Peso en Kg
peso <- data$Peso
hist(peso)
tb4=data.frame(mean(peso),median(peso),median(peso),sd(peso),quantile(peso,0.25),quantile(peso,0.75))
barplot(tb4$mean.peso.,tb4$median.peso.)

#Consumo de agua promedio semanal en litro
consumoAgua <- data$Consumo_promedio_semanal_agua
hist(consumoAgua)
tb5=data.frame(mean(consumoAgua),median(consumoAgua),median(consumoAgua),sd(consumoAgua),quantile(consumoAgua,0.25),quantile(consumoAgua,0.75))
barplot(tb5$mean.consumoAgua.,tb5$median.consumoAgua.)
