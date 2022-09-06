#Proyecto
install.packages("readxl")
install.packages("dplyr")
install.packages("fdth")
install.packages("ggcorrplot")
install.packages("moments")
install.packages("GGally")
install.packages("nortest")
install.packages("plotly")
# Carga de librerias.
library("dplyr")          #Dataframes management
library("fdth")              #Tablas de frec, hist y poligonos
library("ggcorrplot")    #Correlation matrix on ggplot2
library("moments")        #Kurtosis
library("GGally")          #Mas funciones para ggplot2
library("nortest")        #Tests de normalidad
library("ggplot2")        #Graficos
library("plotly") 

#Leemos el csv
data <- read.csv("Base2DatosEstudiantes.csv")
View(data)
datos = subset(data, select=c("Sexo","Materias_Promedio_x_Termino","Realiza_ejercicio","Numero_comidas_diarias","Promedio","Porcentaje_promedio_carbohidrato_por_comida","Estatura","Peso","Consumo_promedio_semanal_agua"))

#Grafica de variables cualitativas
sex_bd<- ggplot(data) + aes(x = Sexo, fill = Sexo) +
  geom_bar() + 
  labs(title ="Diagrama de Barra de Sexo\n", x="Sexo", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)
ggplotly(sex_bd)

#Materias_Promedio_x_Termin
materias = ggplot(data) + aes(x = Materias_Promedio_x_Termino, fill = Materias_Promedio_x_Termino) +
  geom_bar() + 
  labs(title ="Diagrama de Promedio de materias por termino\n", x="Número de materias", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)
ggplotly(materias)

#Realiza ejercicio
ejercicio = ggplot(data) + aes(x = Realiza_ejercicio, fill = Realiza_ejercicio) +
  geom_bar() + 
  labs(title ="Diagrama de Promedio personas que realizan ejercicio\n", x="Personas que realizan ejercicio", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)
ggplotly(ejercicio)

# Comidas diarias
comidas<- ggplot(data) + aes(x = Numero_comidas_diarias, fill = Numero_comidas_diarias) +
  geom_bar() + 
  labs(title ="Diagrama de Barra de Comidas diarias\n", x="Número de comidas diarias", y="Frecuencia") +
  geom_text(stat='count', aes(label=..count..), vjust=1)
ggplotly(comidas)



#Cualitativas (4)
sexo <- data$Sexo
cantMaterias <- data$Materias_Promedio_x_Termino
ejercicio <- data$Realiza_ejercicio
comidasDiarias <- data$Numero_comidas_diarias


#Cuantitativas (5)

#Promedio
promedio <- data$Promedio
hist(promedio)
boxplot(promedio)
sd(promedio)
quantile(promedio,0.25)
quantile(promedio,0.75)
tb1=data.frame(mean(promedio),median(promedio),median(promedio),sd(promedio),quantile(promedio,0.25),quantile(promedio,0.75))
tb1 =(tb1)
View(tb1)
summary(data$Promedio)
kurtosis(data$Promedio)
skewness(data$Promedio)

promedio1 <- fdt(data$Promedio,
                breaks=c('Sturges'),
                right=FALSE,
                na.rm=FALSE)
promedio_table <- promedio1$table
#Histograma de promedio
prom_hist<-ggplot(promedio_table, aes(factor(`Class limits`), `f`, fill = `Class limits`)) + 
  geom_col()+
  labs(title ="Histograma de Promedios\n", x="Promedios", y="Frecuencia") +
  geom_text(aes(label = `f`), vjust = 1)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggplotly(prom_hist)
#Diagrama de cajas de promedio
prom_boxplot<-ggplot(data, aes(x="",y=Promedio))+
  geom_boxplot(fill = 2)+
  labs(title ="Diagrama de cajas de Promedios\n")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
prom_boxplot
ggplotly(prom_boxplot)



#Carbohidratos por Comida
carbohidratosPorComida <- data$Porcentaje_promedio_carbohidrato_por_comida
hist(carbohidratosPorComida)
tb2=data.frame(mean(carbohidratosPorComida),median(carbohidratosPorComida),median(carbohidratosPorComida),sd(carbohidratosPorComida),quantile(carbohidratosPorComida,0.25),quantile(carbohidratosPorComida,0.75))
summary(carbohidratosPorComida)

#Estatura en cm
estatura <- data$Estatura
hist(estatura)
tb3=data.frame(mean(estatura),median(estatura),median(estatura),sd(estatura),quantile(estatura,0.25),quantile(estatura,0.75))


#Peso en Kg
peso <- data$Peso
hist(peso)
tb4=data.frame(mean(peso),median(peso),median(peso),sd(peso),quantile(peso,0.25),quantile(peso,0.75))


#Consumo de agua promedio semanal en litro
consumoAgua <- data$Consumo_promedio_semanal_agua
hist(consumoAgua)
tb5=data.frame(mean(consumoAgua),median(consumoAgua),median(consumoAgua),sd(consumoAgua),quantile(consumoAgua,0.25),quantile(consumoAgua,0.75))


# Estadistica descriptiva univariante



#Comparación Sexo vs Promedio
sexo_prom_boxplot<-ggplot(data, aes(x=Sexo,y=Promedio))+
  geom_boxplot(fill = 2)+
  labs(title ="Diagrama de cajas de Sexo - Promedio\n",y="Horas promedio")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)
ggplotly(sexo_prom_boxplot)

#Matriz grafica de correlacion - Cuantitativas

#Intervalo de confianza
hombres<-data[data$Sexo=="M", ]
mujeres<-data[data$Sexo=="F", ]

prom_hombres_<-hombres[hombres$Promedio>8,]

shapiro.test(hombres$Promedio)
shapiro.test(mujeres$Promedio)

