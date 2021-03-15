#Encuesta de trabajadores en la industria de software 
#Desde 2014 sysarmy lleva a cabo las Encuestas de Sueldos en la región, 
#abarcando a Argentina y el resto de Latinoamérica. 
#Trabajaremos sobre los datos de los salarios argentinos

##Objetivo:
#Introducir R vía RStudio indentificar tipos de variables. Generar tablas y medidas  resumenes y gráficos descriptivos 


#leer tabla de datos
#setwd("C:/Users/franc/Dropbox/Franca/UCAsueldossys")

#librerias
library(readxl)

sueldossys <- read_excel("datos/sys2021.xlsx")

View(sueldossys)

#Población, n de la muestra
dim(sueldossys)

#Primer variable Actividad principal
class(sueldossys$`Actividad principal`)

act <- table(sueldossys$`Actividad principal`)

plot(act)

colors = c("green3", "purple", "grey") 

barplot(act, col=colors )

pie(act, col = colors)


#Segunda variable edad 

class(sueldossys$Tengo)

sueldossys$Tengo <- as.integer(sueldossys$Tengo)

summary(sueldossys$Tengo)

plot(sueldossys$Tengo)

hist(sueldossys$Tengo)

which(sueldossys$Tengo>80)

sueldossys_res <- sueldossys %>% filter(Tengo < 80) %>% summarise()

##depuracion 
library(tidyverse)

sueldossys_dep <- sueldossys %>% filter(Tengo < 80)

hist(sueldossys_dep$Tengo)

sueldossys_dep %>%
  group_by(`Actividad principal`) %>%
  summarise(edad = mean(Tengo), 
            sd = sd(Tengo), 
            n=n(n))

#library(ggplot2)

ggplot(sueldossys_dep, aes(Tengo))+
  geom_histogram(binwidth = 2)+
  ylab("Frecuencia absoluta")+
  xlab("Edad")

ggplot(sueldossys_dep, aes(Tengo, fill=`Actividad principal`))+
  geom_histogram(binwidth = 2)+
  ylab("Frecuencia absoluta")+
  xlab("Edad")

ggplot(sueldossys_dep, aes(`Actividad principal`,Tengo, color=`Actividad principal`))+
  geom_boxplot()+
  ylab("Edad")+
  xlab("Actividad")


ggplot(sueldossys_dep, aes(`Actividad principal`, fill=`Actividad principal`))+
  geom_bar()+
  ylab("Frecuencia absoluta")+
  xlab("Actividad")

##Lugar de trabajo
provincias <- as.data.frame(table(sueldossys$`Dónde estás trabajando`))

colnames(provincias) <- c("Provincia", "Frec abs")

ggplot(sueldossys_dep, aes(x=`Dónde estás trabajando`, fill=`Dónde estás trabajando`))+
  geom_bar()+
  ylab("Frecuencia absoluta")+
  xlab("Provincia")

##sueldos
summary(sueldossys$`Salario mensual o retiro NETO (en tu moneda local)`)

hist(sueldossys$`Salario mensual o retiro NETO (en tu moneda local)`)

salario_bruto <- sueldossys %>% 
  filter(`Salario mensual o retiro NETO (en tu moneda local)` < 400000) %>% 
  select(`Salario mensual o retiro NETO (en tu moneda local)`) %>%
  rename(bruto=`Salario mensual o retiro NETO (en tu moneda local)`)


salario_bruto %>%
  summarise(`Salario bruto medio`= mean(bruto), 
            sd = sd(bruto), 
            n=n())  

ggplot(salario_bruto, aes(bruto))+
  geom_histogram(binwidth = 10000)+
  ylab("Frecuencia absoluta")+
  xlab("Salario bruto mensual")

ggplot(salario_bruto, aes(bruto))+
  geom_step(stat="ecdf") +
  ylab("Frecuencia")+
  xlab("Salario bruto mensual")


## demasiado por hoy
