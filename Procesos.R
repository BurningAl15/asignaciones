install.packages("dplyr")#manipulacion de datos
install.packages("stringr")#manipulacion de cadenas de caracteres
install.packages("lubridate")#manipulacion de fechas

library("dplyr")
library("stringr")
library("lubridate")

getwd()

procesamiento<-setwd("C:/Users/aldhair/Documents/R/Unidad3/Criminalidad/Procesamiento")
dataset<-setwd("C:/Users/aldhair/Documents/R/Unidad3/Criminalidad/Datasets")
reportes<-setwd("C:/Users/aldhair/Documents/R/Unidad3/Criminalidad/Reportes")

Datos<-read.csv(paste(dataset,"Datos2.csv",sep="/"))
head(Datos,n=20)
#Mostrar categoria del delito
Datos %>% select(Categoria)
#Mostrar solo puesto y la categoria de delito
Datos %>% select(Categoria,Puesto)
#Mostrar los campos excepto el puesto policial
head(Datos,n=20000) %>% select(-Puesto)
#Ordenar por categoria de delito
head(Datos,n=20) %>% arrange(Categoria)

#Agregar columnas del año, mes, dia del delito (uso de lubridate)
Datos %>% mutate(anho=year(Datos$Fecha_Hora),mes=month(Datos$Fecha_Hora),dia=day(Datos$Fecha_Hora))

#Agregar columnas para hora y minuto (uso de lubridate)
add<-function()
{
  d<-Datos %>% mutate(hora=hour(Datos$Fecha_Hora),minuto=minute(Datos$Fecha_Hora))
  return(d)
}

View(head(add(),100))


Datos %>% distinct(categoria)
#Cantidad de categorias
Datos$Categoria %>% n_distinct()
Datos %>% summarise(nro=n_distinct(Categoria))
#filtrar los delitos de la categoria robos.
Datos %>% filter(Categoria=="Robos")

#Filtrar los delitos por categoria que contenga robo.
Datos%>% filter(str_detect(Categoria,"Robo")& Puesto==18)
Datos%>% filter(str_detect(Categoria,"Robo")) %>% filter(Puesto==18) %>% arrange(Categoria)

#Mostrar k categoria de robos y con fecha mayor al 31-12-2014
class(Datos$Fecha_Hora)
fecha<- as.POSIXct(Datos$Fecha_Hora)
Datos %>% filter(fecha>as.POSIXct("2014-12-31"))
?as.POSIXct
##################################################################################################################
#Cantidad de denuncias por dia

Datos%>%group_by(as.Date(Datos$Fecha_Hora))%>%summarise(n=n())
group_by(summarise)

install.packages("tidyr")
library(tidyr)
aa<-Datos%>%tidyr::separate(Fecha_Hora,into=c("Anio","Mes","Dia"))%>%filter(Categoria=="Fraude",Mes==12)%>%group_by(Datos$Categoria)%>% summarise(n=n())
bb<-Datos%>%group_by(as.Date(Datos$Fecha_Hora))%>%summarise(n=n())
str(aa)
View(aa)
head(aa)

#Graficar
library(ggplot2)
qplot(x=bb$`as.Date(Datos$Fecha_Hora)`,y=bb$n,geom="point")
qplot(x=bb$`as.Date(Datos$Fecha_Hora)`,y=bb$n,geom=c("density","point"))
df<-Datos%>% group_by(as.Date(Datos$Fecha_Hora))%>%summarise(n=n())
colnames(df)=c("Fecha","Cantidad")
ggplot(df,aes(Fecha_Hora,Cantidad),color=Cantidad)+geom_line()
