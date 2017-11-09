#Tendencia general de los delitos durante todo el per???odo de tiempo en el conjunto de datos. 
#La granularidad debe estar al nivel de d???a.

head(Datos$Fecha_Hora)
Datos$FechaHora <- as.POSIXct(Datos$Fecha_Hora, format="%Y-%m-%d %H:%M:%S", tz="GMT")
?POSIXct
head(Datos$Fecha_Hora)
head(Datos$FechaHora)

#Crear Columna Fecha
Datos$Fecha <- as.Date(Datos$FechaHora, tz="GMT")
str(Datos)

#contando el numero de crimenes por dia
por_fecha <- aggregate(Datos$Fecha, by = list(Date = Datos$Fecha), FUN = length)
?aggregate
str(por_fecha)

#Renombrando columnas
colnames(por_fecha) <- c("Fecha", "Total")

#Ploteando el resultado
ggplot(por_fecha, aes(Fecha, Total, color=Total)) + geom_line()


#2.  ???Cu???les son las horas m???s y menos peligrosas?  

#Obteniendo Horas de la columna Fecha Hora
Datos$Hora <- strftime(Datos$FechaHora, format = '%H', tz='GMT')
str(Datos)

#Consolidado por hora
por_hora <- aggregate(Datos$Hora, by = list(Hour = Datos$Hora), FUN=length)
por_hora

#renombrando columnas
colnames(por_hora) <- c("Hora", "Total")
str(por_hora)

#convertir (hora categorica) a entero
por_hora$Hora <- as.integer(por_hora$Hora)
str(por_hora)

#ploteando el resultado
ggplot(por_hora, aes(Hora, Total)) +
  geom_line(colour="Red") +
  ggtitle("Crimenes Por Hora") +
  xlab("Hora del dia") +
  ylab("Total de Crimenes")


#3.  ???Hay estacionalidad en la tasa de criminalidad? 

#Obtenew meses de la columna Fecha/hora 
Datos$Mes <- strftime(Datos$FechaHora, format = '%m', tz='GMT')
str(Datos)

#Aggregate por hora
por_mes <- aggregate(Datos$Mes, by = list(Mes = Datos$Mes), FUN=length)
por_mes

#renombrando columnas
colnames(por_mes) <- c("Mes", "Total")
str(por_mes)

#convertir horas (categoricas) a entero
por_mes$Mes <- as.integer(por_mes$Mes)
str(por_mes)

#ploteando los resultados
ggplot(por_mes, aes(Mes, Total)) +
  geom_bar(fill="Maroon", stat="identity")+
  ggtitle("Crimenes por Mes") +
  xlab("Mes") +
  ylab("Total de Crimenes")


#4.  ???Cu???les son los 10 principales tipos de cr???menes? 

#contando por tipo
por_categoria <- aggregate(Datos$Categoria,
                           by = list(Tipo = Datos$Categoria),
                           FUN = length)
por_categoria

#renombrando columnas
colnames(por_categoria) <- c("Categoria", "Total")
por_categoria

#ordenando
por_categoria_ordenado <- por_categoria[order(por_categoria$Total, decreasing=T),]
por_categoria_ordenado
?order

#seleccionando el top 10 de crimenes
top10 <- por_categoria_ordenado[1:10,]
top10

#ploteando el resultado
ggplot(top10, aes(x=reorder(Categoria,Total), y=Total)) +
  geom_bar(aes(fill=Categoria), stat="identity") +
  coord_flip()


#5.  Que puesto policial necesita ser mas fortalecido? 

#contando crimenes por puesto
por_puesto <- aggregate(Datos$Puesto, by = list(HQ = Datos$Puesto), FUN=length)

#renombrando columnas
colnames(por_puesto) <- c("Puesto", "Total")

#Ploteando los resultados
ggplot(por_puesto, aes(reorder(Puesto, -Total), Total)) +
  geom_bar(color = "blue", stat="identity")
