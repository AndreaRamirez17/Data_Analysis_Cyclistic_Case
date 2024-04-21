#Cargamos Librerias para el analisis de datos
library(tidyverse)
library(janitor)
library(dplyr)
library(skimr)
library(ggplot2)
library(readr)
library(lubridate)
library(modeest)
library(tidyr)

#Cargamos los archivos csv 
enero<- read_csv("Casos Practicos/ZIP y CSV/202301-divvy-tripdata.csv")
febrero<-read_csv("Casos Practicos/ZIP y CSV/202302-divvy-tripdata.csv")
marzo<-read_csv("Casos Practicos/ZIP y CSV/202303-divvy-tripdata.csv")
abril<-read_csv("Casos Practicos/ZIP y CSV/202304-divvy-tripdata.csv")
mayo<-read_csv("Casos Practicos/ZIP y CSV/202305-divvy-tripdata.csv")
junio<-read_csv("Casos Practicos/ZIP y CSV/202306-divvy-tripdata.csv")
julio<-read_csv("Casos Practicos/ZIP y CSV/202307-divvy-tripdata.csv")
agosto<-read_csv("Casos Practicos/ZIP y CSV/202308-divvy-tripdata.csv")
sept<-read_csv("Casos Practicos/ZIP y CSV/202309-divvy-tripdata.csv")
oct<-read_csv("Casos Practicos/ZIP y CSV/202310-divvy-tripdata.csv")
nov<-read_csv("Casos Practicos/ZIP y CSV/202311-divvy-tripdata.csv")
dec<-read_csv("Casos Practicos/ZIP y CSV/202312-divvy-tripdata.csv")

#Combinamos todo en un mismo data frame 

alldata<-rbind(enero,febrero,marzo,abril,mayo,junio,agosto,sept,oct,nov,dec)
head(alldata)


#Crearemos tres columnas de interés, la primer columna se llamará “ride_length”. Calculando la extensión de cada viaje restando la columna “started_at” de la columna “ended_at”. La segunda columna se llamará "day_of_week". Calculando el día de la semana en el que empezó cada viaje mediante el comando “wday” . Observando que 1 = domingo y 7 = sábado. La tercer  columna se llamará  "month". Calculando el mes en el que se hicieron los viajes, mediante el comando "month". Observado que 1= enero y 12=diciembre.Por ultimo filtramos la duracion del viaje, para solo considerar viajes mayores a un minuto, ya que los que sean meor a este tiempo no son relevantes para el estudio del caso.

alldata<- alldata %>% 
  mutate(ride_length=as.double(ended_at - started_at),
         day_of_week= as.integer(wday(started_at)),
         month=as.integer(month(started_at))) %>%
           filter(ride_length>=60)


#Obtendremos  el promedio de la duración de los viajes, asi como otros resultados de interés, los resultados estan en segundos.

alldata%>% 
  group_by(member_casual) %>% 
  summarize(media=mean(ride_length),moda=mfv(ride_length),maximo=max(ride_length),min=min(ride_length))

#Contamos el total de viajes por usuario
viajes_totales_por_usuario <- alldata %>% 
  count(member_casual)
print(viajes_totales_por_usuario)

#Calculamos los porcentajes  de los viajes totales casuales y anuales
totales_casuales <- alldata %>%
  filter(member_casual=="casual")%>%
  count(member_casual)%>%
  mutate(porcentaje = (n/sum(viajes_totales_por_usuario$n))*100, n_str = n)%>%
  select(porcentaje)
redo<-round(totales_casuales,digits = 0)
totalcas<-paste(redo,"%")

totales_anuales <-alldata %>%
  filter(member_casual=="member")%>%
  count(member_casual)%>%
  mutate(porcentaje = (n/sum(viajes_totales_por_usuario$n))*100, n_str = n)%>%
  select(porcentaje)
red<-round(totales_anuales,digits = 0)
totalan<-paste(red,"%")

#Crearemos variables pata el titulo y subtitulo de una gráfica 
titulo <- "Viajes realizados durante 2023"
subtitulo <- paste("Viajes totales por tipo de usuario:\n","Casuales=",totalcas," Anuales=",totalan)

#Creamos una grafica de pastel de los viajes totales por usuario
ggplot(data = viajes_totales_por_usuario, mapping = aes(x="", y=n, fill=member_casual)) +
  geom_col() +
  labs(fill = "Tipos de usuario:") +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle(titulo, subtitle = subtitulo) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#Creamos una grafica de barras de los viajes totales por mes y tipo de usuario
ggplot(data = alldata)+
  geom_bar(mapping = aes(x=month,fill=member_casual), position = "dodge")+
  labs(fill = "Tipos de usuario:") +
  scale_fill_discrete(labels = c("Casuales", "Anuales")) +
  ggtitle("Viajes realizados durante 2023 por mes y por tipo de usuario") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 1)) +
  ylab("Viajes realizados")+
  xlab("Mes") +
  scale_x_continuous(breaks = c(1:12),
                   labels = c("ENE","FEB", "MAR", "ABR", "MAY", "JUN",
                              "JUL", "AGO", "SEP", "OCT", "NOV", "DIC"))


# Creamos una grafica de los viajes totales por semana y por usuario
ggplot(data = alldata)+
  geom_bar(mapping = aes(x=day_of_week,fill=member_casual), position = "dodge")+
  labs(fill = "Tipos de usuario:") +
  scale_fill_discrete(labels = c("Casuales", "Anuales")) +
  ggtitle("Viajes realizados durante 2023 por semana y por tipo de usuario") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 1)) +
  ylab("Viajes realizados")+
  xlab("Dia de la semana") +
  scale_x_continuous(breaks = c(1:7),
                   labels = c("DOMINGO","LUNES","MARTES", "MIERCOLES", "JUEVES", "VIERNES", "SABADO" ))

#Calculamos nuevamente el total de usuarios por modelo de bicicleta
tipo_bicicleta <- alldata %>% 
  count(rideable_type)

#Calculamos el porcentaje
clasica <- alldata %>%
  filter(rideable_type=="classic_bike")%>%
  count(rideable_type)%>%
  mutate(porcentaje = (n/sum(tipo_bicicleta$n))*100, n_str = n)%>%
  select(porcentaje)
redondeo_classic<-round(clasica,digits = 0)
Classic<-paste(redondeo_classic,"%")

docked <- alldata %>%
  filter(rideable_type=="docked_bike")%>%
  count(rideable_type)%>%
  mutate(porcentaje = (n/sum(tipo_bicicleta$n))*100, n_str = n)%>%
  select(porcentaje)
redondeo_docked<-round(docked,digits = 0)
Dockedd<-paste(redondeo_docked,"%")

electric <- alldata %>%
  filter(rideable_type=="electric_bike")%>%
  count(rideable_type)%>%
  mutate(porcentaje = (n/sum(tipo_bicicleta$n))*100, n_str = n)%>%
  select(porcentaje)
redondeo_electric<-round(electric,digits = 0)
Electrica<-paste(redondeo_electric,"%")
#Variables para titulos de grafica
titulo <- "Viajes realizados por modelo de bicicleta:\n"
subtitulo <- paste("Classic_bike=",Classic
                   ," Docked_bike=",Dockedd,"\n Electric_bike=",Electrica)


#Creamos grafica de pastel de usuarios totales por modelo de bicicleta 
ggplot(data = tipo_bicicleta, mapping = aes(x="", y=n, fill=rideable_type)) +
  geom_col() +
  labs(fill = "Tipos de bicicleta:") +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle(titulo, subtitle = subtitulo) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

tipo_bicicleta <- alldata %>% 
  count(rideable_type)


#Calculamos los porcentajes de los viajes por tipo de usuario y modelo de bicicleta
casual_by_bike<-alldata%>%
  group_by(rideable_type)%>%
  filter(member_casual=="casual")%>%
  count(member_casual)%>%
  mutate(porcentaje = (n/sum(tipo_bicicleta$n))*100, n_str = n)%>%
  select(rideable_type,member_casual,porcentaje,)
print(casual_by_bike)

member_by_bike<-alldata%>%
  group_by(rideable_type)%>%
  filter(member_casual=="member")%>%
  count(member_casual)%>%
  mutate(porcentaje = (n/sum(tipo_bicicleta$n))*100, n_str = n)%>%
  select(rideable_type,member_casual,porcentaje,)
print(member_by_bike)


#Grafica de pastel  de los viajes realizados por tipo de usurio y modelo de bicicleta
ggplot(data=alldata)+
  geom_bar(mapping = aes(x=member_casual, fill=rideable_type),position = "dodge")+
  labs(fill = "Tipos de bicicleta:") +
  scale_fill_discrete(labels = c("Classic_bike", "Docked_bike","Electric_bike")) +
  ggtitle("Viajes realizados durante 2023 por tipo de usuario y el modelo de bicicleta") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 1)) +
  ylab("Viajes realizados")+
  xlab("Tipo de usuario") 



