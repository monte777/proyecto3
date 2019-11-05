#Cargamos la librerías que se utilizarán

library(gstat)
library(sf)
library(sp)
library(MASS)
library(dismo)
library(tmap)
library(rgdal)
library(tidyverse)
library(ggfortify)

#cargamos los datos
datos <- readRDS("Datos/datos_finales.Rds")

#nombres de columnas en minúscula
colnames(datos) <- tolower(colnames(datos))

#
#set.seed(1231)
#s1 <- ts(as.numeric(datos[sample(1:289,1),8:19]))
#s2 <- ts(as.numeric(datos[sample(1:289,1),8:19]))
#s3 <- ts(as.numeric(datos[sample(1:289,1),8:19]))

#autoplot(s3)
#Pasar los datos a numericos
#Media de temperatura
datos1 <- datos %>% 
    filter(elementos == "TEM.MED.") %>%
    dplyr::select(cod_estacion,lat,lon,alt,ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic) %>% 
    mutate_if(is.character,as.numeric) %>% 
    mutate(cod = factor(cod_estacion)) %>% 
    group_by(cod) %>% 
    mutate(temp = mean(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic))) %>% 
    dplyr::select(cod,lat,lon,alt,temp) %>% 
    ungroup()

datos2 <- datos %>% 
    filter(elementos == "LLUVIA") %>%
    dplyr::select(cod_estacion,ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic) %>% 
    mutate_if(is.character,as.numeric) %>% 
    mutate(cod = factor(cod_estacion)) %>% 
    group_by(cod) %>% 
    mutate(lluvia = mean(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic))) %>% 
    dplyr::select(cod,lluvia) %>% 
    ungroup()

datos1 <- left_join(datos1,datos2, by="cod")

datos_sp <- st_as_sf(datos1,coords = c("lon","lat")) 
provincias_sp <- read_sf(dsn="Datos/provincias",layer = "provincias")

rm(datos,datos1,datos2)

#Primer ploteo
#pdf("graficos/Figura1.pdf")
tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluvia",alpha=0.9,col="steelblue",
               title.size = "Lluvia promedio(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))
#dev.off()

#pdf("graficos/Figura2.pdf")
tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "temp",alpha=0.9,col="orange",
               title.size = "Temperatura media(°C)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

#dev.off()
#Análisis no Geospacial
#Hacer regresiones IDW

#Análisis Geoespacial
plot(datos_sp$temp,datos_sp$alt)

