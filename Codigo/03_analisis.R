#Cargamos la librerías que se utilizarán

library(gstat)
library(sf)
library(sp)
library(MASS)
library(dismo)
library(tmap)
library(rgdal)
library(tidyverse)

#cargamos los datos
datos <- readRDS("Datos/datos_finales.Rds")

#nombres de columnas en minúscula
colnames(datos) <- tolower(colnames(datos))

#Pasar los datos a numericos
#Media de temperatura
datos1 <- datos %>% 
    filter(elementos == "LLUVIA") %>%
    dplyr::select(cod_estacion,lat,lon,alt,ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic) %>% 
    mutate_if(is.character,as.numeric) %>% 
    mutate(cod = factor(cod_estacion)) %>% 
    group_by(cod) %>% 
    mutate(lluviaprom = mean(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic))) %>% 
    dplyr::select(cod,lat,lon,alt,lluviaprom)

datos_sp <- st_as_sf(datos1,coords = c("lon","lat")) 
provincias_sp <- read_sf(dsn="Datos/provincias",layer = "provincias")

rm(datos,datos1)



#Primer ploteo
tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluviaprom",alpha=0.9,col="steelblue",
               title.size = "Lluvia promedio(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

#Análisis no Geospacial
#Hacer regresiones IDW

#Análisis Geoespacial
plot(sort(datos_sp$lluviaprom))
plot(datos_sp$alt,datos_sp$lluviaprom)
plot(variogram(lluviaprom ~ 1, datos_sp))

#Dados estos resultados debemos hacer una transformación no lineal
#boxcox(lluviaprom~alt,data=as.data.frame(datos_sp))
#

