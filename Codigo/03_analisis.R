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

#Pasar los datos a numericos
datos1 <- datos %>% 
    filter(elementos == "LLUVIA") %>%
    dplyr::select(cod_estacion,alt,ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic,lon,lat) %>% 
    mutate_if(is.character,as.numeric) %>% 
    mutate(cod = factor(cod_estacion)) %>% 
    group_by(cod) %>% 
    mutate(lluvia_prom = mean(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic)),
           lluvia_med = median(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic)),
           lluvia_min = min(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic)),
           lluvia_max = max(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic)))%>% 
    dplyr::select(cod,alt,lluvia_prom,lluvia_med,lluvia_min,lluvia_max,lon,lat) %>% 
    ungroup()

datos_sp <- st_as_sf(datos1,coords = c("lon","lat")) 
provincias_sp <- read_sf(dsn="Datos/provincias",layer = "provincias")

rm(datos,datos1)
#Plots 
tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluvia_prom",alpha=0.9,col="steelblue",
               title.size = "Lluvia promedio(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluvia_med",alpha=0.9,col="steelblue",
               title.size = "Lluvia mediana(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluvia_min",alpha=0.9,col="steelblue",
               title.size = "Lluvia mínima(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluvia_max",alpha=0.9,col="steelblue",
               title.size = "Lluvia máxima(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

#Arreglar escala
#Transformaciones

data_sp <- datos_sp %>% 
    st_set_crs(32617) %>% 
    st_transform(crs=5367)

prov_sp <- provincias_sp %>% 
    st_set_crs(32617) %>% 
    st_transform(crs=st_crs(5367))

rm(datos_sp,provincias_sp)

#Análisis no Geospacial

#Hacer regresiones IDW

#Análisis Geoespacial
hscat(lluvia_prom~1,data_sp,seq(1,3.4,0.3))

v <- variogram(temp ~ 1, datos_sp)

