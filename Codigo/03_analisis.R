#Cargamos la librerías que se utilizarán

library(tidyverse)
library(gstat)
library(sf)
library(MASS)
library(dismo)

#cargamos los datos
datos <- readRDS("Datos/datos_finales.Rds")

#nombres de columnas en minúscula
colnames(datos) <- tolower(colnames(datos))

#Pasar los datos a numericos
#Media de temperatura
datos1 <- datos %>% 
    filter(elementos == "LLUVIA") %>%
    select(cod_estacion,lat,lon,alt,ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic) %>% 
    mutate_if(is.character,as.numeric) %>% 
    mutate(cod = factor(cod_estacion)) %>% 
    group_by(cod) %>% 
    mutate(lluviaprom = mean(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic))) %>% 
    select(cod,lat,lon,alt,lluviaprom)

rm(datos)


